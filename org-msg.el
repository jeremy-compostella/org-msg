;;; org-msg.el --- Org mode to send and reply to email in HTML. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: January 2018
;; Keywords: extensions mail
;; Homepage: https://github.com/jeremy-compostella/org-msg
;; Package-Version: 2.9
;; Package-Requires: ((emacs "24.4") (htmlize "1.54"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OrgMsg is a GNU/Emacs global minor mode mixing up Org mode and your
;; Mail User Agent Mode to compose and reply to emails in a HTML
;; friendly style.

;; * Presentation

;; By default, if the original message is in text form OrgMsg keeps it
;; that way and does not activate itself.  It allows to reply to
;; developer mailing list seamlessly.  If the original message is in
;; the HTML form, it activates the OrgMsg mode on the reply buffer.

;; OrgMsg provides a `org-msg-edit-mode' which is an derivation of Org
;; mode in which some functionality of your Mail User Agent are
;; imported or replicated.  For instance, a OrgMsg buffer uses the
;; same `font-lock-keywords' than Message mode or the `TAB' key while
;; the cursor is in the header calls the `message-tab' function.

;; For convenience, the original message is quoted below the
;; `--citation follows this line (read-only)--' marker.  So you can
;; easily refer to the original message.  However, the entire quoted
;; text is read-only because OrgMsg does not support modification of
;; the original content.

;; OrgMsg has a mechanism to support different Mail User Agents
;; (message, mu4e, notmuch ...).  Each function which depends on the Mail User
;; Agent calls the `org-msg-mua-call' which is an indirection to the
;; OrgMsg Mail User Agent specific function.

;; * Keys and interactive functions

;; The OrgMsg mode keys are the usual key combination used in either
;; Org mode or Message mode.
;; - C-c C-e - calls `org-msg-preview', it generates the final HTML
;;   email, save it into a temporary file and call the `browse-url'
;;   function on that file.
;; - C-c C-k - calls `message-kill-buffer'
;; - C-c C-s - calls `message-goto-subject' (same as in Message
;;   mode)
;; - C-c C-b - calls `org-msg-goto-body' (similar to
;;   `message-goto-body' in Message mode)
;; - C-c C-a - calls `org-msg-attach', very similar to the
;;   `org-attach' function.  It lets you add or delete attachment for
;;   this email.  Attachment list is stored in the `:attachment:'
;;   property.
;; - C-c C-c - calls `org-ctrl-c-ctrl-c'.  OrgMsg configures
;;   `org-msg-ctrl-c-ctrl-c' as a final hook of Org mode.  When
;;   `org-msg-ctrl-c-ctrl-c' is called in a OrgMsg buffer it
;;   generates the MIME message and send it.

;; The `org-msg-mode' interactive function can be called to
;; enable/disable OrgMsg.  By default, once the module is loaded, it
;; is disabled.  If you want to reply to an email without making use
;; of OrgMsg, you should call that function before you call the
;; reply-to function.

;; To start composing a new OrgMsg email, you can call the interactive
;; `message-mail' function.  If your `mail-user-agent' is
;; `message-user-agent' (which is the by default Emacs configuration),
;; `compose-mail' calls `message-mail' and is bound to [C-x m] by
;; default.

;; * Configuration

;; The following is my configuration as an Example

;; (require 'org-msg)
;; (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil")
;; (setq org-msg-startup "hidestars indent inlineimages")
;; (setq org-msg-greeting-fmt "\nHi %s,\n\n")
;; (setq org-msg-greeting-fmt-mailto t)
;; (setq org-msg-signature "
;;
;; Regards,
;;
;; #+begin_signature
;; -- *Jeremy* \\\\
;; /One Emacs to rule them all/
;; #+end_signature")
;; (org-msg-mode)

;; The `org-msg-greeting-fmt' can be customized to configure the
;; default greeting message.  If this format contains a `%s' token it
;; is automatically replaced with the first name of the person you are
;; replying to.  If `org-msg-greeting-fmt-mailto' is t, the first name
;; it is formatted as mailto link.

;; In order to avoid CSS conflict, OrgMsg performs inline replacement
;; when it generates the final HTML message.  See the
;; `org-msg-enforce-css' variable to customize the style (and the
;; default `org-msg-default-style' variable for reference).

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'gnus-art)
(require 'gnus-msg)
(require 'htmlize)
(require 'message)
(require 'mml)
(require 'org)
(require 'url-parse)
(require 'xml)

(defgroup org-msg nil
  "Org Message group."
  :group 'applications)

(defvar org-msg-attachment '()
  "Temporary variable to pass the list of attachment.")

(defvar org-msg-alternatives nil
  "Temporary alist to hold the contents of each alternative.")

(defvar org-msg-export-in-progress nil
  "Internal use only.
It is used by function advice.")

(defcustom org-msg-separator "--citation follows this line (read-only)--"
  "String separating the reply area and the original mail."
  :type '(string))

(defcustom org-msg-options "html-postamble:nil toc:nil author:nil email:nil"
  "Org Mode #+OPTIONS."
  :type '(string))

(defcustom org-msg-startup nil
  "Org Mode #+STARTUP."
  :type '(string))

(defcustom org-msg-alternative-exporters
  '((text . ("text/plain" . org-msg-export-as-text))
    (html . ("text/html" . org-msg-export-as-html)))
  "Alist of the available alternative exporters.
Entries are in the form of `(tag . (mime-part . export-function))'.
The export function takes an `org-msg' message buffer string and
returns the exported content as a string."
  :type '(list sexp))

(defcustom org-msg-default-alternatives '(html)
  "List of alternative MIME formats to send.
Available exporters are present in `org-msg-alternative-exporters'"
  :type '(list symbol))

(defcustom org-msg-greeting-fmt nil
  "Mail greeting format.
If it contains a '%s' format, '%s' is replaced with the first
name of the person you are replying to.

Example: \"\nHi %s,\n\n\""
  :type '(string))

(defcustom org-msg-greeting-name-limit 1
  "Maximum number of recipient first name for the greeting format.
If replying to an email for which the 'To' field contains more
than one recipient and the `org-msg-greeting-fmt' contains a '%s'
format, this variable limits the number of recipient first name
used as a replacement of the '%s' format.  nil means unlimited."
  :type '(integer))

(defcustom org-msg-greeting-fmt-mailto nil
  "Define the format behavior for recipient greeting.
If t and `org-msg-greeting-fmt' contains a '%s' the first name is
formatted as a mailto link."
  :type '(boolean))

(defcustom org-msg-signature nil
  "Mail signature string appended if not nil.
The part in the signature block gets applied the \"signature\"
CSS style.

Example:
\"\n\nRegards,\n\n#+begin_signature\n-- *Your name*\n#+end_signature\""
  :type '(string))

(defconst org-msg-default-style
  (let* ((font-family '(font-family . "\"Arial\""))
	 (font-size '(font-size . "10pt"))
	 (font `(,font-family ,font-size))
	 (line-height '(line-height . "10pt"))
	 (bold '(font-weight . "bold"))
	 (theme-color "#0071c5")
	 (color `(color . ,theme-color))
	 (table `(,@font (margin-top . "0px")))
	 (ftl-number `(,@font ,color ,bold (text-align . "left")))
	 (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
			     fundamental ini json makefile man org plantuml
			     python sh xml))
	 (inline-src `((color . ,(face-foreground 'default))
		       (background-color . ,(face-background 'default))))
	 (code-src
	  (mapcar (lambda (mode)
		    `(code ,(intern (concat "src src-" (symbol-name mode)))
			   ,inline-src))
		  inline-modes)))
  `((del nil (,@font (color . "grey") (border-left . "none")
	      (text-decoration . "line-through") (margin-bottom . "0px")
	      (margin-top . "10px") (line-height . "11pt")))
    (a nil (,color))
    (a reply-header ((color . "black") (text-decoration . "none")))
    (div reply-header ((padding . "3.0pt 0in 0in 0in")
		       (border-top . "solid #e1e1e1 1.0pt")
		       (margin-bottom . "20px")))
    (span underline ((text-decoration . "underline")))
    (li nil (,@font ,line-height (margin-bottom . "0px")
	     (margin-top . "2px")))
    (nil org-ul ((list-style-type . "square")))
    (nil org-ol (,@font ,line-height (margin-bottom . "0px")
		 (margin-top . "0px") (margin-left . "30px")
		 (padding-top . "0px") (padding-left . "5px")))
    (nil signature (,@font (margin-bottom . "20px")))
    (blockquote nil ((padding-left . "5px") (margin-left . "10px")
		     (margin-top . "20px") (margin-bottom . "0")
		     (border-left . "3px solid #ccc") (font-style . "italic")
		     (background . "#f9f9f9")))
    (code nil (,font-size (font-family . "monospace") (background . "#f9f9f9")))
    ,@code-src
    (nil linenr ((padding-right . "1em")
		 (color . "black")
		 (background-color . "#aaaaaa")))
    (pre nil ((line-height . "12pt")
	      ,@inline-src
	      (margin . "0px")
	      (font-size . "9pt")
	      (font-family . "monospace")))
    (div org-src-container ((margin-top . "10px")))
    (nil figure-number ,ftl-number)
    (nil table-number)
    (caption nil ((text-align . "left")
		  (background . ,theme-color)
		  (color . "white")
		  ,bold))
    (nil t-above ((caption-side . "top")))
    (nil t-bottom ((caption-side . "bottom")))
    (nil listing-number ,ftl-number)
    (nil figure ,ftl-number)
    (nil org-src-name ,ftl-number)

    (table nil (,@table ,line-height (border-collapse . "collapse")))
    (th nil ((border . "1px solid white")
	     (background-color . ,theme-color)
	     (color . "white")
	     (padding-left . "10px") (padding-right . "10px")))
    (td nil (,@table (padding-left . "10px") (padding-right . "10px")
		     (background-color . "#f9f9f9") (border . "1px solid white")))
    (td org-left ((text-align . "left")))
    (td org-right ((text-align . "right")))
    (td org-center ((text-align . "center")))

    (div outline-text-4 ((margin-left . "15px")))
    (div outline-4 ((margin-left . "10px")))
    (h4 nil ((margin-bottom . "0px") (font-size . "11pt")
	     ,font-family))
    (h3 nil ((margin-bottom . "0px") (text-decoration . "underline")
	     ,color (font-size . "12pt")
	     ,font-family))
    (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
	     (font-style . "italic") ,color (font-size . "13pt")
	     ,font-family))
    (h1 nil ((margin-top . "20px")
	     (margin-bottom . "0px") ,color (font-size . "12pt")
	     ,font-family))
    (p nil ((text-decoration . "none") (margin-bottom . "0px")
	    (margin-top . "10px") (line-height . "11pt") ,font-size
	    ,font-family))
    (div nil (,@font (line-height . "11pt"))))))

(defcustom org-msg-enforce-css org-msg-default-style
  "Define how to handle CSS style:
- list - style definition: see `org-msg-default-style' for
  example.
- string - path to a CSS file: same as t but use this file
  definitions."
  :type '(choice (file :must-match t)
		 (list (list symbol symbol
			     (alist :value-type string)))))

(defcustom org-msg-reply-header-class 'reply-header
  "Default CSS class for reply header tags."
  :type '(symbol))

(defcustom org-msg-supported-mua '((gnus-user-agent . "gnus")
				   (message-user-agent . "gnus")
				   (mu4e-user-agent . "mu4e")
				   (notmuch-user-agent . "notmuch"))
  "Supported Mail User Agents."
  :type '(alist :value-type string))

(defun org-msg-mua-call (sym &optional default &rest arg)
  "Call the specific MUA function for SYM with ARG parameters.
If no function is defined for this MUA, the DEFAULT function
is called."
  (let ((mua (assoc-default mail-user-agent org-msg-supported-mua)))
    (if mua
	(let ((fun (intern (format "org-msg-%s-%s" sym mua))))
	  (if (functionp fun)
	      (apply fun arg)
	    (when default
	      (apply default arg))))
      (error "Backend not found"))))

(defun org-msg-mml-recursive-support ()
  (fboundp 'mml-expand-all-html-into-multipart-related))

(defun org-msg-save-article-for-reply-gnus ()
  "Export the currently visited `gnus-article-buffer' as HTML.
It exports in a file using the `gnus-article-browse-html-article'
function.  If the current article contains other HTML emails as
attachments, the `browse-url-browser-function' is called several
times.  We only keep track of the first call which is usually the
actual email we want to reply to.  The
`gnus-article-browse-html-article' also extract all the inline
images.  This function returns the absolute path of the HTML
file."
  (let* ((browse-url-browser-function #'ignore)
	 (save (cl-copy-list gnus-article-browse-html-temp-list)))
    (cl-letf (((symbol-function 'gnus-summary-show-article) #'ignore))
      (save-window-excursion
	(gnus-article-browse-html-article)))
    (let ((temp-files (cl-set-difference gnus-article-browse-html-temp-list save
					 :test 'string=)))
      (setq gnus-article-browse-html-temp-list save)
      temp-files)))

(defun org-msg-save-article-for-reply-mu4e ()
  "Export the currently visited mu4e article as HTML."
  (let* ((msg mu4e-compose-parent-message)
	 (html (mu4e-message-field msg :body-html))
	 (file (concat "/tmp/" (mu4e-message-field msg :message-id))))
    (cl-flet* ((mails2str (l)
		 (mapconcat (lambda (m)
			      (format "%S &lt;%s&gt;" (car m) (cdr m)))
			    l ", "))
	       (field2str (f)
		 (let ((value (funcall (cdr f)
				       (mu4e-message-field msg (car f)))))
		   (when value
		     (format "%s: %s<br>\n"
			     (capitalize (substring (symbol-name (car f)) 1))
			     value)))))
      (with-temp-buffer
	(save-excursion
	  (insert html))
	;; Remove everything before html tag
	(save-excursion
	  (if (re-search-forward "^<html\\(.*?\\)>" nil t)
	      (delete-region (point-min) (match-beginning 0))
	    ;; Handle malformed HTML
	    (insert "<html><body>")
	    (goto-char (point-max))
	    (insert "</body></html>")))
	;; Insert reply header after body tag
	(when (re-search-forward "<body\\(.*?\\)>" nil t)
	  (goto-char (match-end 0))
	  (insert "<div align=\"left\">\n"
		  (mapconcat #'field2str
			     `((:from . ,#'mails2str)
			       (:subject . identity)
			       (:to . ,#'mails2str)
			       (:cc . ,#'mails2str)
			       (:date . message-make-date))
			     "")
		  "</div>\n<hr>\n"))
	(write-file file))
      (list file))))

(defun org-msg-attrs-str (attr)
  "Convert ATTR list of attributes into a string."
  (cl-flet ((attr-str (x)
	      (concat " " (symbol-name (car x)) "=\""
		      (xml-escape-string (cdr x)) "\"")))
    (if attr
	(apply 'concat (mapcar #'attr-str attr))
      "")))

(defun org-msg-xml-escape-string (string)
  "Convert STRING into a string containing valid XML character data.
This is a reduction of `xml-escape-string' to work-around a bug
during email generation where '&apos;' is turned into
'&amp;apos;'."
  (with-temp-buffer
    (insert string)
    (dolist (substitution '(("&" . "&amp;")
			    ("<" . "&lt;")
			    (">" . "&gt;")
			    ("\"" . "&quot;")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
	(replace-match (cdr substitution) t t nil)))
    (buffer-string)))

(defun org-msg-xml-to-str (xml)
  "Convert the XML tree into a HTML string."
  (cond ((and (listp xml) (equal xml '(p nil " ")))
	 "<o:p>&nbsp;</o:p>")
	((and (listp xml) (equal xml '(p nil)))
	 "<o:p>\n</o:p>")
	((stringp xml)
	 (replace-regexp-in-string " " "&nbsp;"
				   (org-msg-xml-escape-string xml)))
	((eq (car xml) 'comment)
	 (format "<!--%s-->" (caddr xml)))
	((eq (car xml) 'style)
	 (format "<style>%s</style>" (caddr xml)))
	((cddr xml)
	 (format "<%s%s>%s</%s>" (symbol-name (car xml))
		 (org-msg-attrs-str (cadr xml))
		 (apply 'concat (mapcar 'org-msg-xml-to-str (cddr xml)))
		 (symbol-name (car xml))))
	((format "<%s%s/>" (symbol-name (car xml))
		 (org-msg-attrs-str (cadr xml))))))

(defun org-msg-css-to-list ()
  "Convert the current buffer CSS content into a list.
\((tag class ((prop1 . val1) ...)) ...)."
  (let ((l))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\([a-zA-Z0-9, \-\\\._]+\\\) *{" nil t)
	(let ((selectors (split-string (match-string 1) "," nil " +"))
	      (start (point))
	      (props '()))
	  (backward-char 1)
	  (forward-sexp)
	  (let ((text-props (buffer-substring start (1- (point)))))
	    (dolist (p (split-string text-props ";" t "[\n ]*"))
	      (cl-multiple-value-bind (prop val) (split-string p ":" t "[\n ]*")
		(push (cons (intern prop) val) props)))
	    (dolist (sel selectors)
	      (cl-multiple-value-bind (tag class) (split-string sel "\\\.")
		(push (list (if (string= tag "") nil (intern tag))
			    (if (stringp class) (intern class) nil)
			    props)
		      l)))))))
    l))

(defun org-msg-css-file-to-list (file)
  "Convert FILE CSS content into a list representation.
See `org-msg-css-to-list'."
  (with-temp-buffer
    (insert-file-contents file)
    (org-msg-css-to-list)))

(defun org-msg-props-to-style (props)
  "Convert PROPS properties to a CSS style string."
  (cl-flet ((css-str (css)
	      (concat (symbol-name (car css)) ":"
		      (cdr css) ";")))
    (apply 'concat (mapcar #'css-str props))))

(defun org-msg-build-style (tag class css)
  "Given a TAG and CLASS selector, it builds a CSS style string.
This string can be used as a HTML style attribute value."
  (cl-flet ((css-match-p (css)
	      (or (and (eq tag (car css))
		       (eq class (cadr css)))
		  (and (not (car css))
		       (eq class (cadr css)))
		  (and (not (cadr css))
		       (eq tag (car css))))))
    (let* ((sel (cl-remove-if-not #'css-match-p css))
	   (props (apply 'append (mapcar 'caddr sel))))
      (when props
	(org-msg-props-to-style props)))))

;; TODO: Make use of `mail-extract-address-components' from 'mail-extr
;; package
(defun org-msg-str-to-mailto (str css)
  "Convert a string of mail addresses into mailto anchor links.
Takes a string STR as a parameter and build a list of string and
mailto anchor link.  If a CSS style list is provided and a 'a
selectors on class `org-msg-reply-header-class', it sets the
style mailto anchor link style appropriately."
  (with-temp-buffer
    (insert str)
    (let ((name-regexp "\\\([a-zA-Z\"][0-9a-zA-Z ,\"\(\)@\./\-]+\\\)")
	  (mail-regexp "<\\\([A-Za-z0-9@\.]+\\\)>")
	  (cursor (goto-char (point-min)))
	  (style (org-msg-build-style 'a org-msg-reply-header-class css))
	  (res))
      (while (re-search-forward (concat name-regexp " " mail-regexp) nil t)
	(unless (= (match-beginning 0) cursor)
	  (push (buffer-substring cursor (match-beginning 0))
		res)
	  (setq cursor (match-end 0)))
	(let ((anchor `(a ((href . ,(concat "mailto:" (match-string 0))))
			  ,(delete ?\" (match-string 1)))))
	  (when style
	    (push `(style . ,style) (cadr anchor)))
	  (push anchor res)))
      (nreverse res))))

(defmacro org-msg-list-foreach (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each cons from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  `(let ((,(car spec) ,(cadr spec)))
     (while ,(car spec)
       ,@body
       (let ((temp ,(car spec)))
	 (setq ,(car spec) (cdr temp))))))

(defun org-msg-improve-reply-header (xml css)
  "Aesthetically improve the reply header.
The reply header (From, Subject, Date, ...) generated by
`gnus-article-browse-html-article' does not look very nice.  XML
is the XML tree and CSS the style."
  (let ((div (assq 'div (assq 'body xml))))
    ;; Delete unnecessary line break
    (let ((e (cdr div)))
      (while e
	(if (and (stringp (car e))
		 (eq (cl-caadr e) 'br)
		 (and (stringp (caddr e))
		      (string-prefix-p "\n " (caddr e))))
	    (progn
	      (setcar e (replace-regexp-in-string "\n +" " "
						  (concat (car e) (cl-caddr e))))
	      (setcdr e (cl-cdddr e)))
	  (setf e (cdr e)))))
    ;; Add a bold property to the prefixes like "From", "Date",
    ;; "Subject", ...
    (org-msg-list-foreach (e (cdr div))
      (when (stringp (cadr e))
	(let ((prefix (car (split-string (cadr e) ":"))))
	  (setcar (cdr e) (replace-regexp-in-string prefix "" (cadr e)))
	  (setcdr e (cons `(b nil ,(capitalize prefix)) (cdr e)))
	  (setf e (cdr e)))))
    ;; Transform mail addresses into "mailto" links
    (org-msg-list-foreach (e (cdr div))
      (when (stringp (cadr e))
    	(let ((mailto (org-msg-str-to-mailto (cadr e) css)))
    	  (when mailto
    	    (setf mailto (append mailto (cddr e)))
    	    (setcdr e mailto)))))
    (when css
      (assq-delete-all 'hr (assq 'body xml))
      (assq-delete-all 'align (cadr div))
      (setf (cadr div) (assq-delete-all 'style (cadr div)))
      (let ((div-style (org-msg-build-style 'div
					    org-msg-reply-header-class css))
	    (p-style (org-msg-build-style 'p org-msg-reply-header-class css)))
	(when div-style
	  (push `(style . ,div-style) (cadr div)))
	(when p-style
	  (setf (cddr div) `((p ((style . ,p-style)) ,@(cddr div)))))))))

(defun org-msg-xml-walk (xml fun)
  "Recursively walk a XML tree and call FUN on each node."
  (when (listp xml)
    (funcall fun xml)
    (dolist (e (cddr xml))
      (org-msg-xml-walk e fun))))

(defun org-msg-html-buffer-to-xml (&optional base)
  "Return the XML tree of the current HTML buffer.
BASE is the path used to convert the IMG SRC relative paths to
absolute paths.  Base is also used to locate SVG objects tag file
and include the SVG content into the email XML tree."
  (let ((dirs (list base (temporary-file-directory))))
    (cl-flet* ((get-file-path (file)
		(let ((paths (cl-mapcar 'concat dirs
					(make-list (length dirs) file))))
		  (car (cl-delete-if-not 'file-exists-p paths))))
	       (make-img-abs (xml)
		(when (eq (car xml) 'img)
		  (let ((src (assq 'src (cadr xml))))
		    (unless (url-type (url-generic-parse-url (cdr src)))
		      (when src
			(unless (file-name-absolute-p (cdr src))
			  (let* ((file (cdr src))
				 (path (get-file-path file)))
			    (if path
				(setcdr src path)
			      (unless (y-or-n-p (format "'%s' Image is missing,\
 do you want to continue ?" file))
				(error "'%s' Image is missing" file))))))))))
	       (inline-svg (xml)
		(when (and (eq (car xml) 'object)
			   (string= (cdr (assq 'type (cadr xml)))
				    "image/svg+xml"))
		  (let ((file (get-file-path (assoc-default 'data (cadr xml)))))
		    (when file
		      (let ((svg (with-temp-buffer
				   (insert-file file)
				   (when (search-forward "<svg " nil t)
				     (libxml-parse-xml-region (match-beginning 0)
							      (point-max))))))
			(setcar xml (car svg))
			(setcdr xml (cdr svg))))))))
      (let ((xml (libxml-parse-html-region (point-min) (point-max))))
	(when base
	  (org-msg-xml-walk xml #'make-img-abs)
	  (org-msg-xml-walk xml #'inline-svg))
	(assq-delete-all 'title (assq 'head xml))
	xml))))

(defun org-msg-load-html-file (file)
  "Return the XML tree of a HTML FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-msg-html-buffer-to-xml (file-name-directory file))))

(defun org-msg-org-to-xml (str &optional base)
  "Transform the STR Org string into a XML tree.
BASE is the path used to convert the IMG SRC relative paths to
absolute paths."
  (save-window-excursion
    (with-temp-buffer
      (insert str)
      (let ((org-html-table-default-attributes nil)
	    (org-html-htmlize-output-type 'inline-css)
	    (org-html-head-include-scripts nil)
	    (org-html-head-include-default-style nil)
	    (org-msg-export-in-progress t))
	(with-current-buffer (org-html-export-as-html)
	  (let ((xml (org-msg-html-buffer-to-xml base)))
	    (kill-buffer)
	    xml))))))

(defun org-msg-export-as-text (str)
  "Transform the Org STR into a plain text."
  (with-temp-buffer
    (insert str)
    (with-current-buffer (org-ascii-export-as-ascii)
      (buffer-string))))

(defun org-msg-export-as-html (str)
  "Transform the Org STR into html."
  (org-msg-xml-to-str (org-msg-build str)))

(defun org-msg-load-css ()
  "Load the CSS definition according to `org-msg-enforce-css'."
  (cond ((listp org-msg-enforce-css) org-msg-enforce-css)
	((stringp org-msg-enforce-css)
	 (org-msg-css-file-to-list org-msg-enforce-css))))

(defmacro org-msg-with-match-prop (prop &rest body)
  "Look for the Org PROP property and call @BODY on match."
  (declare (indent 1))
  `(save-excursion
     (goto-char (point-min))
     (when (re-search-forward (org-re-property ,prop nil t) nil t)
       (progn ,@body))))

(defun org-msg-get-prop (prop)
  "Return the Org PROP property value, nil if undefined."
  (org-msg-with-match-prop prop
    (read (match-string-no-properties 3))))

(defun org-msg-set-prop (prop val)
  "Set the Org PROP property value to VAL."
  (org-msg-with-match-prop prop
    (replace-match (format "%S" val) nil nil nil 3)))

(defun org-msg-build (org)
  "Build and return the XML tree for ORG string."
  (let ((css (org-msg-load-css)))
    (cl-flet ((enforce (xml)
	       (let* ((tag (car xml))
		      (tmp (assq 'class (cadr xml)))
		      (class (when tmp
			       (intern (cdr tmp))))
		      (style (org-msg-build-style tag class css)))
		 (when style
		   (setf (cadr xml) (assq-delete-all 'style (cadr xml)))
		   (setf (cadr xml) (assq-delete-all 'class (cadr xml)))
		   (push `(style . ,style) (cadr xml)))))
	      (fix-img-src (xml)
			   (let ((src (assq 'src (cadr xml))))
			     (when (string-prefix-p "file://" (cdr src))
			       (setcdr src (substring (cdr src) (length "file://")))))))
      (let* ((reply (org-msg-org-to-xml org default-directory))
	     (temp-files (org-msg-get-prop "reply-to"))
	     (original (when temp-files
			 (org-msg-load-html-file (car temp-files)))))
	(assq-delete-all 'h1 (assq 'div (assq 'body reply)))
	(org-msg-xml-walk (assq 'body reply) #'fix-img-src)
	(when css
	  (assq-delete-all 'style (assq 'head reply))
	  (org-msg-xml-walk (assq 'body reply) #'enforce))
	(if (not original)
	    (assq-delete-all 'script (assq 'head reply))
	  (org-msg-improve-reply-header original css)
	  (push (or (assq 'article (assq 'body reply))
		    (assq 'div (assq 'body reply)))
		(cddr (assq 'body original))))
	(or original reply)))))

(defun org-msg-preview (arg)
  "Create a temporary mail and open it with `browse-url'.
With the prefix argument ARG set, it calls
`xwidget-webkit-browse-url' instead of `browse-url'."
  (interactive "P")
  (save-window-excursion
    (let ((browse-url-browser-function (if arg
					   'xwidget-webkit-browse-url
					 browse-url-browser-function))
	  (tmp-file (make-temp-file "org-msg" nil ".html"))
	  (mail (org-msg-build (buffer-substring-no-properties
                                (org-msg-start) (org-msg-end)))))
      (with-temp-buffer
	(insert (org-msg-xml-to-str mail))
	(write-file tmp-file))
      (browse-url (concat "file://" tmp-file)))))

(defun org-msg-build-alternatives (alternatives)
  "Build the contents of the current Org-msg buffer for each of the ALTERNATIVES."
  ;; Verify that all exporters are actually available
  (let ((available (mapcar #'car org-msg-alternative-exporters))
        (str (buffer-substring-no-properties (org-msg-start) (org-msg-end))))
    (dolist (alt alternatives)
      (unless (member alt available)
        (error "%s is not a valid alternative, must be one of %s"
               missing
               available)))
    ;; Build the contents of each alternative
    (mapcar (lambda (alt)
              (let ((exporter (cdr (assq alt org-msg-alternative-exporters))))
                (cons (car exporter)
                      (funcall (cdr exporter) str))))
            alternatives)))

(defun org-msg-prepare-to-send ()
  "Convert the current OrgMsg buffer into `mml' content.
This function is a hook for `message-send-hook'."
  (save-window-excursion
    (when (eq major-mode 'org-msg-edit-mode)
      (if (get-text-property (org-msg-start) 'mml)
          (message "Warning: org-msg: %S is already a MML buffer" (current-buffer))
        (let ((alternatives (org-msg-get-prop "alternatives"))
              (attachments (org-msg-get-prop "attachment")))
          ;; Verify all attachments exist
          (dolist (file attachments)
            (unless (file-exists-p file)
              (error "File '%s' does not exist" file)))
          ;; Set temporary global variable with attachment list
          (setq org-msg-attachment attachments)
          ;; Generate contents of alternatives and save globally
          (setq org-msg-alternatives
                (org-msg-build-alternatives alternatives))
          ;; Clear the contents of the message
          (goto-char (org-msg-start))
          (delete-region (org-msg-start) (point-max))
          ;; If mml has recursive html support (in later versions of emacs)
          ;; we want to generate the structure of the MIME document here.
          ;; If not we do this by manually editing the structure of the
          ;; parsed MML tree in `org-msg-mml-into-multipart-related'. We
          ;; also don't need to worry about this if we are only sending
          ;; text/plain
          (if (or (org-msg-mml-recursive-support)
                  (not (assq 'html org-msg-alternatives)))
              (progn
                (when attachments
                  (mml-insert-multipart "mixed"))
                (when (> (length org-msg-alternatives) 1)
                  (mml-insert-multipart "alternative"))
                (dolist (alt org-msg-alternatives)
                  (mml-insert-part (car alt))
                  (insert (cdr alt))
                  (forward-line))
                (when (> (length org-msg-alternatives) 1)
                  (forward-line))
                (dolist (file attachments)
                  (mml-insert-tag 'part 'type (org-msg-file-mime-type file)
                                  'filename file 'disposition "attachment")))
            (mml-insert-part "text/html")
            (insert (cdr (assoc "text/html" org-msg-alternatives))))
          ;; Propertise the message contents so we don't accidently run
          ;; this function on the buffer twice
          (add-text-properties (save-excursion (message-goto-body))
                               (point-max)
                               '(mml t)))))))

(defun org-msg-file-mime-type (file)
  "Return FILE mime type based on FILE extension.
If FILE does not have an extension, \"text/plain\" is returned."
  (let ((extension (file-name-extension file)))
    (if extension
	(mailcap-extension-to-mime extension)
      "text/plain")))

(defun org-msg-mml-into-multipart-related (orig-fun cont)
  "Extend the capability to handle file attachments.
This function is used as an advice function of
`mml-expand-html-into-multipart-related'.
- ORIG-FUN is the original function.
- CONT is the MIME representation of the mail content.
The implementation depends on the `org-msg-attachment' temporary
variable set by `org-msg-prepare-to-send'."
  (setq cont (funcall orig-fun cont))
  (let ((newparts '()))
    ;; Generate this list of attachment parts
    (dolist (file org-msg-attachment)
      (let ((type (org-msg-file-mime-type file)))
	(push (list 'part `(type . ,type) `(filename . ,file)
		    '(disposition . "attachment"))
	      newparts)))
    (let ((alternative (if (eq (car cont) 'multipart) (list cont) cont)))
      ;; Generate and insert any non-html alternatives
      (when (> (length org-msg-alternatives) 1)
        (dolist (alt org-msg-alternatives)
          (unless (equal (car alt) "text/html")
            (push `(part (type . ,(car alt))
		         (disposition . "inline")
		         (contents . ,(cdr alt)))
	          alternative)))
        ;; Put all the alternatives in a multipart
        (setf alternative `((multipart (type . "alternative")
                                       ,@alternative))))
      ;; Combine the attachments and the resulting content part/multipart
      (if org-msg-attachment
          `(multipart (type . "mixed")
                      ,@alternative
                      ,@newparts)
	alternative))))

(defun org-msg-html--todo (orig-fun todo &optional info)
  "Format todo keywords into HTML.
This function is used as an advice function of `org-html--todo'.
- ORIG-FUN is the original function.
- TODO is a TODO keyword.
- INFO is a property list."
  (cl-flet ((rgb-to-hex (r g b)
	     (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255))))
    (cl-macrolet ((add-if-exist (val lst sym)
		   `(when ,val
		      (push (cons ,sym (apply #'rgb-to-hex
					      (color-name-to-rgb ,val)))
			    ,lst))))
      (if org-msg-export-in-progress
	  (let ((face (org-get-todo-face todo)))
	    (when (and todo face)
	      (let (props)
		(add-if-exist (htmlize-face-foreground face) props 'color)
		(add-if-exist (htmlize-face-background face) props
			      'background-color)
		(format "<span%s>%s</span>"
			(if props
			    (format " style=\"%s\""
				    (org-msg-props-to-style props))
			  "")
			todo))))
	(if info
	    (funcall orig-fun todo info)
	  (funcall orig-fun todo))))))

(defun org-msg-message-fetch-field (field-name)
  "Return the value of the header field whose type is FIELD-NAME."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-fetch-field field-name))))

(defun org-msg-get-to-first-name ()
  "Return the first name of the recipient.
It parses the 'To:' field of the current `org-msg-edit-mode'
buffer to extract and return the first name.  It is used to
automatically greet the right name, see `org-msg-greeting-fmt'."
  (cl-flet ((recipient2name (r)
	     (cl-multiple-value-bind (name mail) r
		 (when name
		   (let* ((split (split-string name ", " t))
			  (first-name (if (= (length split) 2)
					  (cadr split)
					(car (split-string name " " t)))))
		     (setf first-name (capitalize first-name))
		     (if org-msg-greeting-fmt-mailto
			 (format "[[mailto:%s][%s]]" mail first-name)
		       first-name))))))
    (save-excursion
      (let ((to (org-msg-message-fetch-field "to")))
	(if to
	    (let ((recipients (mail-extract-address-components to t)))
	      (when org-msg-greeting-name-limit
		(setf recipients (seq-take recipients org-msg-greeting-name-limit)))
	      (mapconcat #'recipient2name recipients ", "))
	  "")))))

(defun org-msg-header (reply-to)
  "Build the Org OPTIONS and PROPERTIES blocks.
REPLY-TO is the file path of the original email export in HTML."
  (concat (format "#+OPTIONS: %s d:nil\n#+STARTUP: %s\n"
		  (or org-msg-options "") (or org-msg-startup ""))
	  (format ":PROPERTIES:\n:reply-to: %S\n:attachment: nil\n:alternatives: %s\n:END:\n"
		  reply-to
                  org-msg-default-alternatives)))

(defun org-msg-article-htmlp-gnus ()
  "Return t if the current gnus article is HTML article.
If the currently visited article (`gnus-article-buffer') contains
a html mime part, it returns t, nil otherwise."
  (let* ((parts (with-current-buffer gnus-article-buffer
		  (set-buffer gnus-original-article-buffer)
		  (mm-dissect-buffer t t)))
	 (str (format "%s" parts)))
    (string-match-p "text/html" str)))

(defun org-msg-article-htmlp-mu4e ()
  "Return t if the current mu4e article is HTML article."
  (when (mu4e-message-field mu4e-compose-parent-message :body-html) t))

(defun org-msg-article-htmlp-notmuch ()
  "Return t if the current notmuch reply is an HTML article."
  ;; Seems like never the case for notmuch but we want to use org-msg
  t)

(defun org-msg-post-setup (&rest _args)
  "Transform the current `message' buffer into a OrgMsg buffer.
If the current `message' buffer is a reply, the
`org-msg-separator' string is inserted at the end of the editing
area."
  (unless (eq major-mode 'org-msg-edit-mode)
    (message-goto-body)
    (let ((new (not (org-msg-message-fetch-field "subject")))
	  (reply-to))
      (when (or new (org-msg-mua-call 'article-htmlp))
	(unless new
	  (setq reply-to (org-msg-mua-call 'save-article-for-reply)))
	(insert (org-msg-header reply-to))
	(when org-msg-greeting-fmt
	  (insert (format org-msg-greeting-fmt
			  (if new
			      ""
			    (org-msg-get-to-first-name)))))
	(save-excursion
	  (unless new
	    (save-excursion
	      (insert "\n\n" org-msg-separator "\n")
	      (delete-region (line-beginning-position)
			     (1+ (line-end-position)))
	      (dolist (rep '(("^>+ *" . "") ("___+" . "---")))
		(save-excursion
		  (while (re-search-forward (car rep) nil t)
		    (replace-match (cdr rep)))))
	      (org-escape-code-in-region (point) (point-max))))
	  (when org-msg-signature
	    (insert org-msg-signature))
	  (org-msg-edit-mode))
	(set-buffer-modified-p nil))
      (if (org-msg-message-fetch-field "to")
	  (org-msg-goto-body)
	(message-goto-to)))))

(defun org-msg-post-setup--if-not-reply (&rest _args)
  "Helper for new mail setup vs reply in notmuch"
  (unless (org-msg-message-fetch-field "subject")
    (org-msg-post-setup _args)))

(defalias 'org-msg-send-and-exit-notmuch 'notmuch-mua-send-and-exit)

(defun org-msg-ctrl-c-ctrl-c ()
  "Send message like `message-send-and-exit'.
If the current buffer is OrgMsg buffer and OrgMsg is enabled (see
`org-msg-toggle'), it calls `message-send-and-exit'."
  (when (eq major-mode 'org-msg-edit-mode)
    (org-msg-mua-call 'send-and-exit 'message-send-and-exit)))

(defun org-msg-tab ()
  "Complete names or Org mode visibility cycle.
If `point' is in the mail header region, the `message-tab'
function is called.  `org-cycle' is called otherwise."
  (interactive)
  (if (message-in-body-p)
      (org-cycle)
    (message-tab)))

(defun org-msg-attach-attach (file)
  "Link FILE into the list of attachment."
  (interactive (list (read-file-name "File to attach: ")))
  (let ((files (org-msg-get-prop "attachment")))
    (org-msg-set-prop "attachment" (push file files))))

(defun org-msg-attach-delete ()
  "Delete a single attachment."
  (interactive)
  (let* ((files (org-msg-get-prop "attachment"))
	 (d (completing-read "File to remove: " files)))
    (org-msg-set-prop "attachment" (delete d files))))

(defun org-msg-attach ()
  "The dispatcher for attachment commands.
Shows a list of commands and prompts for another key to execute a
command."
  (interactive)
  (let (c)
    (save-excursion
      (save-window-excursion
	(with-output-to-temp-buffer "*Org Attach*"
	  (princ "Select an Attachment Command:

a       Select a file and attach it this mail.
d       Delete one attachment, you will be prompted for a file name."))
	(org-fit-window-to-buffer (get-buffer-window "*Org Attach*"))
	(message "Select command: [ad]")
	(setq c (read-char-exclusive))
	(and (get-buffer "*Org Attach*") (kill-buffer "*Org Attach*"))))
    (cond ((memq c '(?a ?\C-a)) (call-interactively 'org-msg-attach-attach))
	  ((memq c '(?d ?\C-d)) (call-interactively 'org-msg-attach-delete)))))

(defun org-msg-start ()
  "Return the point of the beginning of the message body."
  (save-excursion
    (message-goto-body)
    (search-forward "#+OPTIONS:" nil t)
    (line-beginning-position)))

(defun org-msg-end ()
  "Return the point of the end of the message body."
  (save-excursion
    (goto-char (point-min))
    (or (when (re-search-forward
	       (concat "^" (regexp-quote org-msg-separator) "$") nil t)
	  (match-beginning 0))
	(point-max))))

(defun org-msg-goto-body ()
  "Move point to the beginning of the message body."
  (interactive)
  (goto-char (point-min))
  (if org-msg-signature
      (when (search-forward org-msg-signature nil t)
	(goto-char (match-beginning 0)))
    (message-goto-body)))

(defun org-msg-font-lock-make-header-matcher (regexp)
  "Create a function which look for REGEXP."
  `(lambda (limit)
     (save-restriction
       (widen)
       (let ((start (point))
	     (citation-start (org-msg-end)))
	 (when (< start citation-start)
	   (goto-char citation-start))
	 (re-search-forward ,regexp (point-max) t)))))

(defun org-msg-kill-buffer ()
  "Delete temporary files."
  (let ((files (org-msg-get-prop "reply-to")))
    (dolist (file files)
      (when (and (not (string= "" file)) (file-exists-p file))
	(cond ((file-directory-p file) (delete-directory file t))
	      ((delete-file file)))))))

(defun org-msg-mode-gnus ()
  "Setup the hook for gnus mail user agent."
  (if org-msg-mode
      (add-hook 'gnus-message-setup-hook 'org-msg-post-setup)
    (remove-hook 'gnus-message-setup-hook 'org-msg-post-setup)))

(defun org-msg-mode-mu4e ()
  "Setup the hook for mu4e mail user agent."
  (if org-msg-mode
      (add-hook 'mu4e-compose-mode-hook 'org-msg-post-setup)
    (remove-hook 'mu4e-compose-mode-hook 'org-msg-post-setup)))

(defun org-msg-mode-notmuch ()
  "Setup the hook for notmuch mail user agent."
  (if org-msg-mode
      (progn
        (advice-add 'notmuch-mua-reply :after 'org-msg-post-setup)
        (advice-add 'notmuch-mua-mail :after 'org-msg-post-setup--if-not-reply))
    (progn
      (advice-remove 'notmuch-mua-reply 'org-msg-post-setup)
      (advice-remove 'notmuch-mua-mail 'org-msg-post-setup--if-not-reply))))

;;;###autoload
(define-minor-mode org-msg-mode
  "Toggle OrgMsg mode.
With a prefix argument ARG, enable Delete Selection mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When OrgMsg mode is enabled, the Message mode behavior is
modified to make use of Org Mode for mail composition and build
HTML emails."
  :global t
  (org-msg-mua-call 'mode)
  (if org-msg-mode
      (progn
	(put 'message-sent-hook 'permanent-local t)
	(add-hook 'org-ctrl-c-ctrl-c-final-hook 'org-msg-ctrl-c-ctrl-c)
	(add-to-list 'message-syntax-checks '(invisible-text . disabled))
	(unless (org-msg-mml-recursive-support)
	  (advice-add 'mml-expand-html-into-multipart-related
		      :around #'org-msg-mml-into-multipart-related))
	(advice-add 'org-html--todo :around #'org-msg-html--todo)
	(advice-add 'message-mail :after #'org-msg-post-setup)
	(when (boundp 'bbdb-mua-mode-alist)
	  (add-to-list 'bbdb-mua-mode-alist '(message org-msg-edit-mode))))
    (put 'message-sent-hook 'permanent-local nil)
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook 'org-msg-ctrl-c-ctrl-c)
    (setq message-syntax-checks (delete '(invisible-text . disabled)
					message-syntax-checks))
    (unless (org-msg-mml-recursive-support)
      (advice-remove 'mml-expand-html-into-multipart-related
		     #'org-msg-mml-into-multipart-related))
    (advice-remove 'org-html--todo #'org-msg-html--todo)
    (advice-remove 'message-mail #'org-msg-post-setup)
    (when (boundp 'bbdb-mua-mode-alist)
      (setq bbdb-mua-mode-alist (delete '(message org-msg-edit-mode)
					bbdb-mua-mode-alist)))))

(defvar org-msg-font-lock-keywords
  (let ((content "[ \t]*\\(.+\\(\n[ \t].*\\)*\\)\n?"))
    `((,(org-msg-font-lock-make-header-matcher
	 (concat "^\\([Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-to nil t))
      (,(org-msg-font-lock-make-header-matcher
	 (concat "^\\(^[GBF]?[Cc][Cc]:\\|^[Rr]eply-[Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-cc nil t))
      (,(org-msg-font-lock-make-header-matcher
	 (concat "^\\([Ss]ubject:\\)" content))
       (1 'message-header-name)
       (2 'message-header-subject nil t))
      (,(org-msg-font-lock-make-header-matcher
	 (concat "^\\([A-Z][^: \n\t]+:\\)" content))
       (1 'message-header-name)
       (2 'message-header-other nil t))
      ,@(if (and org-msg-separator
		 (not (equal org-msg-separator "")))
	    `((,(concat "^\\(" (regexp-quote org-msg-separator) "\\)$")
	       1 'message-separator))
	  nil)))
  "Additional expressions to highlight in OrgMsg mode.")

(defun org-msg-edit-mode-mu4e ()
  "Setup mu4e faces, addresses completion and run mu4e."
  (mu4e~compose-remap-faces)
  (mu4e~start)
  (when mu4e-compose-complete-addresses
    (mu4e~compose-setup-completion))
  ;; the following code is verbatim from mu4e-compse.el, mu4e-compose-mode
  ;; this will setup fcc (saving sent messages) and handle flags
  ;; (e.g. replied to)
  (add-hook 'message-send-hook
            (lambda () ;; mu4e~compose-save-before-sending
              ;; when in-reply-to was removed, remove references as well.
              (when (eq mu4e-compose-type 'reply)
                (mu4e~remove-refs-maybe))
              (when use-hard-newlines
                (mu4e-send-harden-newlines))
              ;; for safety, always save the draft before sending
              (set-buffer-modified-p t)
              (save-buffer)
              (mu4e~compose-setup-fcc-maybe)
              (widen)) nil t)
  ;; when the message has been sent.
  (add-hook 'message-sent-hook
            (lambda () ;;  mu4e~compose-mark-after-sending
              (setq mu4e-sent-func 'mu4e-sent-handler)
              (mu4e~proc-sent (buffer-file-name))) nil t))

(defalias 'org-msg-edit-kill-buffer-mu4e 'mu4e-message-kill-buffer)

(defun org-msg-edit-kill-buffer ()
  (interactive)
  (org-msg-mua-call 'edit-kill-buffer 'message-kill-buffer))

(defvar org-msg-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "<tab>") 'org-msg-tab)
    (define-key map [remap org-export-dispatch] 'org-msg-preview)
    (define-key map (kbd "C-c C-k") 'org-msg-edit-kill-buffer)
    (define-key map (kbd "C-c C-s") 'message-goto-subject)
    (define-key map (kbd "C-c C-b") 'org-msg-goto-body)
    (define-key map [remap org-attach] 'org-msg-attach)
    map)
  "Keymap for `org-msg-edit-mode'.")

(define-derived-mode org-msg-edit-mode org-mode "OrgMsg"
  "Major mode to compose email using Org mode.
Like Org Mode but with these additional/changed commands:
Type \\[org-ctrl-c-ctrl-c] to send the message if the cursor is
  not a C-c C-c Org mode controlled region (Org babel for
  example).
Type \\[org-msg-preview] to preview the final email with
  `browse-url'.
Type \\[message-kill-buffer] to kill the current OrgMsg buffer.
Type \\[message-goto-subject] to move the point to the Subject
  header.
Type \\[org-msg-goto-body] to move the point to the beginning of
  the message body.
Type \\[org-msg-attach] to call the dispatcher for attachment
  commands.

\\{org-msg-edit-mode-map}"
  (set (make-local-variable 'message-sent-message-via) nil)
  (add-hook 'message-send-hook 'org-msg-prepare-to-send nil t)
  (add-hook 'message-sent-hook 'undo t t)
  (add-hook 'completion-at-point-functions 'message-completion-function nil t)
  (setq org-font-lock-keywords
	(append org-font-lock-keywords message-font-lock-keywords
		org-msg-font-lock-keywords))
  (toggle-truncate-lines)
  (org-msg-mua-call 'edit-mode)
  (setq-local kill-buffer-hook 'org-msg-kill-buffer)
  (unless (= (org-msg-end) (point-max))
    (add-text-properties (1- (org-msg-end)) (point-max) '(read-only t))))

(provide 'org-msg)

;;; org-msg.el ends here
