;;; org-msg.el --- Org mode to send and reply to email in HTML. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: January 2018
;; Keywords: extensions mail
;; Homepage: https://github.com/jeremy-compostella/org-msg
;; Package-Version: 4.0
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

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'gnus-art)
(require 'gnus-cite)
(require 'gnus-dired)
(require 'gnus-icalendar)
(require 'gnus-msg)
(require 'htmlize)
(require 'message)
(require 'mml)
(require 'org)
(require 'ox)
(require 'subr-x)
(require 'url-parse)
(require 'xml)

(defgroup org-msg nil
  "Org Message group."
  :group 'applications)

(defvar org-msg-attachment '()
  "Temporary variable to pass the list of attachment.")

(defvar org-msg-mml nil
  "Temporary variable to pass the MML content.")

(defvar org-msg-alternatives nil
  "Temporary alist to hold the contents of each alternative.")

(defvar org-msg-export-in-progress nil
  "Internal use only.
It is used by function advice.")

(defvar-local org-msg-mml-buffer-list '()
  "Used to store the `mml-buffer-list' variable content of the
current message. `mml-buffer-list' is the list of temporary
buffer holding mml contents.")

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
  `((text . ("text/plain" . ,(apply-partially 'org-msg-export-as-text 'ascii)))
    (utf-8 . ("text/plain" . ,(apply-partially 'org-msg-export-as-text 'utf-8)))
    (org . ("text/org" . identity))
    (html . ("text/html" . org-msg-export-as-html)))
  "Alist of the available alternative exporters.
Entries are in the form of `(tag . (mime-part . export-function))'.
The export function takes an `org-msg' message buffer string and
returns the exported content as a string."
  :type '(list (const symbol (cons string symbol))))

(defcustom org-msg-default-alternatives '((new . (html))
					  (reply-to-html . (html)))
  "Alternative MIME formats to send.
This customization variable orderly lists the alternatives of an
outgoing email. The possible keys are:
- `new' for new email is not a reply
- `reply-to-text' when the email being replied to is plain text
- `reply-to-html' when the email being replied to is html

When set to a simple list of alternatives and for backward
compatibility it applies to new emails and replies to html emails
but not to replies to plain text emails.

Available alternatives are listed in `org-msg-alternative-exporters'."
  :type '(choice (list symbol)
		 (list (alist symbol (list symbol)))))

(defcustom org-msg-greeting-fmt nil
  "Mail greeting format.
If it contains a '%s' format, '%s' is replaced with the first
name of the person you are replying to with a space prefix.

Example: \"Hi%s,\"
is replaced by either \"Hi Mark,\" or \"Hi,\"."
  :type '(string))

(defcustom org-msg-recipient-names '()
  "List of recipients preferred names.
The automatic replacement of '%s' format in
`org-msg-greeting-fmt' is not always ideal.  Some email addresses
do not include the actual recipient name or this recipient wants
to be called with another name, an acronym or its name has
accents. This variable can be used to specify these exceptions.

Example: ((\"jeremy.compostella@gmail.com\" . \"Jérémy\"))"
  :type '(list (cons string string)))

(defcustom org-msg-greeting-name-limit 1
  "Maximum number of recipient first name for the greeting format.
If replying to an email for which the 'To' field contains more
than one recipient and the `org-msg-greeting-fmt' contains a '%s'
format, this variable limits the number of recipient first name
used as a replacement of the '%s' format.  nil means unlimited."
  :type '(integer))

(defcustom org-msg-greeting-fmt-mailto nil
  "Define the format behavior for recipient greeting.
If t and `org-msg-greeting-fmt' contains a '%s' the recipient
name is formatted as a mailto link."
  :type '(boolean))

(defcustom org-msg-signature nil
  "Mail signature string appended if not nil.
The part in the signature block gets applied the \"signature\"
CSS style.

Example:
\"\n\nRegards,\n\n#+begin_signature\n-- *Your name*\n#+end_signature\""
  :type '(string))

(defcustom org-msg-posting-style 'top-posting
  "Define the posting style for HTML replies.
Can be either `top-posting' or nil."
  :type '(symbol))

(defcustom org-msg-undesirable-headers '("^attachments?$")
  "List of undesirable header to delete from the original email."
  :type '(list regexp))

(defcustom org-msg-dnd-protocol-alist
  '(("^file:" . org-msg-dnd-handle-file))
  "The functions to call when a file drop is made."
  :type '(repeat (cons (regexp) (function))))

(defcustom org-msg-attached-file-reference
  "attach[a-z]*\\|enclose"
  "Regular expression detecting a reference to an attached file."
  :type 'regexp)

(defun org-msg-lighten (color)
  "Make a color lighter by a 20%."
  (apply 'color-rgb-to-hex
	 (append
	  (apply 'color-hsl-to-rgb
		 (apply 'color-lighten-hsl
			(append
			 (apply 'color-rgb-to-hsl
				(color-name-to-rgb color))
			 (list 20))))
	  (list 2))))

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
		  inline-modes))
	 (base-quote '((padding-left . "5px") (margin-left . "10px")
		       (margin-top . "10px") (margin-bottom . "0")
		       (font-style . "italic") (background . "#f9f9f9")))
	 (quote-palette '("#324e72" "#6a3a4c" "#7a4900" "#ff34ff"
			  "#ff4a46" "#008941" "#006fa6" "#a30059"
			  "#ffdbe5" "#000000" "#0000a6" "#63ffac"))
	 (quotes
	  (mapcar (lambda (x)
		    (let ((c (nth x quote-palette)))
		      `(blockquote ,(intern (format "quote%d" (1+ x)))
				   (,@base-quote
				    (color . ,c)
				    (border-left . ,(concat "3px solid "
						      (org-msg-lighten c)))))))
		  (number-sequence 0 (1- (length quote-palette))))))
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
    (blockquote quote0 ,(append base-quote '((border-left . "3px solid #ccc"))))
    ,@quotes
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

(defcustom org-msg-convert-citation nil
  "Activate the conversion of mail citation into quote blocks.
If t, lines matching the '^>+ ' regular expression are turned
into multi-level quote blocks before being passed to the Org mode
HTML export engine."
  :type '(boolean))

(defcustom org-msg-supported-mua '((gnus-user-agent . "gnus")
				   (message-user-agent . "message")
				   (mu4e-user-agent . "mu4e")
				   (notmuch-user-agent . "notmuch"))
  "Supported Mail User Agents."
  :type '(alist :value-type string))

(defun org-msg-dnd-handle-file (uri _action)
  "Attach a file to the current draft.
URI is the file to handle, ACTION is one of copy, move, link or
ask."
  (when-let ((file (dnd-get-local-file-name uri t)))
    (org-msg-attach-attach file)))

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
  "Return t if mml has recursive html support.
Starting with Emacs 28, mml recursively searches for the
text/html part allowing multipart composition with HTML content
including images.

If this is not supported, OrgMsg places the text/html as a single
part and via an advice on the
`mml-expand-html-into-multipart-related` function, it modified
the mime data structure."
  (fboundp 'mml-expand-all-html-into-multipart-related))

(defun org-msg-save-article-for-reply-gnus (&optional parts header)
  "Export the currently visited `gnus-article-buffer' as HTML.
If parts is not nil, it exports in a file using the
`gnus-article-browse-html-parts' function otherwise, it uses the
`gnus-article-browse-html-article' function.  If the current
article contains other HTML emails as attachments, the
`browse-url-browser-function' is called several times.  We only
keep track of the first call which is usually the actual email we
want to reply to.  Both `gnus-article-browse-html-article' and
`gnus-article-browse-html-parts' also extract all the inline
images.  This function returns the absolute path of the HTML
file."
  (let* ((browse-url-browser-function #'ignore)
	 (save (cl-copy-list gnus-article-browse-html-temp-list)))
    (cl-letf (((symbol-function 'gnus-summary-show-article) #'ignore))
      (save-window-excursion
	(if parts
	    (gnus-article-browse-html-parts parts header)
	  (gnus-article-browse-html-article))))
    (prog1 (cl-set-difference gnus-article-browse-html-temp-list save
			      :test 'string=)
      (setq gnus-article-browse-html-temp-list save))))

(defun org-msg-save-article-for-reply-mu4e ()
  "Export the currently visited mu4e article as HTML."
  (let* ((msg mu4e-compose-parent-message)
	 (html (mu4e-message-field msg :body-html))
	 (file (make-temp-file "org-msg" nil ".html")))
    (cl-flet* ((mails2str (l)
		 (mapconcat (lambda (m)
			      (let ((name (or (car m) (cdr m))))
				(format "%S &lt;%s&gt;" name (cdr m))))
			    l ", "))
	       (field2str (f)
		 (let ((value (funcall (cdr f)
				       (mu4e-message-field msg (car f)))))
		   (when (and value (not (string-empty-p value)))
		     (format "%s: %s<br>\n"
			     (capitalize (substring (symbol-name (car f)) 1))
			     value))))
	       (get-charset (xml)
		 (when (eq 'meta (car xml))
		   (let ((attr (cadr xml)))
		     (cond ((string= (downcase (alist-get 'http-equiv attr ""))
				     "content-type")
			    (let ((c (alist-get 'content attr)))
			      (when (string-match "charset=\\([a-z0-9-]+\\)" c)
				(throw 'found (match-string 1 c)))))
			   ((alist-get 'charset attr)
			    (throw 'found (alist-get 'charset attr))))))))

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
	;; Save the HTML file with the appropriate coding system
	(when-let ((xml (libxml-parse-html-region (point-min) (point-max)))
		   (charset (catch 'found (org-msg-xml-walk xml #'get-charset))))
	  (let ((coding (intern (downcase charset))))
	    (when (coding-system-p coding)
	      (setq save-buffer-coding-system coding))))
	(write-file file))
      (list file))))

(defmacro org-msg-with-original-notmuch-message (&rest body)
  "Execute the forms in BODY with the replied notmuch message
buffer temporarily current."
  (declare (indent 0))
  (let ((id (make-symbol "id"))
	(buf (make-symbol "buf")))
    `(let ((,id (org-msg-message-fetch-field "in-reply-to")))
       (save-window-excursion
	 (let* ((notmuch-show-only-matching-messages t)
	       (,buf (notmuch-show (format "id:%s" (substring ,id 1 -1)))))
	   (notmuch-show-view-raw-message)
	   (prog1 (progn ,@body)
	     (kill-buffer ,buf)
	     (kill-buffer)))))))

(defun org-msg-save-article-for-reply-notmuch ()
  "Export the currently visited notmuch article as HTML."
  (let (header parts)
    (cl-flet ((get-field (field)
	       (when-let ((value (org-msg-message-fetch-field field)))
		 (concat (capitalize field) ": " value))))
      (org-msg-with-original-notmuch-message
	(let ((fields (mapcar #'get-field
			      '("from" "subject" "to" "cc" "date"))))
	  (setf header (mapconcat 'identity (delq nil fields) "\n")))
	(setf parts (mm-dissect-buffer))
	(unless (listp (car parts))
	  (setf parts (list parts))))
      (with-temp-buffer
	(let ((gnus-article-buffer (current-buffer))
	      (gnus-article-mime-handles parts))
	  (prog1 (org-msg-save-article-for-reply-gnus parts header)
	    (mm-destroy-parts parts)))))))

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
      (while (re-search-forward "\\([a-zA-Z0-9, -\\._]+\\) *{" nil t)
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
	      (cl-multiple-value-bind (tag class) (split-string sel "\\.")
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

(defsubst org-msg-in-quote-block ()
  "Whether point is in a quote block."
  (let ((face (get-char-property (point) 'face)))
    (if (listp face)
	(cl-find 'org-quote face)
      (eq 'org-quote face))))

(defun org-msg-ascii-blockquote (level begin end)
  "Recursively convert lines matching the `^ ?>+ ' regular
expression into multi-level quote blocks."
  (let ((suffix (format "quote%d\n" level)))
    (goto-char begin)
    (while (re-search-forward "^ ?>+ " end t)
      (if (and (= level 0) (org-msg-in-quote-block))
	  (org-msg-ascii-blockquote (1+ level) begin end)
	(unless (org-in-src-block-p)
	  (goto-char (line-beginning-position))
	  (let ((new-begin (point-marker)))
	    (insert "#+begin_" suffix)
	    (if (re-search-forward "^\\([^ >]\\| [^>]\\)" end t)
		(goto-char (line-beginning-position))
	      (goto-char (point-max))
	      (unless (= (point) (line-beginning-position))
		(insert "\n")))
	    (insert "#+end_" suffix)
	    (let ((new-end (point-marker)))
	      (goto-char new-begin)
	      (while (re-search-forward "^ ?>" new-end t)
		(replace-match "")
		(forward-char 1))
	      (org-msg-ascii-blockquote (1+ level) new-begin new-end))))))))

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
    (when-let ((sel (cl-remove-if-not #'css-match-p css))
	       (props (apply 'append (mapcar 'caddr sel))))
      (org-msg-props-to-style props))))

(defun org-msg-str-to-mailto (str css)
  "Convert a string of mail addresses into mailto anchor links.
Takes a string STR as a parameter and build a list of string and
mailto anchor link.  If a CSS style list is provided and a 'a
selectors on class `org-msg-reply-header-class', it sets the
style mailto anchor link style appropriately."
  (with-temp-buffer
    (insert str)
    (let ((name-regexp "\\([[:alpha:]\"][[:alnum:] ,\"()@./-]+\\)")
	  (mail-regexp "<\\([A-Za-z0-9@.-]+\\)>")
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
    ;; Add a bold property to the prefixes like "From", "Date", "Subject",
    ;; ... This section also deletes the undesirable header lines as
    ;; specified by `org-msg-undesirable-headers'.
  (let ((e (cdr div)))
    (while e
      (if (stringp (cadr e))
	  (let ((prefix (car (split-string (cadr e) ":"))))
	    (if (cl-find prefix org-msg-undesirable-headers
			 :test (lambda (x y) (string-match-p y (string-trim x))))
		(setcdr e (cdddr e))
	      (setcar (cdr e) (replace-regexp-in-string prefix "" (cadr e)))
	      (setcdr e (cons `(b nil ,(capitalize prefix)) (cdr e)))
	      (setf e (cddr e))))
	(setf e (cdr e)))))
    ;; Transform mail addresses into "mailto" links
    (org-msg-list-foreach (e (cdr div))
      (when (stringp (cadr e))
    	(when-let ((mailto (org-msg-str-to-mailto (cadr e) css)))
    	  (setf mailto (append mailto (cddr e)))
    	  (setcdr e mailto))))
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
    (cl-flet* ((get-html-root (xml)
		(catch 'found
		  (org-msg-xml-walk xml (lambda (x)
					  (when (eq (car x) 'html)
					    (throw 'found x))))))
	       (get-file-path (file)
		(let ((paths (cl-mapcar (lambda (d)
					  (expand-file-name file d))
					dirs)))
		  (car (cl-delete-if-not 'file-exists-p paths))))
	       (make-img-abs (xml)
		(when (eq (car xml) 'img)
		  (when-let ((src (assq 'src (cadr xml)))
			     (file (cdr src)))
		    (unless (or (url-type (url-generic-parse-url file))
				(file-name-absolute-p file))
		      (if-let ((path (get-file-path file)))
			  (setcdr src path)
			(unless (y-or-n-p (format "'%s' Image is missing,\
 do you want to continue ?" file))
			  (error "'%s' Image is missing" file)))))))
	       (inline-svg (xml)
		(when (and (eq (car xml) 'object)
			   (string= (cdr (assq 'type (cadr xml)))
				    "image/svg+xml"))
		  (let ((file (get-file-path (assoc-default 'data (cadr xml)))))
		    (when file
		      (let ((svg (with-temp-buffer
				   (insert-file-contents file)
				   (when (search-forward "<svg " nil t)
				     (libxml-parse-xml-region (match-beginning 0)
							      (point-max))))))
			(setcar xml (car svg))
			(setcdr xml (cdr svg))))))))
      (let ((xml (libxml-parse-html-region (point-min) (point-max))))
	(setf xml (get-html-root xml))
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

(defun org-msg--html-quote-block (quote-block contents info)
  (let ((cur (car (org-element-property :attr_html quote-block))))
    (unless (and cur (string-match-p ":class " cur))
      (let ((attr (concat ":class quote0" (when cur " ") cur)))
	(org-element-put-property quote-block :attr_html (list attr)))))
  (org-html-quote-block quote-block contents info))

(defun org-msg--html-special-block (special-block contents info)
  "Similar to `org-html-special-block' but treat specially the
blocks of type \"quote...\" generated by `org-msg-ascii-blockquote'."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ((string-match "quote[0-9]+" block-type)
      (let* ((contents (or contents ""))
	     (a (org-html--make-attribute-string `(:class ,block-type))))
	(format "<blockquote %s>\n%s\n</blockquote>" a contents)))
     (t (org-html-special-block special-block contents info)))))

(defun org-msg-org-to-xml (str &optional base)
  "Transform the STR Org string into a XML tree.
BASE is the path used to convert the IMG SRC relative paths to
absolute paths."
  (save-window-excursion
    (with-temp-buffer
      (insert str)
      (when org-msg-convert-citation
	(org-msg-ascii-blockquote 0 (point-min-marker) (point-max-marker)))
      (let ((org-html-table-default-attributes nil)
	    (org-html-htmlize-output-type 'inline-css)
	    (org-html-head-include-scripts nil)
	    (org-html-head-include-default-style nil)
	    (org-msg-export-in-progress t))
	(let ((buf (generate-new-buffer-name " *OrgMsg HTML Export*")))
	  (with-current-buffer (org-export-to-buffer 'org-msg-html buf)
	    (let ((xml (org-msg-html-buffer-to-xml base)))
	      (kill-buffer)
	      xml)))))))

(defun org-msg-export-as-text (charset str)
  "Transform the Org STR into a plain text."
  (with-temp-buffer
    (insert str)
    (cl-letf (((symbol-function #'fill-region) #'ignore))
      (let ((org-ascii-charset charset)
	    (org-ascii-inner-margin 0)
	    (files '()))
	(with-current-buffer (org-ascii-export-as-ascii)
	  (while (re-search-forward "<file:\\\([a-z0-9AZ_\./-]+\\\)>" nil t)
	    (setf files (push (match-string-no-properties 1) files)))
	  (cl-values (buffer-string) files))))))

(defun org-msg-export-as-html (str)
  "Transform the Org STR into html."
  (prog2
      (org-export-define-derived-backend 'org-msg-html 'html
	:translate-alist `((special-block . org-msg--html-special-block)
			   (quote-block . org-msg--html-quote-block)
			   ,@(org-export-get-all-transcoders 'html)))
      (org-msg-xml-to-str (org-msg-build str))
    (setq org-export-registered-backends
	  (cl-delete-if (apply-partially 'eq 'org-msg-html)
			org-export-registered-backends
			:key 'org-export-backend-name))))

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
		    (setcdr src (substring (cdr src) (length "file://"))))))
	      (set-charset (xml)
		(when (eq 'meta (car xml))
		  (let ((l (cadr xml)))
		    (cond ((string= (downcase (alist-get 'http-equiv l "?"))
				    "content-type")
			   (setf (alist-get 'content l)
				 (format "text/html;charset=%s"
					 org-html-coding-system)))
			  ((alist-get 'charset l)
			   (setf (alist-get 'charset l)
				 (symbol-name org-html-coding-system))))))))
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
	(when original
	  (org-msg-xml-walk original #'set-charset))
	(or original reply)))))

(defun org-msg-preview (arg)
  "Export and display the current OrgMsg buffer.
It uses the last alternative of the `alternatives' property as
the alternatives should be listed in increasing order of
preference.  If this alternative is `html' it calls the
`browse-url' function to display the exported mail in a web
browser.  With the prefix argument ARG set, it calls
`xwidget-webkit-browse-url' instead of `browse-url'.  For all
other alternatives, it displays the exported result in a buffer."
  (interactive "P")
  (let* ((preferred (last (org-msg-get-prop "alternatives")))
	 (alt (caar (org-msg-build-alternatives preferred t))))
    (cond ((string= (car alt) "text/html")
	   (save-window-excursion
	     (let ((browse-url-browser-function (if arg
						    'xwidget-webkit-browse-url
						  browse-url-browser-function))
		   (tmp-file (make-temp-file "org-msg" nil ".html")))
	       (with-temp-buffer
		 (insert (cdr alt))
		 (write-file tmp-file))
	       (browse-url (concat "file://" tmp-file)))))
	  (t (with-current-buffer (get-buffer-create
				   (format "*OrgMsg %s Preview*" (car alt)))
	       (delete-region (point-min) (point-max))
	       (insert (cdr alt)))
	     (display-buffer (current-buffer))))))

(defun org-msg-separate-mml-and-org (&optional preserve)
  "Separate the Org Mode and the MML content of the current buffer.
Returns the MML content and the Org Mode content as a list of two
strings. If PRESERVE is nil, the MML content is removed from the
buffer otherwise, the buffer is left untouched."
  (let ((buf (current-buffer))
	mml org)
    (when preserve
      (setf buf (generate-new-buffer " *temp*"))
      (copy-to-buffer buf (point-min) (org-msg-end)))
    (with-current-buffer buf
      (goto-char (point-min))
      (let (stack res)
	(while (re-search-forward "<#\\\(/?[a-z]+\\\)[ >]" nil t)
	  (unless (org-in-block-p '(""))
	    (let ((tag (match-string-no-properties 1)))
	      (unless (string= tag "secure")
		(if (string= (substring tag 0 1) "/")
		    (let ((cur (pop stack)))
		      (while (not (string= (substring tag 1) (car cur)))
			(setf cur (pop stack)))
		      (unless stack
			(push (list (cdr cur) (line-end-position)) res)))
		  (push (cons tag (line-beginning-position)) stack))))))
	(setf mml (mapconcat (lambda (x)
			       (apply 'delete-and-extract-region x))
			     res "\n"))
	(setf org (buffer-substring (org-msg-start) (org-msg-end))))
      (when preserve
	(kill-buffer)))
    (cl-values mml org)))

(defun org-msg-build-alternatives (alternatives &optional preserve)
  "Build the contents of the current Org-msg buffer for each of the ALTERNATIVES.
If PRESERVE is t, it does not alter the content of the
buffer (cf. `org-msg-separate-mml-and-org').

Returns a list of three items:
1. An association list of the exported alternatives
2. A list of attachments generated during the exportation if any
3. MML tags as a string if any"
  (let (mml org files)
    (cl-multiple-value-setq (mml org)
      (org-msg-separate-mml-and-org preserve))
    (cl-flet ((export (alt)
	       (let ((exporter (cdr (assq alt org-msg-alternative-exporters))))
		 (unless exporter
		   (error "%s is not a valid alternative, must be one of %s"
			  alt (mapcar #'car org-msg-alternative-exporters)))
		 (let ((exported (funcall (cdr exporter) org))
		       (exp-files '()))
		   (when (listp exported)
		     (cl-multiple-value-setq (exported exp-files) exported))
		   (setf files (append files exp-files))
		   (cons (car exporter) exported)))))
      (cl-values (mapcar #'export alternatives) files mml))))

(defun org-msg-prepare-to-send ()
  "Convert the current OrgMsg buffer into `mml' content.
This function is a hook for `message-send-hook'."
  (save-window-excursion
    (when (eq major-mode 'org-msg-edit-mode)
      (if (get-text-property (org-msg-start) 'mml)
          (message "Warning: org-msg: %S is already a MML buffer" (current-buffer))
        (let ((alternatives (org-msg-get-prop "alternatives"))
              attachments mml)
	  (cl-multiple-value-setq (org-msg-alternatives attachments mml)
	    (org-msg-build-alternatives alternatives))
	  (when (memq 'html alternatives)
	    (cl-flet ((is-image-but-svg (file)
		       (string-match-p "image/\\([^s]\\|s[^v]\\|sv[^g]\\)"
				       (org-msg-file-mime-type file))))
	      (setf attachments (cl-delete-if #'is-image-but-svg attachments))))
	  (setf attachments (cl-union (org-msg-get-prop "attachment")
				      attachments))
          ;; Verify all attachments exist
          (dolist (file attachments)
            (unless (file-exists-p file)
              (error "File '%s' does not exist" file)))
          ;; Clear the contents of the message
          (goto-char (org-msg-start))
          (delete-region (org-msg-start) (point-max))
          ;; If mml has recursive html support (starting with Emacs 28), we want
          ;; to generate the structure of the MIME document here.  If not we do
          ;; this by manually editing the structure of the parsed MML tree in
          ;; `org-msg-mml-into-multipart-related'. We also don't need to worry
          ;; about this if we are only sending text/plain
          (if (or (org-msg-mml-recursive-support)
                  (not (memq 'html alternatives)))
              (progn
                (when (or attachments mml)
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
                                  'filename file 'disposition "attachment"))
		(when mml
		  (insert mml)))
            (mml-insert-part "text/html")
            (insert (cdr (assoc "text/html" org-msg-alternatives)))
	    ;; Pass data to `org-msg-mml-into-multipart-related'
            (setq org-msg-attachment attachments
		  org-msg-mml mml))
          ;; Propertise the message contents so we don't accidentally run this
          ;; function on the buffer twice
          (add-text-properties (save-excursion (message-goto-body))
                               (point-max)
                               '(mml t)))))))

(defun org-msg-file-mime-type (file)
  "Return FILE mime type based on FILE extension.
If FILE does not have an extension, \"text/plain\" is returned."
  (if-let ((extension (file-name-extension file)))
      (mailcap-extension-to-mime extension)
    "text/plain"))

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
      (prog1
	  (if (or org-msg-attachment org-msg-mml)
	      `(multipart (type . "mixed")
			  ,@alternative
			  ,@newparts
			  ,@(when org-msg-mml
			      (with-temp-buffer
				(insert org-msg-mml)
				(mml-parse))))
	    alternative)
	(setq org-msg-mml nil
	      org-msg-attachment nil
	      org-msg-alternatives nil)))))

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
	(funcall orig-fun todo info)))))

(defun org-msg-message-fetch-field (field-name)
  "Return the value of the header field whose type is FIELD-NAME."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-fetch-field field-name))))

(defun org-msg-get-to-name ()
  "Return the name of the recipient.
It parses the 'To:' field of the current `org-msg-edit-mode'
buffer to extract and return a name.  The returned name is either
the recipient name specified in `org-msg-recipient-names' or the
first name automatically extracted from the email address. It is
used to automatically greet the right name, see
`org-msg-greeting-fmt'."
  (cl-flet ((recipient2name (r)
	     (cl-multiple-value-bind (name mail) r
	       (when name
		 (or (assoc-default mail org-msg-recipient-names)
		     (let* ((split (split-string name ", " t))
			    (first-name (if (= (length split) 2)
					    (cadr split)
					  (car (split-string name " " t)))))
		       (setf first-name (capitalize first-name))
		       (if org-msg-greeting-fmt-mailto
			   (format "[[mailto:%s][%s]]" mail first-name)
			 first-name)))))))
    (save-excursion
      (if-let ((to (org-msg-message-fetch-field "to")))
	  (let ((recipients (mail-extract-address-components to t)))
	    (when org-msg-greeting-name-limit
	      (setf recipients (seq-take recipients org-msg-greeting-name-limit)))
	    (string-join (delq nil (mapcar #'recipient2name recipients)) ", "))
	""))))

(defun org-msg-header (reply-to alternatives)
  "Build the Org OPTIONS and PROPERTIES blocks.
REPLY-TO is the file path of the original email export in HTML."
  (concat (format "#+OPTIONS: %s d:nil\n#+STARTUP: %s\n"
		  (or org-msg-options "") (or org-msg-startup ""))
	  (format ":PROPERTIES:\n:reply-to: %S\n:attachment: nil\n:alternatives: %s\n:END:\n"
		  reply-to alternatives)))

(defun org-msg-article-htmlp ()
  "Return t if the current article buffer contains a text/html part."
  (when-let ((parts (mm-dissect-buffer t t)))
    (mm-destroy-parts parts)
    (cl-find "text/html" (flatten-tree parts) :test 'equal)))

(defun org-msg-article-htmlp-gnus ()
  "Return t if the current gnus article is HTML article.
If the currently visited article (`gnus-article-buffer') contains
a html mime part, it returns t, nil otherwise."
  (with-current-buffer gnus-article-buffer
    (set-buffer gnus-original-article-buffer)
    (org-msg-article-htmlp)))

(defun org-msg-article-htmlp-mu4e ()
  "Return t if the current mu4e article is HTML article."
  (when (mu4e-message-field mu4e-compose-parent-message :body-html) t))

(defun org-msg-article-htmlp-notmuch ()
  "Return t if the current notmuch article is an HTML article."
  (org-msg-with-original-notmuch-message
    (org-msg-article-htmlp)))

(defun org-msg-has-mml-tags ()
  "Return t if the current buffer contains MML tags."
  (let ((mml (mml-parse)))
    (or (> (length mml) 1)
	(not (string= (alist-get 'type (car mml)) "text/plain")))))

(defun org-msg-get-alternatives (type)
  "Return the alternatives list for TYPE.
TYPE is a one of the keys of `org-msg-default-alternatives'.

This function can be advised if a more subtle behavior is needed
such as always use a particular alternatives list when replying
to a particular mail address."
  (cond ((listp (car org-msg-default-alternatives))
	 (alist-get type org-msg-default-alternatives))
	((eq type 'reply-to-text) nil)
	(org-msg-default-alternatives)))

(defun org-msg-composition-parameters (type alternatives)
  "Return the posting-style, greeting format and signature.
TYPE is a one of the keys of `org-msg-default-alternatives'.
ALTERNATIVES is a list of alternative symbols included as defined
in `org-msg-alternative-exporters'.

This function returns the value of the `org-msg-posting-style',
`org-msg-greeting-fmt' and `org-msg-posting-style' customization
variables as an association list with `style', `greeting-fmt' and
`signature' as their respective keys. The goal of this function
is to offer a anchor point for advanced configuration: it can be
advised to implement more complex behaviors such as change the
signature and posting style when replying to a particular mail
address or tweak the signature when replying with plain text
email."
  `((style . ,(when (and (eq type 'reply-to-html)
			 (memq 'html alternatives)
			 (not (= (point) (point-max)))
			 (not (org-msg-has-mml-tags)))
		org-msg-posting-style))
    (greeting-fmt . ,org-msg-greeting-fmt)
    (signature . ,org-msg-signature)))

(defun org-msg-post-setup (&rest _args)
  "Transform the current `message' buffer into a OrgMsg buffer.
If the current `message' buffer is a reply, the
`org-msg-separator' string is inserted at the end of the editing
area. If the current buffer contains MML tags,
`org-msg-edit-mode' is not activated as OrgMsg does not support
MML tags."
  (unless (eq major-mode 'org-msg-edit-mode)
    (message-goto-body)
    (let* ((type (cond ((not (org-msg-message-fetch-field "subject")) 'new)
		       ((org-msg-mua-call 'article-htmlp) 'reply-to-html)
		       ('reply-to-text)))
	   (alternatives (org-msg-get-alternatives type)))
      (when alternatives
	(let-alist (org-msg-composition-parameters type alternatives)
	  (insert (org-msg-header (when (eq .style 'top-posting)
				    (org-msg-mua-call 'save-article-for-reply))
				  alternatives))
	  (when .greeting-fmt
	    (insert (format .greeting-fmt
			    (if (eq type 'new)
				""
			      (concat " " (org-msg-get-to-name))))))
	  (when (eq .style 'top-posting)
	    (save-excursion
	      (insert "\n\n" org-msg-separator "\n")
	      (delete-region (line-beginning-position) (1+ (line-end-position)))
	      (dolist (rep '(("^>+ *" . "") ("___+" . "---")))
		(save-excursion
		  (while (re-search-forward (car rep) nil t)
		    (replace-match (cdr rep)))))
	      (org-escape-code-in-region (point) (point-max))))
	  (when .signature
	    (unless (eq .style 'top-posting)
	      (goto-char (point-max)))
	    (insert .signature))
	  (if (org-msg-message-fetch-field "to")
	      (org-msg-goto-body)
	    (message-goto-to))
	  (org-msg-edit-mode))
	(set-buffer-modified-p nil)))))

(defun org-msg-post-setup--if-not-reply (&rest args)
  "Helper for new mail setup vs reply in notmuch"
  (unless (org-msg-message-fetch-field "subject")
    (org-msg-post-setup args)))

(defalias 'org-msg-send-notmuch 'notmuch-mua-send)
(defalias 'org-msg-send-and-exit-notmuch 'notmuch-mua-send-and-exit)

(defun org-msg-sanity-check ()
  "Sanity check the mail body for any reference to a missing file
attachment. The detection relies on the regular expression
defined by the `org-msg-attached-file-reference' customization
variable."
  (or (org-msg-get-prop "attachment")
      (save-excursion
	(goto-char (org-msg-start))
	(while (re-search-forward org-property-re nil t)
	  (forward-line))
	(not (re-search-forward org-msg-attached-file-reference (org-msg-end) t)))
      (y-or-n-p "You may have forgotten to attach a file. Do you still want \
to proceed?")
      (error "Aborted")))

(defun org-msg-ctrl-c-ctrl-c ()
  "Send message like `message-send-and-exit'.
If the current buffer is OrgMsg buffer and OrgMsg is enabled (see
`org-msg-toggle'), it calls `message-send-and-exit'. With the
universal prefix argument, it calls `message-send'."
  (when (eq major-mode 'org-msg-edit-mode)
    (org-msg-sanity-check)
    (if current-prefix-arg
	(org-msg-mua-call 'send 'message-send)
      (org-msg-mua-call 'send-and-exit 'message-send-and-exit))))

(defun org-msg-tab ()
  "Complete names or Org mode visibility cycle.
If `point' is in the mail header region, the `message-tab'
function is called.  `org-cycle' is called otherwise."
  (interactive)
  (if (message-in-body-p)
      (org-cycle)
    (message-tab)))

(defun org-msg-attach-attach (file &rest _args)
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

(defun org-msg-dired-attach (orig-fun files-to-attach)
  "Attach dired's marked files to a OrgMsg message composition.
This function is used as an advice function of
`gnus-dired-attach'."
  (cl-flet* ((mail-buffer-p (b)
	      (with-current-buffer b
		(and (derived-mode-p 'org-msg-edit-mode)
		     (null message-sent-message-via))))
	     (mail-buffers ()
	      (when-let (bufs (cl-remove-if-not #'mail-buffer-p (buffer-list)))
		(mapcar 'buffer-name bufs))))
    (cl-letf (((symbol-function #'mml-attach-file) #'org-msg-attach-attach)
	      ((symbol-function #'gnus-dired-mail-buffers) #'mail-buffers))
      (funcall orig-fun files-to-attach))))

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
    (while (re-search-forward org-property-re nil t)
      (forward-line))))

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
	      ((delete-file file)))))
    (when org-msg-mml-buffer-list
      (let ((mml-buffer-list org-msg-mml-buffer-list))
	(mml-destroy-buffers)))))

(defun org-msg-store-mml-buffers ()
  "Locally store the list of MML temporary buffers."
  (when mml-buffer-list
    (setq org-msg-mml-buffer-list mml-buffer-list
	  mml-buffer-list nil)))

(defun org-msg-mode-message ()
  "Setup the advice for message mail user agent."
  (if org-msg-mode
      (advice-add 'message-mail :after #'org-msg-post-setup)
    (advice-remove 'message-mail #'org-msg-post-setup)))

(defun org-msg-inhibited (orig-fun &rest args)
  "Call ORIG-FUN with OrgMsg mode disabled."
  (let ((enable org-msg-mode))
    (when enable
      (org-msg-mode 0))
    (prog1
        (apply orig-fun args)
      (when enable
        (org-msg-mode 1)))))

(defun org-msg-mode-gnus ()
  "Setup the hook for gnus mail user agent."
  (if org-msg-mode
      (progn
	(add-hook 'gnus-message-setup-hook 'org-msg-post-setup)
	(add-hook 'gnus-message-setup-hook 'org-msg-store-mml-buffers)
	(advice-add 'gnus-icalendar-send-buffer-by-mail
		    :around #'org-msg-inhibited))
    (remove-hook 'gnus-message-setup-hook 'org-msg-post-setup)
    (remove-hook 'gnus-message-setup-hook 'org-msg-store-mml-buffers)
    (advice-remove 'gnus-icalendar-send-buffer-by-mail 'org-msg-inhibited)))

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
    (advice-remove 'notmuch-mua-reply 'org-msg-post-setup)
    (advice-remove 'notmuch-mua-mail 'org-msg-post-setup--if-not-reply)))

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
	(put 'message-exit-actions 'permanent-local t)
	(add-hook 'org-ctrl-c-ctrl-c-final-hook 'org-msg-ctrl-c-ctrl-c)
	(add-to-list 'message-syntax-checks '(invisible-text . disabled))
	(unless (org-msg-mml-recursive-support)
	  (advice-add 'mml-expand-html-into-multipart-related
		      :around #'org-msg-mml-into-multipart-related))
	(advice-add 'gnus-dired-attach :around #'org-msg-dired-attach)
	(advice-add 'org-html--todo :around #'org-msg-html--todo)
	(when (boundp 'bbdb-mua-mode-alist)
	  (add-to-list 'bbdb-mua-mode-alist '(message org-msg-edit-mode))))
    (put 'message-sent-hook 'permanent-local nil)
    (put 'message-exit-actions 'permanent-local nil)
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook 'org-msg-ctrl-c-ctrl-c)
    (setq message-syntax-checks (delete '(invisible-text . disabled)
					message-syntax-checks))
    (unless (org-msg-mml-recursive-support)
      (advice-remove 'mml-expand-html-into-multipart-related
		     #'org-msg-mml-into-multipart-related))
    (advice-remove 'gnus-dired-attach #'org-msg-dired-attach)
    (advice-remove 'org-html--todo #'org-msg-html--todo)
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
  (unless (mu4e-running-p)
    (mu4e~start))
  (when mu4e-compose-complete-addresses
    (mu4e~compose-setup-completion))
  ;; the following code is verbatim from mu4e-compose.el, `mu4e-compose-mode'
  ;; this will setup fcc (saving sent messages) and handle flags
  ;; (e.g. replied to)
  (add-hook 'message-send-hook
	    (if (functionp #'mu4e~setup-fcc-message-sent-hook-fn)
		#'mu4e~setup-fcc-message-sent-hook-fn
	      (lambda ()
		;; when in-reply-to was removed, remove references as well.
		(when (eq mu4e-compose-type 'reply)
		  (mu4e~remove-refs-maybe))
		(when use-hard-newlines
		  (mu4e-send-harden-newlines))
		;; for safety, always save the draft before sending
		(set-buffer-modified-p t)
		(save-buffer)
		(mu4e~compose-setup-fcc-maybe)
		(widen)))
	    nil t)
  ;; when the message has been sent.
  (add-hook 'message-sent-hook
	    (if (functionp #'mu4e~set-sent-handler-message-sent-hook-fn)
		#'mu4e~set-sent-handler-message-sent-hook-fn
	      (lambda ()
		(setq mu4e-sent-func 'mu4e-sent-handler)
		(mu4e~proc-sent (buffer-file-name))))
	    nil t))

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
  (setq-local message-sent-message-via nil)
  (add-hook 'message-send-hook 'org-msg-prepare-to-send nil t)
  (add-hook 'message-sent-hook 'undo t t)
  (add-hook 'completion-at-point-functions 'message-completion-function nil t)
  (cond ((message-mail-alias-type-p 'abbrev) (mail-abbrevs-setup))
	((message-mail-alias-type-p 'ecomplete) (ecomplete-setup)))
  (setq org-font-lock-keywords
	(append message-font-lock-keywords org-font-lock-keywords
		gnus-message-citation-keywords org-msg-font-lock-keywords))
  (toggle-truncate-lines)
  (org-msg-mua-call 'edit-mode)
  (setq-local kill-buffer-hook 'org-msg-kill-buffer
	      org-link-file-path-type 'absolute)
  (when (featurep 'dnd)
    (setq-local dnd-protocol-alist
                (append org-msg-dnd-protocol-alist dnd-protocol-alist)))
  (unless (= (org-msg-end) (point-max))
    (add-text-properties (1- (org-msg-end)) (point-max) '(read-only t))))

(provide 'org-msg)

;;; org-msg.el ends here
