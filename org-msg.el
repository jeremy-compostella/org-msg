;;; org-msg.el --- org mode to send and reply to email in HTML

;; Copyright (C) 2018 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: January 2018
;; Keywords: extensions mail
;; Homepage: https://github.com/jeremy-compostella/org-msg
;; Package-Version: 1.1
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

;; In a work environment it is necessary to be able to send email and
;; reply to email in HTML.  This module provides a `org-msg-mode'
;; which make use of `org-mode' for body HTML composition,
;; `message-mode' for overall mail composition and `mml-mode' to
;; handle inline images inclusion and attach files.
;;
;; - It uses `gnus-article-browse-html-article' to generate an HTML
;;   version of the article to reply to.  This HTML content is
;;   modified to add the reply generated using org-mode HTML export.
;; - It overrides `mml-expand-html-into-multipart-related' to add the
;;   support for file attachment.

;;; Code:

(require 'cl)
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
  "Temporary variable")

(defvar org-msg-enable t
  "")

(defvar org-msg-export-in-progress nil
  "Internal use only")

(defcustom org-msg-separator (purecopy "--citation follows this line (read-only)--")
  ""
  :type 'string
  :group 'org-msg)

(defcustom org-msg-options "html-postamble:nil toc:nil"
  "Org Mode #+OPTIONS."
  :group 'org-msg)

(defcustom org-msg-startup nil
  "Org Mode #+STARTUP."
  :group 'org-msg)

(defcustom org-msg-greeting-fmt "\nHi %s,\n\n"
  "Mail greeting format.  If it contains a %s format, %s is
replaced with the first name of the person you are replying to."
  :group 'org-msg)

(defcustom org-msg-signature "\n#+begin_signature\n\n#+end_signature"
  "Mail signature."
  :group 'org-msg)

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
	    ,font-family (max-width . "100ch")))
    (div nil (,@font (line-height . "11pt"))))))

(defcustom org-msg-enforce-css org-msg-default-style
  "Define how to handle CSS style:

- listp - style definition: see `org-msg-default-style' for
  example.

- string - path to a CSS file: same as t but use this file
  definitions."
  :group 'org-msg)

(defcustom org-msg-reply-header-class 'reply-header
  "Default CSS class for reply header tags.")

(defun org-msg-save-article-for-reply ()
  "Export the currently visited `gnus-article-buffer' as a HTML
file using the `gnus-article-browse-html-article' function. If
the current article contains other HTML emails as attachments,
the `browse-url-browser-function' is called several times.  We
only keep track of the first call which is usually the actual
email we want to reply to.  The
`gnus-article-browse-html-article' also extract all the inline
images.  This function returns the absolute path of the HTML
file."
  (let* ((pages '())
	 (save-page (lambda (url &optional args) (push url pages)))
	 (browse-url-browser-function save-page))
    (cl-letf (((symbol-function 'gnus-summary-show-article) #'ignore))
      (save-window-excursion
	(gnus-article-browse-html-article)))
    (substring (car (last pages)) (length "file://"))))

(defun org-msg-attrs-str (attr)
  "Convert an list of attributes into a string."
  (cl-flet ((attr-str (x)
	      (concat " " (symbol-name (car x)) "=\""
		      (xml-escape-string (cdr x)) "\"")))
    (if attr
	(apply 'concat (mapcar #'attr-str attr))
      "")))

(defun org-msg-xml-escape-string (string)
  "This is a reduction of `xml-escape-string' to work-around a
bug during email generation where '&apos;' is turned into
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
  "Convert the xml tree into a HTML string."
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
  "Convert the current buffer CSS content into a list
representation. ((tag class ((prop1 . val1) ...)) ...)."
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
  "Convert FILE CSS into content into a list representation. See
`org-msg-css-to-list'."
  (with-temp-buffer
    (insert-file-contents file)
    (org-msg-css-to-list)))

(defun org-msg-props-to-style (props)
  (cl-flet ((css-str (css)
	      (concat (symbol-name (car css)) ":"
		      (cdr css) ";")))
    (apply 'concat (mapcar #'css-str props))))

(defun org-msg-build-style (tag class css)
  "Given a TAG and CLASS selector, it builds a CSS style string
that can be used as a HTML style attribute value."
  (cl-flet ((css-match-p (css)
	      (or (and (eq tag (car css))
		       (eq class (cadr css)))
		  (and (not (car css))
		       (eq class (cadr css)))
		  (and (not (cadr css))
		       (eq tag (car css))))))
    (let* ((sel (remove-if-not #'css-match-p css))
	   (props (apply 'append (mapcar 'caddr sel))))
      (when props
	(org-msg-props-to-style props)))))

;; TODO: Make use of `mail-extract-address-components' from 'mail-extr
;; package
(defun org-msg-str-to-mailto (str css)
  "Takes a string STR as a parameter and build a list of string
and mailto anchor link.  If a css style list is provided and a 'a
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
  (declare (indent 1))
  `(let ((,(car spec) ,(cadr spec)))
     (while ,(car spec)
       ,@body
       (let ((temp ,(car spec)))
	 (setq ,(car spec) (cdr temp))))))

(defun org-msg-improve-reply-header (xml css)
  "The reply header (From, Subject, Date, ...) generated by
`gnus-article-browse-html-article' does not look very nice.  This
function does its best to improve it."
  (let ((div (assq 'div (assq 'body xml))))
    ;; Delete unnecessary line break
    (let ((e (cdr div)))
      (while e
	(if (and (stringp (car e))
		 (eq (caadr e) 'br)
		 (and (stringp (caddr e))
		      (string-prefix-p "\n " (caddr e))))
	    (progn
	      (setcar e (replace-regexp-in-string "\n +" " "
						  (concat (car e) (caddr e))))
	      (setcdr e (cdddr e)))
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
  "Walk a XML tree calling FUN on each node."
  (when (listp xml)
    (funcall fun xml)
    (dolist (e (cddr xml))
      (org-msg-xml-walk e fun))))

(defun org-msg-parse-html-buffer (&optional base)
  "Parse the current buffer as a HTML content and return a XML
tree out of it.  If BASE is specified, it makes all the IMG SRC
file references absolute based on that path."
  (cl-flet ((make-img-abs (xml)
	     (when (eq (car xml) 'img)
	       (let ((src (assq 'src (cadr xml))))
		 (unless (url-type (url-generic-parse-url (cdr src)))
		   (unless (file-name-absolute-p (cdr src))
		     (let ((file (concat base (cdr src))))
		       (if (file-exists-p file)
			   (setcdr src (concat base (cdr src)))
			 (error "'%s' Image is missing" file)))))))))
    (let ((xml (libxml-parse-html-region (point-min) (point-max))))
      (when base
	(org-msg-xml-walk xml #'make-img-abs))
      (assq-delete-all 'title (assq 'head xml))
      xml)))

(defun org-msg-load-html-file (file)
  "Load the html FILE and returns a XML tree. See
`org-msg-parse-html-buffer'."
  (with-temp-buffer
    (insert-file-contents file)
    (org-msg-parse-html-buffer (file-name-directory file))))

(defun org-msg-org-to-xml (str &optional base)
  "Transform the str OrgMode content into a html XML tree. Base
is specified is the path to use to update the IMG SRC relative
paths. See `org-msg-parse-html-buffer'."
  (save-window-excursion
    (with-temp-buffer
      (insert str)
      (let ((org-html-table-default-attributes nil)
	    (org-html-htmlize-output-type 'inline-css)
	    (org-html-head-include-scripts nil)
	    (org-html-head-include-default-style nil)
	    (org-msg-export-in-progress t))
	(org-html-export-as-html))
      (let ((xml (org-msg-parse-html-buffer base)))
	(kill-buffer)
	xml))))

(defun org-msg-load-css ()
  (cond ((listp org-msg-enforce-css) org-msg-enforce-css)
	((stringp org-msg-enforce-css)
	 (org-msg-css-file-to-list org-msg-enforce-css))))

(defmacro org-msg-search-prop (prop &rest body)
  (declare (indent 1))
  `(save-excursion
     (goto-char (point-min))
     (when (re-search-forward (org-re-property ,prop nil t) nil t)
       (progn ,@body))))

(defun org-msg-get-prop (prop)
  (org-msg-search-prop prop
    (match-string-no-properties 3)))

(defun org-msg-set-prop (prop val)
  (org-msg-search-prop prop
    (replace-match val nil nil nil 3)))

(defun org-msg-get-attachment ()
  (let ((str (org-msg-get-prop "attachment")))
    (when str
      (read (concat "(" str ")")))))

(defun org-msg-build ()
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
			   (let ((tag (car xml))
				 (src (assq 'src (cadr xml))))
			     (when (string-prefix-p "file://" (cdr src))
			       (setcdr src (substring (cdr src) (length "file://")))))))
      (let* ((org (buffer-substring-no-properties (org-msg-start) (org-msg-end)))
	     (reply (org-msg-org-to-xml org default-directory))
	     (original-file (org-msg-get-prop "reply-to"))
	     (original (unless (string= "" original-file)
			 (org-msg-load-html-file original-file))))
	(assq-delete-all 'h1 (assq 'div (assq 'body reply)))
	(org-msg-xml-walk (assq 'body reply) #'fix-img-src)
	(when css
	  (assq-delete-all 'style (assq 'head reply))
	  (org-msg-xml-walk (assq 'body reply) #'enforce))
	(if (not original)
	    (assq-delete-all 'script (assq 'head reply))
	  (org-msg-improve-reply-header original css)
	  (push (assq 'div (assq 'body reply)) (cddr (assq 'body original))))
	(or original reply)))))

(defun org-msg-preview (arg)
  (interactive "P")
  (save-window-excursion
    (let ((browse-url-browser-function (if arg
					   'xwidget-webkit-browse-url
					 browse-url-browser-function))
	  (tmp-file (make-temp-file "org-msg" nil ".html"))
	  (mail (org-msg-build)))
      (with-temp-buffer
	(insert (org-msg-xml-to-str mail))
	(write-file tmp-file))
      (browse-url (concat "file://" tmp-file)))))

(defun org-msg-prepare-to-send ()
  (when org-msg-enable
    (save-window-excursion
      (when (eq major-mode 'org-msg-mode)
	(let ((mail (org-msg-build))
	      (attachments (org-msg-get-attachment)))
	  (dolist (file attachments)
	    (unless (file-exists-p file)
	      (error "File '%s' does not exist" file)))
	  (setq org-msg-attachment attachments)
	  (message-goto-body)
	  (delete-region (point) (point-max))
	  (mml-insert-part "text/html")
	  (insert (org-msg-xml-to-str mail)))))))

(defun org-msg-mml-into-multipart-related (orig-fun cont)
  (setq cont (funcall orig-fun cont))
  (let ((newparts '()))
    (dolist (file org-msg-attachment)
      (let ((type (mailcap-extension-to-mime (file-name-extension file))))
	(push (list 'part `(type . ,type) `(filename . ,file)
		    '(disposition . "attachment"))
	      newparts)))
      (nconc (list 'multipart (cons 'type "mixed"))
      	     (if (eq (car cont) 'multipart) (list cont) cont)
      	     newparts)))

(advice-add 'mml-expand-html-into-multipart-related
	    :around #'org-msg-mml-into-multipart-related)

(defun org-msg-html--todo (orig-fun todo &optional info)
   (cl-macrolet ((add-if-exist (val lst sym)
  	         `(when ,val
		    (push (cons ,sym (apply 'color-rgb-to-hex
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
			  (format " style=\"%s\"" (org-msg-props-to-style props))
			"")
		      todo))))
      (if info
	  (funcall orig-fun todo info)
	(funcall orig-fun todo)))))

(advice-add 'org-html--todo :around #'org-msg-html--todo)

(defun org-msg-get-to-first-name ()
  "Parse the 'To:' field of the current `org-msg-mode' buffer to
extract and return the first name.  It is used to automatically
greet the right name, see `org-msg-greeting-fmt'."
  (save-excursion
    (message-goto-to)
    (let* ((to (buffer-substring-no-properties
		(+ (length "To: ") (line-beginning-position))
		(line-end-position)))
	   (split (split-string to "[<,]" t "[ \">]+")))
      (if (or (not split) (= (length split) 1))
	  ""
	(format "[[mailto:%s][%s]]" to
		(if (= (length split) 2)
		    (car (split-string (car split) " "))
		  (cadr split)))))))

(defun org-msg-header (reply-to)
  (concat (format "#+OPTIONS: %s d:nil\n#+STARTUP: %s\n"
		  (or org-msg-options "") (or org-msg-startup ""))
	  (format ":PROPERTIES:\n:reply-to: %s\n:attachment: \n:END:\n"
		  (or reply-to ""))))

(defun org-msg-article-htmlp ()
  "Returns t if the currently visited
article (`gnus-article-buffer') contains a html mime part, nil
otherwise."
  (let* ((parts (with-current-buffer gnus-article-buffer
		  (set-buffer gnus-original-article-buffer)
		  (mm-dissect-buffer t t)))
	 (str (format "%s" parts)))
    (string-match-p "text/html" str)))

(defun org-msg-post-setup ()
  (when org-msg-enable
    (message-goto-body)
    (let ((new (= (point) (point-max)))
	  (reply-to))
      (when (or new (org-msg-article-htmlp))
	(unless new
	  (setq reply-to (org-msg-save-article-for-reply)))
	(insert (org-msg-header reply-to))
	(insert (format org-msg-greeting-fmt (org-msg-get-to-first-name)))
	(save-excursion
	  (unless new
	    (save-excursion
	      (insert "\n\n" org-msg-separator "\n")
	      (delete-region (line-beginning-position)
			     (1+ (line-end-position)))
	      (while (re-search-forward "^>+ *" nil t)
		(replace-match ""))))
	  (when org-msg-signature
	    (insert org-msg-signature))
	  (org-msg-mode))))))

(defun org-msg-ctrl-c-ctrl-c ()
  (when (and (eq major-mode 'org-msg-mode) org-msg-enable)
    (message-send-and-exit)))

(defun org-msg-tab ()
  (interactive)
  (if (message-in-body-p)
      (org-cycle)
    (message-tab)))

(defsubst org-msg-set-attachment-prop (files)
  (org-msg-set-prop "attachment"
		    (mapconcat (lambda (file) (format "%S" file))
			       files " ")))

(defun org-msg-attach-attach (file)
  (interactive (list (ido-read-file-name "File to attach: ")))
  (let ((files (org-msg-get-attachment)))
    (org-msg-set-attachment-prop (push file files))))

(defun org-msg-attach-delete ()
  (interactive)
  (let* ((files (org-msg-get-attachment))
	 (d (ido-completing-read "File to remove: " files)))
    (org-msg-set-attachment-prop (delete d files))))

(defun org-msg-attach ()
  (interactive)
  (let (c)
    (save-excursion
      (save-window-excursion
	(with-output-to-temp-buffer "*Org Attach*"
	  (princ "Select an Attachment Command:

a       Select a file and attach it this mail.
d       Delete one attachment, you will be prompted for a file name.")))
	(org-fit-window-to-buffer (get-buffer-window "*Org Attach*"))
	(message "Select command: [ad]")
	(setq c (read-char-exclusive))
	(and (get-buffer "*Org Attach*") (kill-buffer "*Org Attach*")))
    (cond ((memq c '(?a ?\C-a)) (call-interactively 'org-msg-attach-attach))
	  ((memq c '(?d ?\C-d)) (call-interactively 'org-msg-attach-delete)))))

(defun org-msg-start ()
  (save-excursion
    (message-goto-body)
    (point)))

(defun org-msg-end ()
  (save-excursion
    (goto-char (point-min))
    (or (when (re-search-forward
	       (concat "^" (regexp-quote org-msg-separator) "$") nil t)
	  (match-beginning 0))
	(point-max))))

(defun org-msg-goto-body ()
  (interactive)
  (goto-char (point-min))
  (if org-msg-signature
      (when (search-forward org-msg-signature nil t)
	(goto-char (match-beginning 0)))
    (message-goto-body)))

(defun org-msg-new ()
  (interactive)
  (message-mail)
  (org-msg-post-setup))

(defun org-msg-toggle ()
  (interactive)
  (setq org-msg-enable (not org-msg-enable))
  (message (if org-msg-enable
	       (propertize "OrgMsg enabled." 'face 'success)
	     (propertize "OrgMsg disabled." 'face 'error))))

(defun org-msg-font-lock-make-header-matcher (regexp)
  `(lambda (limit)
     (save-restriction
       (widen)
       (let ((start (point))
	     (citation-start (org-msg-end)))
	 (when (< start citation-start)
	   (goto-char citation-start))
	 (re-search-forward ,regexp (point-max) t)))))

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

(define-derived-mode org-msg-mode org-mode "OrgMsg"
  (local-set-key (kbd "<tab>") 'org-msg-tab)
  (local-set-key (kbd "C-c C-e") 'org-msg-preview)
  (local-set-key (kbd "C-c C-k") 'message-kill-buffer)
  (local-set-key (kbd "C-c C-s") 'message-goto-subject)
  (local-set-key (kbd "C-c C-b") 'org-msg-goto-body)
  (local-set-key (kbd "C-c C-a") 'org-msg-attach)
  (set (make-local-variable 'message-sent-message-via) nil)
  (add-hook 'completion-at-point-functions 'message-completion-function nil t)
  (setq org-font-lock-keywords
	(append org-font-lock-keywords message-font-lock-keywords
		org-msg-font-lock-keywords))
  (toggle-truncate-lines)
  (unless (= (org-msg-end) (point-max))
    (add-text-properties (1- (org-msg-end)) (point-max) '(read-only t))))

(add-hook 'gnus-message-setup-hook 'org-msg-post-setup)
(add-hook 'message-send-hook 'org-msg-prepare-to-send)
(add-hook 'org-ctrl-c-ctrl-c-final-hook 'org-msg-ctrl-c-ctrl-c)

(add-to-list 'message-syntax-checks '(invisible-text . disabled))

(provide 'org-msg)

;;; org-msg.el ends here
