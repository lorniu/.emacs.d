;;; org-special-block-extras.el --- hack the special blocks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)          ;; Extra Lisp functions; e.g., ‘when-let’.
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; Function library; ‘-const’, ‘-compose’, ‘-orfn’, ‘-not’, ‘-partial’, etc.

(require 'org)
(require 'ox-latex)
(require 'ox-html)


;;; core

(defvar org-special-block-extras-blocks nil)

(defvar org-special-block-extras-links nil)

(defvar org-special-block-extras-html-heads nil)

;;

(defun org-special-block-extras--advice (backend special_block contents info)
  "Invoke the appropriate custom block handler, if any."
  (let* ((type    (nth 1 (nth 1 special_block)))
         (handler (intern (format "org-special-block-extras--%s" type))))
    (ignore-errors
      (funcall handler backend special_block (or contents "") info))))

(defun org-element-special-block-parser--advice (limit affiliated)
  "Parse a special block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `special-block' and CDR is a plist
containing `:type', `:begin', `:end', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let* ((case-fold-search t)
         (type (progn (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)")
                      (match-string-no-properties 1))))
    (if (not (save-excursion
               (re-search-forward
                (format "^[ \t]*#\\+END_%s[ \t]*$" (regexp-quote type))
                limit t)))
        ;; Incomplete block: parse it as a paragraph.
        (org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
        (save-excursion
          (let* ((begin (car affiliated))
                 (post-affiliated (point))
                 (title
                  (progn
                    (looking-at (format "^[ \t]*#\\+BEGIN_%s\\([^:\n]*\\)\\(.*\\)$" (regexp-quote type)))
                    (match-string-no-properties 1)))
                 (params
                  (match-string-no-properties 2))
                 ;; Empty blocks have no contents.
                 (contents-begin (progn (forward-line)
                                        (and (< (point) block-end-line)
                                             (point))))
                 (contents-end (and contents-begin block-end-line))
                 (pos-before-blank (progn (goto-char block-end-line)
                                          (forward-line)
                                          (point)))
                 (end (progn (skip-chars-forward " \r\t\n" limit)
                             (if (eobp) (point) (line-beginning-position)))))
            (list 'special-block
                  (nconc
                   (list :type type
;;;; parameters and title support, begin ;;;;
                         :title (and (org-string-nw-p title) (org-trim title))
                         :params (org-babel-parse-header-arguments
                                  (and (org-string-nw-p params) (org-trim params)))
;;;; parameters and title support, end ;;;;
                         :begin begin
                         :end end
                         :contents-begin contents-begin
                         :contents-end contents-end
                         :post-blank (count-lines pos-before-blank end)
                         :post-affiliated post-affiliated)
                   (cdr affiliated)))))))))

(defun org-fontify-meta-lines-and-blocks-1--advice (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (ignore-errors
      (when (re-search-forward
             (rx bol (group (zero-or-more blank) "#"
                            (group (group (or (seq "+" (one-or-more (any "a-zA-Z")) (optional ":"))
                                              space
                                              eol))
                                   (optional (group "_" (group (one-or-more (any "a-zA-Z"))))))
                            (zero-or-more blank)
                            (group (group (zero-or-more (not (any " \t\n"))))
                                   (zero-or-more blank)
                                   (group (zero-or-more any)))))
             limit t)
        (let ((beg (match-beginning 0))
              (end-of-beginline (match-end 0))
              (block-start (match-end 0))  ; includes the \n at end of #+begin line
              (block-end nil)              ; will include \n after end of block content
              (lang (match-string 7))      ; the language, if it is an src block
              (bol-after-beginline (line-beginning-position 2))
              (dc1 (downcase (match-string 2)))
              (dc3 (downcase (match-string 3)))
              (whole-blockline org-fontify-whole-block-delimiter-line)
              beg-of-endline end-of-endline nl-before-endline quoting block-type)
          (cond
;;;; begin: add fontify for special-blocks ;;;;
           ((and (match-end 4)
                 (member (downcase (match-string 5)) org-special-block-extras-blocks)
                 (member dc3 '("+begin" "+end")))
            (add-text-properties beg end-of-beginline '(face org-block-begin-line)))
;;;; end: add fontify for special-blocks ;;;;
           ((and (match-end 4) (equal dc3 "+begin"))
            ;; Truly a block
            (setq block-type (downcase (match-string 5))
                  quoting (member block-type org-protecting-blocks)) ; src, example, export, maybe more
            (when (and
                   (not (member block-type org-special-block-extras-blocks))
                   (re-search-forward
                    (rx-to-string `(group bol (or (seq (one-or-more "*") space)
                                                  (seq (zero-or-more blank)
                                                       "#+end"
                                                       ,(match-string 4)
                                                       word-end
                                                       (zero-or-more any)))))
                    nil t))  ;; on purpose, we look further than LIMIT
              ;; We do have a matching #+end line
              (setq beg-of-endline (match-beginning 0)
                    end-of-endline (match-end 0)
                    nl-before-endline (1- (match-beginning 0)))
              (setq block-end (match-beginning 0)) ; includes the final newline.
              (when quoting
                (org-remove-flyspell-overlays-in bol-after-beginline nl-before-endline)
                (remove-text-properties beg end-of-endline
                                        '(display t invisible t intangible t)))
              (add-text-properties
               beg end-of-endline '(font-lock-fontified t font-lock-multiline t))
              (org-remove-flyspell-overlays-in beg bol-after-beginline)
              (org-remove-flyspell-overlays-in nl-before-endline end-of-endline)
              (cond
               ((and lang (not (string= lang "")) org-src-fontify-natively)
                (org-src-font-lock-fontify-block lang block-start block-end)
                (add-text-properties bol-after-beginline block-end '(src-block t)))
               (quoting
                (add-text-properties
                 bol-after-beginline beg-of-endline
                 (list 'face
                       (list :inherit
                             (let ((face-name
                                    (intern (format "org-block-%s" lang))))
                               (append (and (facep face-name) (list face-name))
                                       '(org-block)))))))
               ((not org-fontify-quote-and-verse-blocks))
               ((string= block-type "quote")
                (add-face-text-property
                 bol-after-beginline beg-of-endline 'org-quote t))
               ((string= block-type "verse")
                (add-face-text-property
                 bol-after-beginline beg-of-endline 'org-verse t)))
              ;; Fontify the #+begin and #+end lines of the blocks
              (add-text-properties
               beg (if whole-blockline bol-after-beginline end-of-beginline)
               '(face org-block-begin-line))
              (unless (string-prefix-p "*" (match-string 1))
                (add-text-properties
                 beg-of-endline
                 (if whole-blockline
                     (let ((beg-of-next-line (1+ end-of-endline)))
                       (min (point-max) beg-of-next-line))
                   (min (point-max) end-of-endline))
                 '(face org-block-end-line)))
              t))
           ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
            (org-remove-flyspell-overlays-in
             (match-beginning 0)
             (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
            (add-text-properties
             beg (match-end 3)
             (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
                 '(font-lock-fontified t invisible t)
               '(font-lock-fontified t face org-document-info-keyword)))
            (add-text-properties
             (match-beginning 6) (min (point-max) (1+ (match-end 6)))
             (if (string-equal dc1 "+title:")
                 '(font-lock-fontified t face org-document-title)
               '(font-lock-fontified t face org-document-info))))
           ((string-prefix-p "+caption" dc1)
            (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display t invisible t intangible t))
            ;; Handle short captions.
            (save-excursion
              (beginning-of-line)
              (looking-at (rx (group (zero-or-more blank)
                                     "#+caption"
                                     (optional "[" (zero-or-more any) "]")
                                     ":")
                              (zero-or-more blank))))
            (add-text-properties (line-beginning-position) (match-end 1)
                                 '(font-lock-fontified t face org-meta-line))
            (add-text-properties (match-end 0) (line-end-position)
                                 '(font-lock-fontified t face org-block))
            t)
           ((member dc3 '(" " "")) ; Just a comment, the plus was not there
            (org-remove-flyspell-overlays-in beg (match-end 0))
            (add-text-properties
             beg (match-end 0)
             '(font-lock-fontified t face font-lock-comment-face)))
           (t ;; just any other in-buffer setting, but not indented
            (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display t invisible t intangible t))
            (add-text-properties beg (match-end 0)
                                 '(font-lock-fontified t face org-meta-line))
            t)))))))

(defun org-special-block-extras--html-build-head--advice (ret)
  (if (equal org-html-htmlize-output-type 'inline-css)
      (org-element-normalize-string
       (concat ret (string-join org-special-block-extras-html-heads "\n")))
    ret))

;;

(cl-defmacro org-special-block-extras--regist-block (name (backend item contents &rest others) &body body)
  (declare (indent 2))
  (let ((fun-name (intern (format "org-special-block-extras--%s" name)))
        (fun-args (list backend item contents)))
    (if others (setq fun-args (append fun-args others)))
    `(progn
       (cl-pushnew ,(symbol-name name) org-special-block-extras-blocks :test #'string-equal)
       (defun ,fun-name ,fun-args ,@body))))

(defun org-special-block-extras--regist-link (type &rest parameters)
  (when (member type (mapcar 'car org-special-block-extras-links))
    (setq org-special-block-extras-links
          (cl-remove-if (lambda (x) (string-equal type (car x)))
                        org-special-block-extras-links)))
  (push (cons type parameters) org-special-block-extras-links))

(defun org-special-block-extras--active-links ()
  (dolist (link org-special-block-extras-links)
    (apply #'org-link-set-parameters link))
  org-special-block-extras-links)

(defun org-special-block-extras--inactive-links ()
  (dolist (link org-special-block-extras-links)
    (setq org-link-parameters
          (cl-remove-if (lambda (x) (string-equal (car link) (car x))) org-link-parameters)))
  (org-link-make-regexps)
  (when (featurep 'org-element) (org-element-update-syntax)))

;;;###autoload
(define-minor-mode org-special-block-extras-mode
  "Custom blocks & link types for Org-mode."
  :body
  (if org-special-block-extras-mode
      (progn
        (advice-add #'org-html-special-block :before-until (apply-partially #'org-special-block-extras--advice 'html))
        (advice-add #'org-latex-special-block :before-until (apply-partially #'org-special-block-extras--advice 'latex))
        (advice-add #'org-element-special-block-parser :override #'org-element-special-block-parser--advice)
        (advice-add #'org-fontify-meta-lines-and-blocks-1 :override #'org-fontify-meta-lines-and-blocks-1--advice)
        (advice-add #'org-html--build-head :filter-return #'org-special-block-extras--html-build-head--advice)
        (org-special-block-extras--active-links)

        (setq org-export-allow-bind-keywords t))

    (advice-remove #'org-html-special-block (apply-partially #'org-special-block-extras--advice 'html))
    (advice-remove #'org-latex-special-block (apply-partially #'org-special-block-extras--advice 'latex))
    (advice-remove #'org-element-special-block-parser #'org-element-special-block-parser--advice)
    (advice-remove #'org-fontify-meta-lines-and-blocks-1 #'org-fontify-meta-lines-and-blocks-1--advice)
    (advice-remove #'org-html--build-head #'org-special-block-extras--html-build-head--advice)
    (org-special-block-extras--inactive-links)))


;;; colors

(defvar org-special-block-extras--colors '(black blue brown cyan darkgray gray green lightgray lime
                                                 magenta olive orange pink purple red teal violet white
                                                 yellow)
  "Colors that should be available on all systems.")

(org-special-block-extras--regist-link "color"
                                       :follow (lambda (_))
                                       :face (lambda (color)
                                               (if (member (intern color) org-special-block-extras--colors)
                                                   `(:foreground ,(format "%s" color))
                                                 `(:underline (:color "red" :style wave))))
                                       :help-echo (lambda (_ __ position)
                                                    (save-excursion
                                                      (goto-char position)
                                                      (let ((path (plist-get (cadr (org-element-context)) :path)))
                                                        (if (member (intern path) org-special-block-extras--colors)
                                                            "Color links just colur the descriptive text"
                                                          (format "Error: “color:%s” ⇒ Unsupported color!" path)))))
                                       :export (lambda (color description backend)
                                                 (let ((block-coloring (intern (format "org-special-block-extras--%s" color))))
                                                   (if (member (intern color) org-special-block-extras--colors)
                                                       (funcall block-coloring backend nil description nil t)
                                                     (error "Error: “color:%s” ⇒ Unsupported color!" color)))))

(org-special-block-extras--regist-block color (backend item contents _b)
  "Format CONTENTS according to the ‘:color:’ they specify for BACKEND."
  (let* ((color-name (org-element-property :title item))
         (block-coloring (intern (format "org-special-block-extras--%s" (s-trim color-name)))))
    (if (member (intern (s-trim color-name)) org-special-block-extras--colors)
        (funcall block-coloring backend nil contents nil)
      (error "Error: '#+begin_color:%s' -> Unsupported color!" color-name))))

(defmacro org-special-block-extras--make-color-funs ()
  (cons 'progn
        (cl-loop for color in org-special-block-extras--colors
                 collect
                 `(org-special-block-extras--regist-block ,color (backend _ contents __ &optional inline-p)
                    (let ((tag (if inline-p "span" "div")))
                      (cond ((eq backend 'html)
                             (format ,(format "<%%s style=\"color:%s;\">%%s</%%s>" color) tag contents tag))
                            ((eq backend 'latex)
                             (format ,(format "\\begingroup\\color{%s}%%s\\endgroup" color) contents))))))))

(org-special-block-extras--make-color-funs)


;;; section/div/h

(defvar org-special-block-extras--html-tags '("div" "section" "header" "h1" "h2" "h3" "h4" "h5"))

(defmacro org-special-block-extras--make-html-tags ()
  (cons 'progn
        (cl-loop for tag in org-special-block-extras--html-tags
                 collect
                 `(org-special-block-extras--regist-block ,(intern tag) (backend item contents _b)
                    "#+begin_div :style xxx... :break: to split into multiple divs."
                    (let ((params (org-element-property :params item)))
                      (cond ((eq backend 'html)
                             (format ,(format "\n<%s %%s>\n%%s\n</%s>\n\n" tag tag)
                                     (cl-loop for prop in params
                                              concat (format " %s='%s'" (substring (symbol-name (car prop)) 1) (cdr prop)))
                                     (let ((contents_arr (s-split ":break:" contents t)))
                                       (if (cdr contents_arr)
                                           (cl-loop for s in contents_arr concat (format "<div>\n%s\n</div>\n" s))
                                         contents))))))))))

(org-special-block-extras--make-html-tags)


;;; details

(org-special-block-extras--regist-block details (backend item contents _)
  "Format CONTENTS as a ‘folded region’ according to BACKEND."
  (let ((title (or (cdr (assoc :title (org-element-property :params item)))
                   (org-element-property :title item))))
    (if (s-blank? title) (setq title "Details"))
    (format (s-collapse-whitespace
             (pcase backend
               (`html "<details class=\"details\">
                 <summary class=\"detail-summary\">
                   <span class=\"detail-title\">%s</span>
                 </summary>
                 <div class=\"detail-body\">
                 %s
                 </div>
              </details>")
               (`latex "\\begin{quote}
                 \\begin{tcolorbox}[colback=white,sharp corners,boxrule=0.4pt]
                   \\textbf{%s:}
                   %s
                 \\end{tcolorbox}
               \\end{quote}")))
            title contents)))


;;; kbd

(defvar org-special-block-extras-kbd-css "
<style>
 /* From: https://endlessparentheses.com/public/css/endless.css */
 .special-kbd {
   color: #333;
   background-color: #f7f7f7;
   border: 1px solid #ccc;
   border-radius: 6px;
   box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
   display: inline-block;
   font-family: 'Droid Sans Mono', monospace;
   font-size: 80%;
   font-weight: normal;
   line-height: inherit;
   margin: 2px .1em;
   padding: .04em .4em;
   text-shadow: 0 1px 0 #fff;
   word-spacing: -3px;
 }
</style>")

(cl-pushnew org-special-block-extras-kbd-css org-special-block-extras-html-heads :test 'string-equal)

(org-special-block-extras--regist-link
 "kbd"
 :follow (lambda (_))
 :export (lambda (label description backend)
           (format (pcase backend
                     ('html "<kbd class='special-kbd'> %s </kbd>")
                     ('latex "\texttt{%s}")
                     (_ "%s"))
                   (or description (s-replace "_" " " label)))))


;;; octoicon

(defvar org-special-block-extras--supported-octoicons
  '((home
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M16 9l-3-3V2h-2v2L8 1 0 9h2l1 5c0 .55.45 1 1 1h8c.55 0
   1-.45 1-1l1-5h2zm-4 5H9v-4H7v4H4L2.81 7.69 8 2.5l5.19 5.19L12
   14z\"></path></svg>")
    (link
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69
   3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10
   5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0
   2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5
   0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55
   13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z\"></path></svg>")
    (mail
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 14
   16\" width=\"14\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M0 4v8c0 .55.45 1 1 1h12c.55 0 1-.45
   1-1V4c0-.55-.45-1-1-1H1c-.55 0-1 .45-1 1zm13 0L7 9 1 4h12zM1
   5.5l4 3-4 3v-6zM2 12l3.5-3L7 10.5 8.5 9l3.5 3H2zm11-.5l-4-3
   4-3v6z\"></path></svg>")
    (report
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M0 2a1 1 0 011-1h14a1 1 0 011 1v9a1 1 0 01-1 1H7l-4
   4v-4H1a1 1 0 01-1-1V2zm1 0h14v9H6.5L4 13.5V11H1V2zm6
   6h2v2H7V8zm0-5h2v4H7V3z\"></path></svg>")
    (tag
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 15
   16\" width=\"15\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M7.73 1.73C7.26 1.26 6.62 1 5.96 1H3.5C2.13 1 1 2.13 1
   3.5v2.47c0 .66.27 1.3.73 1.77l6.06 6.06c.39.39 1.02.39 1.41
   0l4.59-4.59a.996.996 0 000-1.41L7.73 1.73zM2.38
   7.09c-.31-.3-.47-.7-.47-1.13V3.5c0-.88.72-1.59
   1.59-1.59h2.47c.42 0 .83.16 1.13.47l6.14 6.13-4.73
   4.73-6.13-6.15zM3.01 3h2v2H3V3h.01z\"></path></svg>")
    (clock
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 14
   16\" width=\"14\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M8 8h3v2H7c-.55 0-1-.45-1-1V4h2v4zM7 2.3c3.14 0 5.7 2.56
   5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 011.3 8c0-3.14 2.56-5.7
   5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14
   7-7-3.14-7-7-7z\"></path></svg>"))

  "An association list of supported OctoIcons.

Usage: (cadr (assoc 'ICON org-special-block-extras--supported-octoicons))")

(org-special-block-extras--regist-link
 "octoicon"
 :follow (lambda (_))
 :export (lambda (icon _ backend)
           (pcase backend
             (`html  (format
                      (s-collapse-whitespace
                       (cadr (assoc (intern icon) org-special-block-extras--supported-octoicons)))))
             (_ ""))))


;;; link here

(org-special-block-extras--regist-link
 "link-here"
 :follow (lambda (path) (message "This is a local anchor link named “%s”" path))
 :export  (lambda (label _ backend)
            (pcase backend
              (`html  (format (s-collapse-whitespace
                               "<a class=\"anchor\" aria-hidden=\"true\" id=\"%s\" href=\"#%s\">%s</a>")
                              label label (cadr (assoc 'link                                                       org-special-block-extras--supported-octoicons))))
              (_ ""))))


(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
