;;; Code:

;;; Dependencies

(require 'ox-html)

;;; Define Back-End

(org-export-define-derived-backend 'impress-js 'html
  :menu-entry
  '(?i "Export to impress.js HTML Presentation"
       ((?I "To *buffer*" org-impress-export-as-html)
        (?i "To file" org-impress-export-to-html)
        (?o "To file and browse" org-impress-export-to-html-and-browse)))

  :options-alist
  '((:impress-javascript "IMPRESS_JS_FILE" nil org-impress-javascript t)
    (:impress-stylesheet "IMPRESS_CSS_FILE" nil org-impress-stylesheet t)
    (:impress-initial "IMPRESS_INITIAL" nil org-impress-initial newline)
    ;; class and data
    (:impress-ext-head "EXTRA_HEAD" nil org-impress-ext-head newline)
    (:impress-base-class "IMPRESS_CLASS" nil org-impress-base-class space)
    (:impress-root "IMPRESS_ROOT" nil nil space)
    (:impress-home "IMPRESS_HOME" nil nil space)
    (:impress-toc "IMPRESS_TOC" nil nil space)
    ;; page top/bottom
    (:impress-page-header "IMPRESS_PAGE_HEADER" nil nil newline)
    (:impress-page-footer "IMPRESS_PAGE_FOOTER" nil nil newline)
    ;; assistances
    (:with-page-home nil "home" org-impress-export-home t)
    (:impress-toolbar nil "tb" org-impress-toolbar t)
    (:impress-progress nil "pg" org-impress-progress t)
    ;; setups
    (:impress-include-default-style nil "default_style" org-impress-include-default-style t)
    (:impress-single nil "single" org-impress-single t))

  :translate-alist
  '((headline . org-impress-headline)
    (inner-template . org-impress-inner-template)
    (link . org-impress-link)
    (section . org-impress-section)
    (quote-block . org-impress-quote-block)
    (special-block . org-impress-special-block)
    (template . org-impress-template))

  :filters-alist '((:filter-parse-tree . org-impress-filter-parse-tree)))


;;; User Configuration Variables

(defcustom org-impress-extension "htm"
  "The extension for exported HTML files."
  :group 'org-export-impress
  :type 'string)

(defcustom org-impress-include-default-style t
  "Include default style or not."
  :group 'org-export-impress
  :type 'boolean)

(defcustom org-impress-base-class "step"
  "Base classList on section."
  :group 'org-export-impress
  :type 'string)

(defcustom org-impress-javascript (concat (file-name-directory (or load-file-name buffer-file-name)) "impress.js")
  "Path to the JavaScript file for impress.js."
  :group 'org-export-impress
  :type 'string)

(defcustom org-impress-stylesheet ""
  "Path to the Stylesheet file for impress.js."
  :group 'org-export-impress
  :type 'string)

(defcustom org-impress-ext-head ""
  "Alternate of HTML_HEAD."
  :group 'org-export-impress
  :type 'string)

(defcustom org-impress-initial "impress().init();"
  "Impress.js initial script."
  :group 'org-export-impress
  :type 'string)

(defcustom org-impress-export-home t
  "Show home page or not."
  :group 'org-export-impress
  :type 'boolean)

(defcustom org-impress-toolbar t
  "Use toolbar plugin or not."
  :group 'org-export-impress
  :type 'boolean)

(defcustom org-impress-progress t
  "Use toolbar plugin or not."
  :group 'org-export-impress
  :type 'boolean)

(defcustom org-impress-single 0
  "Wheather export to a single file. 0/1/2/3+: none/js/css/all."
  :group 'org-export-impress
  :type 'number)


;;; Internal Variables

(defconst org-impress-default-style "
body {
    font-family: 'Open Sans', sans-serif;
}
a {
    text-decoration: none;
}

/* step */
.step {
    min-width: 900px;
    min-height: 600px;
    padding: 20px 40px;
    overflow: hidden;
    font-size: 28px;
}
.impress-enabled .step { opacity: 0.4; }
.impress-enabled .step.active { opacity: 1 }
.step .substep { opacity: 0; }
.step .substep.substep-visible { opacity: 1; transition: opacity 1s; }

/* special */
.home { text-align: center; }
h1 { font-size; 60px; }
h2 { font-size; 40px; }
table { width: 100%; }
table, td, th { border:1px solid #333; padding:3px 10px; border-collapse:collapse; }

/* animations */
.future .fly-in { transform: translateY(-700px); opacity: 0.0; }
.present .fly-in { transform: translateY(0px); opacity: 1.0; transition: 2s; }
.past .fly-out { transform: translateY(700px); opacity: 0.0; transition: 2s; }

.future .fade-in { opacity: 0.0; }
.present .fade-in { opacity: 1.0; transition: 3s; }
.past .fade-out { opacity: 0.0; transition: 3s; }

.future .zoom-in { transform: scale(10); opacity: 0.0; }
.present .zoom-in { transform: scale(1); opacity: 1.0; transition: 3s; }
.past .zoom-out { transform: scale(10); opacity: 0.0; }

/* toolbar */
.impress-enabled div#impress-toolbar {
    position: fixed; right: 1px; bottom: 1px; opacity: 0.6;
}
.impress-enabled div#impress-toolbar > span { margin-right: 10px; }
body.impress-mouse-timeout div#impress-toolbar { display: none; }

/* processbar */
.impress-progressbar {
    position: absolute;
    right: 118px; bottom: 1px; left: 118px; border-radius: 7px;
    border: 2px solid rgba(100, 100, 100, 0.4);
}
.impress-progressbar div {
    width: 0; height: 2px; border-radius: 5px;
    background: rgba(75, 75, 75, 0.4);
    transition: width 1s linear;
}
.impress-progress {
    position: absolute; left: 59px; bottom: 1px;
    text-align: left; font-size: 10pt; opacity: 0.6;
}

/* misc */
body.impress-mouse-timeout { cursor: none; }
"

  "Default style...")


;;; Internal Functions

(defun org-impress--blank? (input)
  (or (null input) (zerop (length (org-trim input)))))

(defun org-impress--not-blank? (input)
  (and input (> (length (org-trim input)) 0)))

(defun org-impress--read-file (file &optional callback)
  "Read the FILE content as string, file can be a url."
  (with-temp-buffer
    (if (string-match-p "^\\(http\\|file://\\)" file)
        (url-insert-file-contents file)
      (insert-file-contents-literally file))
    (let ((buffer-str (buffer-substring-no-properties
                       (point-min) (point-max))))
      (if callback
          (funcall callback buffer-str)
        buffer-str))))

(defun org-impress--file-url-to-path (url)
  "Convert URL that points to local files to file path."
  (replace-regexp-in-string
   (if (string-equal system-type "windows-nt") "^file:///" "^file://")
   "" url))

(defun org-impress--format-line-number (headline info &optional seperator)
  (mapconcat #'number-to-string (org-export-get-headline-number headline info) (or seperator ".")))

(defun org-impress--format-props (props info)
  (if (org-impress--not-blank? props)
      (let* ((props-lst (split-string props "class:"))
             (data (org-impress--format-props-data (car props-lst)))
             (class (org-impress--format-props-class (cadr props-lst) info)))
        (list data class))
    (list nil (org-impress--format-props-class nil info))))

(defun org-impress--format-props-class (class info)
  "Format the class to a proper list. STEP is required.
With -/+ to remove or add to default classList."
  (if (org-impress--blank? class)
      (plist-get info :impress-base-class)
    (let ((base-class "step")
          (class-lst (split-string (plist-get info :impress-base-class))))
      (setq class-lst
            (pcase (substring class 0 1)
              ("+" (concatenate 'list class-lst (split-string (substring class 1))))
              ("-" (remove-if (lambda (e) (member e (split-string (substring class 1)))) class-lst))
              (_ (split-string class))))
      (add-to-list 'class-lst base-class)
      (delete-dups class-lst)
      (string-join class-lst " "))))

(defun org-impress--format-props-data (data)
  "Convert:  xx:33 yy:44 zz:222  to  data-xx='33' data-yy='44'."
  (if (org-impress--not-blank? data)
      (let ((lst (split-string (org-trim data) " +"))
            (fmt (lambda (item)
                   (format "data-%s\"" (replace-regexp-in-string ":\\|=" "=\"" item)))))
        (mapconcat fmt lst " "))))


;;; Template

(defun org-impress-template (contents info)
  (concat "<!DOCTYPE html>
<html lang=\""
          (plist-get info :language) "\">
<head>\n"
          (org-impress--build-meta info)
          (org-impress--build-head info)
          (org-html--build-mathjax-config info)
          "</head>
<body class=\"impress-not-supported\">\n"
          ;; Fallback
          (org-impress--build-fallback info)
          ;; Contents
          (org-impress--build-root contents info)
          ;; Toolbar
          (and (plist-get info :impress-toolbar) (org-impress--build-toolbar info))
          ;; Progress
          (and (plist-get info :impress-progress) (org-impress--build-progress info))
          ;; Initialize
          (org-impress--build-initial info)
          ;; Closing
          "</body>
</html>"))

(defun org-impress--build-root (contents info)
  (let ((data-out (car (org-impress--format-props
                        (plist-get info :impress-root) info))))
    (concat "<div id=\"impress\"" (and data-out (format " %s" data-out)) ">\n\n"
            contents
            "</div>\n\n")))

(defun org-impress-inner-template (contents info)
  (setq iii info)
  (concat
   ;; Home Page.
   (if (plist-get info :with-page-home)
       (org-impress-home info))
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-impress-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun org-impress--build-meta (info)
  (let ((title (org-export-data (plist-get info :title) info))
        (author (if (plist-get info :with-author)
                    (org-element-interpret-data
                     (org-element-map
                         (plist-get info :author)
                         (cons 'plain-text org-element-all-objects)
                       'identity info))))
        (description (plist-get info :html-description))
        (keywords (plist-get info :keywords))
        (charset (if (and org-html-coding-system (fboundp 'coding-system-get))
                     (coding-system-get org-html-coding-system 'mime-charset)
                   "utf-8")))
    (concat
     "<title>" title "</title>\n"
     (format "<meta charset=\"%s\">\n" charset)
     "<meta name=\"viewport\" content=\"width=1024\">\n"
     (org-impress--parse-meta "author" author)
     (org-impress--parse-meta "description" description)
     (org-impress--parse-meta "keywords" keywords)
     "<meta name=\"generator\" content=\"Org-mode\">\n"
     "<meta name=\"apple-mobile-web-app-capable\" content=\"yes\">\n\n"
     "<link rel=\"apple-touch-icon\" href=\"apple-touch-icon.png\">\n"
     "<link href=\"http://fonts.googleapis.com/css?family=Open+Sans:regular,semibold,italic,italicsemibold|PT+Sans:400,700,400italic,700italic|PT+Serif:400,700,400italic,700italic\" rel=\"stylesheet\">\n\n")))

(defun org-impress--parse-meta (key value)
  (let ((protect-string
         (lambda (str) (replace-regexp-in-string "\"" "&quot;" (org-html-encode-plain-text str)))))
    (if (org-string-nw-p value)
        (format "<meta name=\"%s\" content=\"%s\">\n" key (funcall protect-string value)))))

(defun org-impress--build-head (info)
  "Return information for the <head>..</head> of the HTML output."
  (org-element-normalize-string
   (concat
    (org-impress--build-default-style info)
    (org-impress--build-ext-stylesheet info)
    (plist-get info :impress-ext-head))))

(defun org-impress--build-default-style (info)
  (if (plist-get info :impress-include-default-style)
      (format "<style>%s\n</style>\n\n" org-impress-default-style)))

(defun org-impress--build-ext-stylesheet (info)
  (let ((css (plist-get info :impress-stylesheet)))
    (if (org-impress--not-blank? css)
        (if (> (plist-get info :impress-single) 1)
            (concat "<style>\n" (org-impress--read-file css) "\n</style>\n\n")
          (format "<link href=\"%s\" rel=\"stylesheet\">\n\n" css)))))

(defun org-impress--build-fallback (info)
  "<div class=\"fallback-message\">
    <p>Your browser <b>doesn't support the features required</b> by impress.js, so you are presented with a simplified version of this presentation.</p>
    <p>For the best experience please use the latest <b>Chrome</b>, <b>Safari</b> or <b>Firefox</b> browser.</p>
</div>\n\n")

(defun org-impress--build-toolbar (info) "<div id=\"impress-toolbar\"></div>\n\n")

(defun org-impress--build-progress (info) "<div class=\"impress-progressbar\"><div></div></div>\n<div class=\"impress-progress\"></div>\n\n")

(defun org-impress--build-initial (info)
  (concat (if (> (plist-get info :impress-single) 0)
              (concat "<script type=\"text/javascript\">\n"
                      (org-impress--read-file (plist-get info :impress-javascript))
                      "\n</script>\n\n")
            (format "<script src=\"%s\"></script>\n" (plist-get info :impress-javascript)))
          (format "<script>\n%s\n</script>\n\n" (plist-get info :impress-initial))))


;;; Contents

(defun org-impress-home (info)
  (let* ((title (org-export-data (plist-get info :title) info))
         (props (org-impress--format-props (plist-get info :impress-home) info))
         (data-out (car props)) (class-out (cadr props))
         (post-out (org-html--build-pre/postamble 'postamble info)))
    (org-element-normalize-string
     (concat "<section id=\"home\""
             (and class-out (format " class=\"%s home\"" class-out))
             (and data-out (format " %s" data-out))
             ">\n<h1>" title "</h1>\n"
             post-out
             "</section>"))))

(defun org-impress-toc (depth info)
  (let ((toc-entries (mapcar (lambda (headline)
                               (cons (org-impress--format-toc-headline headline info)
                                     (org-export-get-relative-level headline info)))
                             (org-export-collect-headlines info depth))))
    (when toc-entries
      (let* ((title (org-html--translate "Table of Contents" info))
             (props (org-impress--format-props (plist-get info :impress-toc) info))
             (data-out (car props)) (class-out (cadr props))
             (content-out (org-html--toc-text toc-entries)))
        (org-element-normalize-string
         (concat "\n<section id=\"table-of-contents\""
                 (and class-out (format " class=\"%s\"" class-out))
                 (and data-out (format " %s" data-out))
                 ">\n<h1>" title "</h1>\n"
                 "<div id=\"text-table-of-contents\">" content-out "</div>\n"
                 "</section>"))))))

(defun org-impress--format-toc-headline (headline info)
  (let* ((headline-number (org-export-get-headline-number headline info))
         (todo (and (plist-get info :with-todo-keywords)
                    (let ((todo (org-element-property :todo-keyword headline)))
                      (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         (text (org-export-data-with-backend
                (org-export-get-alt-title headline info)
                (org-export-toc-entry-backend 'html)
                info))
         (tags (and (eq (plist-get info :with-tags) t)
                    (org-export-get-tags headline info))))
    (format "<a href=\"#%s\">%s</a>"
            ;; Label.
            (or (org-element-property :CUSTOM_ID headline)
                (concat "step-" (org-impress--format-line-number headline info))
                (org-export-get-reference headline info))
            ;; Body.
            (concat
             (and (not (org-export-low-level-p headline info))
                  (org-export-numbered-headline-p headline info)
                  (concat (mapconcat #'number-to-string headline-number ".") ". "))
             (apply (plist-get info :html-format-headline-function)
                    todo todo-type priority text tags :section-number nil)))))

(defun org-impress-headline (headline contents info)
  "Transcode a HEADLINE element to HTML."
  (unless (org-element-property :footnote-section-p headline)
    (if (org-export-low-level-p headline info)
        ;; This is a deep sub-tree: export it as in ox-html.
        (let* ((type 'unordered))
          (concat
           (and (org-export-first-sibling-p headline info) (org-html-begin-plain-list type))
           (org-html-format-list-item contents type nil info nil (org-html-format-headline--wrap headline info))
           (and (org-export-last-sibling-p headline info) (org-html-end-plain-list type))))
      ;; Standard headline. Export it as a slide
      (let* ((level (org-export-get-relative-level headline info))
             (section-num (org-impress--format-line-number headline info))
             (section-tag (org-html--container headline info))

             (prefer-id (or (org-element-property :CUSTOM_ID headline)
                            (concat "step-" (org-impress--format-line-number headline info))))

             (data (org-export-get-node-property :DATA headline))
             (data-out (org-impress--format-props-data data))

             (class (org-export-get-node-property :CLASS headline))
             (class-out (concat (org-impress--format-props-class class info) " outline-1"))

             (style (org-export-get-node-property :STYLE headline))
             (style-out (and style (format "style=\"%s\"" (replace-regexp-in-string "\"" "\'" style))))

             (header (plist-get info :impress-page-header))
             (header-out (when header (format "\n<div class=\"page-header\">%s</div>\n" header)))
             (footer (plist-get info :impress-page-footer))
             (footer-out (when footer (format "\n<div class=\"page-footer\">%s</div>\n" footer)))

             (title (org-export-data (org-element-property :title headline) info))
             (title-out (and (string-prefix-p "+" title)
                             (format "\n<h%d>%s</h%d>\n" (1+ level) (substring title 1) (1+ level))))

             (first-content (car (org-element-contents headline)))
             (contents-out (if (not (eq (org-element-type first-content) 'section))
                               (concat (org-impress-section first-content "" info) contents)
                             contents)))
        (org-element-normalize-string
         (concat "<section"
                 (and prefer-id (format " id=\"%s\"" prefer-id))
                 (and class-out (format " class=\"%s\"" class-out))
                 (and data-out (format " %s" data-out))
                 (and style-out (format " %s" style-out))
                 ">\n"
                 title-out
                 header-out
                 contents-out
                 footer-out
                 "</section>"))))))

(defun org-impress-section (section contents info)
  (let ((parent (org-export-get-parent-headline section)))
    (if (not parent) contents
      (format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>\n"
              (+ (org-export-get-relative-level parent info)
                 (1- (plist-get info :html-toplevel-hlevel)))
              (or (org-element-property :CUSTOM_ID parent)
                  (if (org-export-numbered-headline-p parent info)
                      (org-impress--format-line-number parent info "-"))
                  (org-export-get-reference parent info))
              (or contents "")))))

(defun org-impress-link (link desc info)
  "Transcode a LINK object from Org to Impress. When `org-impress-single' is t,
the result is the Data URIs of the referenced image."
  (let* ((want-embed-image (and (> (plist-get info :impress-single) 2)
                                (string= "file" (org-element-property :type link))
                                (org-export-inline-image-p link (plist-get info :html-inline-image-rules))))
         (raw-path (org-element-property :path link))
         (clean-path (org-impress--file-url-to-path raw-path))
         (can-embed-image (and want-embed-image (file-readable-p clean-path))))
    (if can-embed-image
        (org-impress--format-image-data-uri link clean-path info)
      (if want-embed-image
          (error "Cannot embed image %s" raw-path)
        (replace-regexp-in-string "<a href=\"#" "<a href=\"#/step-"
                                  (org-html-link link desc info))))))

(defun org-impress--embedded-svg (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((start (re-search-forward "<[ \t\n]*svg[ \t\n]"))
          (end (re-search-forward "<[ \t\n]*/svg[ \t\n]*>")))
      (concat "<svg " (buffer-substring-no-properties start end)))))

(defun org-impress--format-image-data-uri (link path info)
  "Generate the data URI for the image referenced by LINK."
  (let* ((ext (downcase (file-name-extension path))))
    (if (string= ext "svg")
        (org-impress--embedded-svg path)
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (org-combine-plists
         (list :src
               (concat
                "data:image/" ext ";base64,"
                (with-temp-buffer ;; Base64 content
                  (insert-file-contents-literally path)
                  (base64-encode-region 1 (point-max))
                  (buffer-string))))
         (let* ((parent (org-export-get-parent-element link))
                (link (let ((container (org-export-get-parent link)))
                        (if (and (eq (org-element-type container) 'link)
                                 (org-html-inline-image-p link info))
                            container
                          link))))
           (and (eq (org-element-map parent 'link 'identity info t) link)
                (org-export-read-attribute :attr_html parent)))))
       info))))

(defun org-impress-quote-block (quote-block contents info)
  "Add class to Blockquote."
  (format "<blockquote %s>\n%s</blockquote>" "fragment" contents))

(defun org-impress-special-block (special-block contents info)
  "Especially for Notes."
  (let ((block-type (org-element-property :type special-block)))
    (if (string= block-type "NOTES")
        (format "<aside class=\"notes\">\n%s\n</aside>\n" contents)
      (org-html-special-block special-block contents info))))


;; Filters and Hooks.

(defun org-impress-filter-parse-tree (tree backend info)
  tree)


;;; End-user functions

;;;###autoload
(defun org-impress-export-as-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'impress-js "*Org HTML Export*" async subtreep visible-only body-only ext-plist (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-impress-convert-region-to-html ()
  (interactive)
  (org-export-replace-region-by 'impress-js))

;;;###autoload
(defun org-impress-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension (concat "." org-impress-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'impress-js file async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-impress-export-to-html-and-browse (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (browse-url-of-file (expand-file-name (org-impress-export-to-html async subtreep visible-only body-only ext-plist))))

;;;###autoload
(defun org-impress-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'impress-js filename
                      (concat "." (or (plist-get plist :html-extension) org-html-extension "html"))
                      plist pub-dir))


(provide 'ox-impress)
;;; ox-impress.el ends here
