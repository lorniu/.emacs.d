;;; -*- lexical-binding: t -*-

;; After published or git pushed, you can:
;;
;;   (im/interactive-script "notes-publish-to-vps.sh")    ; rsync *.html to vps
;;   (url-retrieve "https://${vps}/sys/pull-and-reload")  ; execute `git pull` on vps
;;
;; Capture Content from Browser
;;
;;   1. Enable Org Protocol:
;;      - Linux (im/org-protocol-enable-for-linux)
;;      - Windows (in/emacs-generate-configs-for-windows)
;;
;;   2. Bookmark in Browser as:
;;      #+begin_src js
;;        javascript:location.href='org-protocol://roam-ref?template=a&ref='+encodeURIComponent(location.href) + '&title='+encodeURIComponent(document.title) + '&body='+encodeURIComponent(function(){var html = '';var sel = window.getSelection();if (sel.rangeCount) {var container = document.createElement('div');for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());}html = container.innerHTML;}var dataDom = document.createElement('div');dataDom.innerHTML = html;['p', 'h1', 'h2', 'h3', 'h4'].forEach(function(tag, idx){dataDom.querySelectorAll(tag).forEach(function(item, index) {var content = item.innerHTML.trim();if (content.length > 0) {item.innerHTML = content + '&#13;&#10;';}});});return dataDom.innerText.trim();}())
;;        javascript:location.href='org-protocol://store-link?url='+encodeURIComponent(location.href)
;;      #+end_src
;;
;; Then capture with a suitable template. See `org-roam-capture-ref-templates' below.
;;

;;; Code:

(defcustom org-publish-directory (locc "_notes")
  "Where to publish the notes."
  :type 'string
  :group 'imfine)

(defcustom org-publish-exclude-regexp "^[a-uw-z_.].*/\\|/[._]"
  "What to ignore when publish, default those begin with [a-z]."
  :type 'string
  :group 'imfine)

(defun %note-publish-project-alist (note-dir note-pub-dir &optional exclude)
  "Generate `org-publish-project-alist' from NOTE-DIR and NOTE-PUB-DIR."
  (when (and (not (file-exists-p note-pub-dir)) (file-exists-p (file-name-parent-directory note-pub-dir)))
    (make-directory note-pub-dir))
  (when (or (not (file-exists-p note-dir))
            (not (file-writable-p note-pub-dir)))
    (error "Please give correct directory names."))
  (let* ((prefix (if (string-match "^.*/\\([^/]+\\)/*$" note-dir) (match-string 1 note-dir)))
         (org-name (concat prefix "-org"))
         (asset-name (concat prefix "-asset"))
         (exclude (or exclude org-publish-exclude-regexp)))
    `((,org-name
       :base-directory       ,note-dir
       :base-extension       "org"
       :exclude              ,exclude
       :publishing-directory ,note-pub-dir
       :publishing-function  org-html-publish-to-html
       :recursive            t
       :with-toc             2
       :html-preamble        t
       :auto-sitemap         t
       :sitemap-filename     ".sitemap.org")
      (,asset-name
       :base-directory       ,note-dir
       :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg\\|pdf\\|zip\\|html\\|txt\\|jar"
       :exclude              ,exclude
       :publishing-directory ,note-pub-dir
       :publishing-function  org-publish-attachment
       :recursive            t)
      ("nnn" :components (,org-name ,asset-name)))))

(defun %note-publish-html-head--suggest ()
  (cl-labels ((face-color (type)
                (if (eq type 'bg)
                    (let ((bg (face-background 'default))) (if (and IS-LINUX-NG (string= "unspecified-bg" bg)) "#333" bg))
                  (let ((fg (face-foreground 'default))) (if (and IS-LINUX-NG (string= "unspecified-fg" fg)) "#eee" fg)))))
    (format "<style>\n%s\n</style>"
            (im:string-pad
             (list "body  { padding:10px; font-size:13px; }"
                   "a     { text-decoration:none; }"
                   "table { border-collapse:collapse; max-width:100%; margin-bottom:1em; }"
                   "th,td { border:1px solid #ccc; padding:0.6em 1em; }"
                   "pre   { margin-left: 0; margin-right:0; }"
                   "blockquote { border-left:3px solid #999; padding-left:10px; margin-left:10px; }"
                   ".org-src-container { position:relative }"
                   (format "pre.src { position:static; overflow:auto; background:%s; color:%s; } " (face-color 'bg) (face-color 'fg))
                   (format "pre.src:before { color:%s; } " (face-color 'bg)))))))

(defun im/note-publish (&optional force)
  (interactive)
  (with-supress-recentf
   (require 'ox-publish)
   (let ((start (current-time))
         (ic/temp-log-buffer "*note-publish-log*"))
     (with-current-buffer (find-file-read-only org-directory) ; for .dir-locals
       (sit-for 0.3) ; have to sleep for a while? waiting .dir-locals ready...
       (let ((default-directory org-directory)
             (case-fold-search nil)
             (backup-inhibited t)
             (auto-save-default nil)
             (file-name-handler-alist nil)
             (org-startup-folded 'showeverything)
             (org-html-head (if (cl-plusp (length org-html-head)) org-html-head (%note-publish-html-head--suggest)))
             (org-publish-project-alist (or org-publish-project-alist (%note-publish-project-alist org-directory org-publish-directory))))
         (unwind-protect
             (let ((gc-cons-threshold most-positive-fixnum))
               (run-hook-with-args 'note-publish-pre-hook ic/temp-log-buffer)
               (cw ">>> publish alist <<<\n{org-publish-project-alist}\n")
               (message "Publishing notes %s..." (propertize org-directory 'face 'font-lock-type-face))
               (im:with-message (lambda (fmt &rest args) (cw (apply #'format (or fmt "") args)))
                 (org-publish "nnn" force))
               (cw "\n=== {(time-str)} ===\n\n\n\n")
               (message "Published in %s seconds." (propertize (format "%.2f" (%time-subtract-seconds (current-time) start)) 'face '(:foreground "red")))
               (run-hook-with-args 'note-publish-post-hook ic/temp-log-buffer))
           (kill-buffer)))))))

(defun im/note-publish-interactively (&optional force)
  "Prompt note directory instead of `org-directory'."
  (interactive)
  (with-supress-recentf
   (let ((org-directory (read-directory-name "Publish From: ")))
     (with-current-buffer (find-file-read-only org-directory)
       (let ((org-publish-project-alist (or org-publish-project-alist
                                            (%note-publish-project-alist
                                             org-directory
                                             (read-directory-name "Publish To: ")))))
         (im/note-publish force))))))

(defun im/note-publish-reset-directories ()
  (interactive)
  (setq org-directory (read-directory-name "Publish From: "))
  (setq org-publish-directory (read-directory-name "Publish To: "))
  (message "Default publish will be [%s] → [%s]"  org-directory org-publish-directory))

(defun:hook note-publish-pre-hook/debug (_)
  "Show some INFORMATION before PUBLISH."
  (message "\n>>> Publish Alist (%s):\n" default-directory)
  (pp org-publish-project-alist))

;; make sitemap generating customable

(defvar org-publish-sitemap-custom-function)

(defun:around org-publish-sitemap//override-by-custom-function (f &rest args)
  "If you want to use your own sitemap generated function, specific IT to `org-publish-sitemap-custom-function'."
  (if (and (boundp 'org-publish-sitemap-custom-function)
           (symbol-value 'org-publish-sitemap-custom-function))
      (apply (symbol-value 'org-publish-sitemap-custom-function) args)
    (apply f args)))

(defun org-publish-sitemap-2 (project &optional sitemap-filename)
  "Predefined sitemap generated function.
Used as values of `org-publish-sitemap-custom-function', see \\='notes/.dir-locals.el' for more."
  (let* ((root (expand-file-name (file-name-as-directory (org-publish-property :base-directory project))))
         (sitemap-filename (concat root (or sitemap-filename "sitemap.org")))
         (title (or (org-publish-property :sitemap-title project) (concat "Sitemap for project " (car project))))
         (sitemap-builder (or (org-publish-property :sitemap-function project) #'org-publish-sitemap-default))
         (format-entry (or (org-publish-property :sitemap-format-entry project) #'org-publish-sitemap-default-entry))
         (fullfill-dirs (lambda (dirs until)
                          (cl-labels ((parent-dir (dir) (file-name-directory (directory-file-name dir)))
                                      (lookup (dir)
                                        (if (cl-search until dir)
                                            (let ((pd (parent-dir dir)))
                                              (when (cl-search until pd)
                                                (cl-pushnew pd dirs :test 'string=)
                                                (lookup pd)))
                                          (cl-delete dir dirs :test 'string=))))
                            (cl-loop for d in dirs do (lookup d) finally (return dirs)))))
         (sort-predicate (lambda (a b)
                           (if (string-equal (cl-substitute ?! ?/ (file-name-directory a))
                                             (cl-substitute ?! ?/ (file-name-directory b)))
                               (string-lessp a b)
                             (string-lessp (cl-substitute ?! ?/ (file-name-directory a))
                                           (cl-substitute ?! ?/ (file-name-directory b)))))))
    (message "Generating sitemap for %s" title)
    (with-temp-file sitemap-filename
      (insert
       (let ((files (remove sitemap-filename (org-publish-get-base-files project))))
         (setq files (nconc (remove root
                                    (org-uniquify (funcall fullfill-dirs (mapcar #'file-name-directory files) root)))
                            files))
         (setq files (sort files sort-predicate))
         (funcall sitemap-builder title (org-publish--sitemap-files-to-lisp files project 'tree format-entry)))))))



(x org-noter
   :ref (org "org-noter: org-noter/org-noter"))
