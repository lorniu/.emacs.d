;;; imoox-note.el --- Note publishing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom note-directory org-directory
  "Current notes directory, take ORG-DIRECTORY default, or change it anyway."
  :type 'string :group 'imfine)

(defcustom note-publish-directory "~/.cache/_notes/"
  "Where to publish the notes."
  :type 'string :group 'imfine)

;;; Publish Notes, default: ~/.notes -> ~/.cache/_notes

(defcustom note-publish-exclude-expr "^[a-zA-Z]?\\..*"
  "What to ignore when publish."
  :type 'string :group 'imfine)


;;; Core Commands

(defun im/note-publish (&optional force)
  (interactive)
  (require 'ox-publish)
  (let ((start (current-time)))
    (with-supress-recentf
     (with-current-buffer (find-file-read-only note-directory) ; for .dir-locals
       (sit-for 0.3) ; may be sleep for a while? waiting .dir-locals ready...
       (let ((vc-handled-backends nil)
             (file-name-handler-alist nil)
             (gc-cons-threshold most-positive-fixnum)
             (default-directory note-directory)
             (log-buffer '*note-publish-log*)
             (org-mode-hook nil)
             (org-startup-folded 'showeverything)
             (org-html-head (if (plusp (length org-html-head)) org-html-head (im/note-publish-html-head--suggest)))
             (org-publish-project-alist (or org-publish-project-alist (im/note-publish-project-alist note-directory note-publish-directory))))
         (run-hook-with-args 'note-publish-pre-hook log-buffer)
         (log/it log-buffer ">>> publish alist <<<\n%s\n" org-publish-project-alist)
         (message "Publishing notes %s ..." (propertize note-directory 'face 'font-lock-type-face))
         (with-message (lambda (fmt &rest args) (apply 'log/it (append (list log-buffer (or fmt "")) args)))
           (org-publish "nnn" force))
         (log/it log-buffer "\n=== %s ===\n\n\n\n" (time-str))
         (message "Published in %s seconds." (propertize (format "%.2f" (time-subtract-seconds (current-time) start)) 'face '(:foreground "red")))
         (run-hook-with-args 'note-publish-post-hook log-buffer))
       (kill-buffer)))))

(quote (progn (profiler-start 'cpu)
              (call-interactively 'im/note-publish)
              (profiler-report)))

(defun im/note-publish-interactively (&optional force)
  "Do not use the `note-directory' specified, but choose note-dir interactively."
  (interactive)
  (with-supress-recentf
   (let ((note-directory (read-directory-name "Publish From: ")))
     (with-current-buffer (find-file-read-only note-directory)
       (let ((org-publish-project-alist (or org-publish-project-alist
                                            (im/note-publish-project-alist
                                             note-directory
                                             (read-directory-name "Publish To: ")))))
         (im/note-publish force))))))

(defun im/note-publish-reset-directories ()
  (interactive)
  (let ((note-pair (im/note-publish-read-directories)))
    (setq note-directory (car note-pair))
    (setq note-publish-directory (cdr note-pair))
    (message "Default publish will be [%s] → [%s]"  note-directory note-publish-directory)))


;;; Helpers

(defun im/note-publish-read-directories ()
  (cons (read-directory-name "Publish From: ")
        (read-directory-name "Publish To: ")))

(defun im/note-publish-html-head--suggest ()
  (cl-labels ((face-color (type)
                          (if (eq type 'bg)
                              (let ((bg (face-background 'default))) (if (and IS-LINUX-NG (string= "unspecified-bg" bg)) "#333" bg))
                            (let ((fg (face-foreground 'default))) (if (and IS-LINUX-NG (string= "unspecified-fg" fg)) "#eee" fg)))))
    (string-join-newline
     (format "<style>\n%s\n</style>"
             (padding-left-to-string
              (list "body  { padding:10px; font-size:13px; }"
                    "a     { text-decoration:none; }"
                    "table { border-collapse:collapse; max-width:100%; margin-bottom:1em; }"
                    "th,td { border:1px solid #ccc; padding:0.6em 1em; }"
                    "pre   { margin-left: 0; margin-right:0; }"
                    "blockquote { border-left:3px solid #999; padding-left:10px; margin-left:10px; }"
                    ".org-src-container { position:relative }"
                    (format "pre.src { position:static; overflow:auto; background:%s; color:%s; } " (face-color 'bg) (face-color 'fg))
                    (format "pre.src:before { color:%s; } " (face-color 'bg))))))))

(defun im/note-publish-project-alist (note-dir note-pub-dir &optional exclude)
  "Generate `org-publish-project-alist' from NOTE-DIR and NOTE-PUB-DIR."
  (if (or (not (file-exists-p note-dir))
          (not (file-writable-p note-pub-dir)))
      (error "Please give correct directory names."))
  (let* ((prefix (if (string-match "^.*/\\([^/]+\\)/*$" note-dir) (match-string 1 note-dir)))
         (org-name (concat prefix "-org"))
         (asset-name (concat prefix "-asset"))
         (exclude (or exclude note-publish-exclude-expr)))
    `((,org-name
       :base-directory       ,note-dir
       :base-extension       "org"
       :exclude              ,exclude
       :publishing-directory ,note-pub-dir
       :publishing-function  org-html-publish-to-html
       :recursive            t
       :headline-levels      3
       :with-toc             3
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


;;; Hooks

(defvar note-push-triggled-url nil)
(defvar note-publish-post-script nil)

(add-hook-fun note-publish-pre-hook/debug (_)
  "Show some INFORMATION before PUBLISH."
  (message "\n>>> Publish Alist (%s):\n" default-directory)
  (pp org-publish-project-alist))

(add-hook-fun note-publish-post-hook/script (_)
  "Execute script after PUBLISH, if script exists."
  (when note-publish-post-script
    (im/interactive-script note-publish-post-script "*note-publish-post-script-log*")))

(add-hook-fun gac/commit-and-push-hook/reload-host ()
  "Request `note-push-triggled-url' to reload or etc, when invoke `gac/commit-and-push' under notes."
  (let ((project-dir (cdr (project-current))))
    (when (and note-push-triggled-url
               (string= (expand-file-name project-dir) note-directory))
      (url-retrieve note-push-triggled-url
                    (lambda (_)
                      (re-search-forward "\n\n")
                      (alert (format "%s" (buffer-substring (point) (point-max))))
                      (kill-buffer))))))


;;; Patches for Sitemap

(defun:around org-publish-sitemap$ (f &rest args)
  "If you want to use your own sitemap generated function, specific IT to `org-publish-sitemap-custom-function'."
  (if (and (boundp 'org-publish-sitemap-custom-function)
           (symbol-value 'org-publish-sitemap-custom-function))
      (apply (symbol-value 'org-publish-sitemap-custom-function) args)
    (apply f args)))

(defun org-publish-sitemap-2 (project &optional sitemap-filename)
  "Predefined sitemap generated function, can be used by `org-publish-sitemap-custom-function' at once."
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
                                                      (pushnew pd dirs :test 'string=)
                                                      (lookup pd)))
                                                (cl-delete dir dirs :test 'string=))))
                            (loop for d in dirs do (lookup d) finally (return dirs)))))
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
         (setq files (nconc (remove root (org-uniquify
                                          (funcall fullfill-dirs
                                                   (mapcar #'file-name-directory files)
                                                   root)))
                            files))
         (setq files (sort files sort-predicate))
         (funcall sitemap-builder title (org-publish--sitemap-files-to-lisp files project 'tree format-entry)))))))


(provide 'imoox-note)

;;; imoox-note.el ends here
