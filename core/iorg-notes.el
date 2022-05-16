;;; iorg-notes.el --- Note publishing -*- lexical-binding: t -*-

;; After published or git pushed, you can:
;;
;;   (im/interactive-script "note-sync-outputs-to-vps.sh")  ; rsync out/*.html to vps
;;   (url-retrieve "https://vps/sys/pull-and-reload")       ; execute `git pull` on vps
;;
;; Use Commands + Hooks to make it automated. Read `init-xxx.el' for more.
;;

;; Capture Content from Browser
;;
;;   1. Enable Org Protocol:
;;      - Linux (s/try-to-enable-org-protocol-for-linux)
;;      - Windows (s/windows-generate-extra-config-for-emacs)
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

(defcustom note-directory org-directory
  "Current notes directory, take ORG-DIRECTORY default, or change it anyway."
  :type 'string :group 'imfine)


;;; Publishing

(defcustom note-publish-directory "~/.cache/_notes/"
  "Where to publish the notes. Default: ~/.notes -> ~/.cache/_notes."
  :type 'string :group 'imfine)

(defcustom note-publish-exclude-expr "^[a-z_.].*/\\|/[._]"
  "What to ignore when publish, default those begin with [a-z]."
  :type 'string :group 'imfine)

(defun im/note-publish (&optional force)
  (interactive)
  (require 'ox-publish)
  (let ((start (current-time)))
    (thread-first
      (with-current-buffer (find-file-read-only note-directory) ; for .dir-locals
        (sit-for 0.3) ; have to sleep for a while? waiting .dir-locals ready...
        (let ((backup-inhibited t)
              (case-fold-search nil)
              (auto-save-default nil)
              (file-name-handler-alist nil)
              (default-directory note-directory)
              (org-mode-hook nil)
              (org-startup-folded 'showeverything)
              (org-html-head (if (cl-plusp (length org-html-head)) org-html-head (im-note-publish-html-head--suggest)))
              (org-publish-project-alist (or org-publish-project-alist (im-note-publish-project-alist note-directory note-publish-directory)))
              (ic/temp-log-buffer "*note-publish-log*"))
          (unwind-protect
              (let ((gc-cons-threshold most-positive-fixnum))
                (run-hook-with-args 'note-publish-pre-hook ic/temp-log-buffer)
                (cw ">>> publish alist <<<\n{org-publish-project-alist}\n")
                (message "Publishing notes %s..." (propertize note-directory 'face 'font-lock-type-face))
                (with-message (lambda (fmt &rest args) (cw (apply #'format (or fmt "") args)))
                  (org-publish "nnn" force))
                (cw "\n=== {(time-str)} ===\n\n\n\n")
                (message "Published in %s seconds." (propertize (format "%.2f" (%time-subtract-seconds (current-time) start)) 'face '(:foreground "red")))
                (run-hook-with-args 'note-publish-post-hook ic/temp-log-buffer))
            (kill-buffer))))
      (with-supress-recentf))))

(defun im/note-publish-with-profiler()
  (interactive)
  (profiler-start 'cpu)
  (call-interactively 'im/note-publish)
  (profiler-report))

(defun im/note-publish-interactively (&optional force)
  "Do not use the `note-directory' specified, but choose note-dir interactively."
  (interactive)
  (with-supress-recentf
   (let ((note-directory (read-directory-name "Publish From: ")))
     (with-current-buffer (find-file-read-only note-directory)
       (let ((org-publish-project-alist (or org-publish-project-alist
                                            (im-note-publish-project-alist
                                             note-directory
                                             (read-directory-name "Publish To: ")))))
         (im/note-publish force))))))

(defun im/note-publish-reset-directories ()
  (interactive)
  (let ((note-pair (im-note-publish-read-directories)))
    (setq note-directory (car note-pair))
    (setq note-publish-directory (cdr note-pair))
    (message "Default publish will be [%s] → [%s]"  note-directory note-publish-directory)))

(defun im-note-publish-read-directories ()
  (cons (read-directory-name "Publish From: ")
        (read-directory-name "Publish To: ")))

(defun im-note-publish-html-head--suggest ()
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

(defun im-note-publish-project-alist (note-dir note-pub-dir &optional exclude)
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
         (setq files (nconc (remove root (org-uniquify
                                          (funcall fullfill-dirs
                                                   (mapcar #'file-name-directory files)
                                                   root)))
                            files))
         (setq files (sort files sort-predicate))
         (funcall sitemap-builder title (org-publish--sitemap-files-to-lisp files project 'tree format-entry)))))))

(defun:hook note-publish-pre-hook/debug (_)
  "Show some INFORMATION before PUBLISH."
  (message "\n>>> Publish Alist (%s):\n" default-directory)
  (pp org-publish-project-alist))

(defun:around org-publish-sitemap$ (f &rest args)
  "If you want to use your own sitemap generated function, specific IT to `org-publish-sitemap-custom-function'."
  (if (and (boundp 'org-publish-sitemap-custom-function)
           (symbol-value 'org-publish-sitemap-custom-function))
      (apply (symbol-value 'org-publish-sitemap-custom-function) args)
    (apply f args)))


;;; Roam

(defvar org-roam-directory (let ((d (loco "Roam/"))) (if (file-exists-p d) (file-truename d))))

(defvar org-roam-file-exclude-regexp "sitemap.org")

(defvar origin-org-roam-directory org-roam-directory)

(defun roam-set-directory (&optional arg)
  (interactive "P")
  (setq org-roam-directory
        (read-directory-name "Directory: "
                             (if arg origin-org-roam-directory default-directory)
                             nil t))
  (message "org-roam-directory now is: %s" org-roam-directory))

(defun s/try-to-enable-org-protocol-for-linux ()
  (interactive)
  (cl-assert IS-LINUX)
  (let ((path
         "~/.local/share/applications/org-protocol.desktop")
        (content
         "[Desktop Entry]
          Name=Org-Protocol
          Exec=emacsclient %u
          Icon=emacs-icon
          Type=Application
          Terminal=false
          MimeType=x-scheme-handler/org-protocol")
        (xdg-command
         "xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol"))
    (if (file-exists-p path) (message "%s already exists." path)
      (with-current-buffer (find-file path)
        (insert (s%indent-string content)))
      (shell-command xdg-command))))

(x org-roam
   :ref ("org-roam/org-roam"
         "Roam Official: https://roamresearch.com/")
   :blackout (org-roam-mode . " ❇")
   :bind (("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph-show)
          ("C-c n r" . org-roam-node-random)
          ((org-mode-map
            ("C-c n i" . org-roam-node-insert)
            ("C-c n o" . org-id-get-create)
            ("C-c n t" . org-roam-tag-add)
            ("C-c n a" . org-roam-alias-add)
            ("C-c n l" . org-roam-buffer-toggle))))
   :init
   (setq org-roam-v2-ack t)
   (setq org-roam-tag-sources '(prop last-directory))
   (setq org-roam-completion-everywhere t)
   :defer-config
   (require 'org-roam-protocol)
   (org-roam-db-autosync-mode 1)

   (setq org-roam-capture-templates
         '(("d" "default"
            plain "%?"
            :if-new (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_aliases:\n")
            :unnarrowed t)))

   ;; 1) add `xdg-org-protocol.desktop`
   ;; 2) xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
   ;; 3) bookmark: javascript:location.href = 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title) + '&body=' + encodeURIComponent(window.getSelection())
   (setq org-roam-capture-ref-templates
         '(("r" "ref"
            plain "* ${slug}\n:PROPERTIES:\n:KEY: ${title}\n:REF: ${ref}\n:END:\n\n${body}%?"
            :if-new (file+head "roam/capture-from-web.org" "#+title: Contents Captured from Web")
            :immediate-finish t
            :empty-lines-before 2
            :empty-lines-after 1
            :unnarrowed t)))

   (setq org-roam-graph-viewer
         (cond (IS-WIN
                (or (executable-find "chrome")
                    (executable-find "edge")
                    (let ((f "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"))
                      (if (file-executable-p f) f))))
               (IS-LINUX-G
                (or (executable-find "microsoft-edge-beta")
                    (executable-find "google-chrome")
                    (executable-find "firefox")
                    (executable-find "chrome"))))))

(x org-roam-ui
   :ref "org-roam/org-roam-ui"
   :after org-roam
   :defer-config
   (setq org-roam-ui-follow t)
   (setq org-roam-ui-update-on-save t))

(provide 'iorg-notes)

;;; iorg-notes.el ends here
