;;; imoox.el --- Org-Mode -*- lexical-binding: t -*-
;;; Commentary:

;; http://orgmode.org/manual/index.html

;;; Code:

(defcustom note-directory "~/.notes/"
  "Where is my notes."
  :type 'string :group 'imfine)

(defcustom note-publish-directory "~/.cache/_notes"
  "Where to publish the notes."
  :type 'string :group 'imfine)

(defcustom note-publish-exclude-expr "^\\(\\.\\).*"
  "What to ignore when publish."
  :type 'string :group 'imfine)

(defcustom note-publish-sitemap-function #'org-publish-sitemap-default
  "How to generete sitemap."
  :type 'symbol :group 'imfine)

(defcustom note-publish-post-script nil
  "Script to execute after note publish if present."
  :type 'string :group 'imfine)



;;; Commands

(defun im/org-publish-note (&optional force)
  (interactive)
  (require 'ox-publish)
  (with-current-buffer (find-file-read-only note-directory) ; for .dir-local
    (sit-for 0.3)
    (message "Begin to publish [%s] → [%s]..." note-directory note-publish-directory)
    (let ((start (current-time))
          (log-buffer '*org-publish-log*)
          (vc-handled-backends nil)
          (file-name-handler-alist nil)
          (gc-cons-threshold (* 50 1024 1024))
          (org-startup-folded 'showeverything)
          (org-publish-project-alist (im/org-generate-project-alist note-directory note-publish-directory)))
      (cl-letf (((symbol-function 'run-hooks) (lambda (&rest _) nil))
                ((symbol-function 'run-hook-with-args) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (fmt &rest args) (apply 'logit (append (list log-buffer fmt) args)))))
        (without-recentf (org-publish "nnn" force)))
      (logit log-buffer "\n=== %s ===\n\n\n" (time))
      (run-hooks 'post-publish-note-hook)
      (message "Publish Finished in %.2f seconds!" (time-subtract-seconds (current-time) start)))
    (kill-buffer)))

(defun im/org-publish-note-interactively (&optional force)
  "Do not use the `note-directory' specified, but choose note-dir interactively."
  (interactive)
  (let* ((note-pair (my-get-notes-dir-interactively))
         (note-dir (car note-pair))
         (note-pub-dir (cdr note-pair))
         (org-publish-project-alist (im/org-generate-project-alist note-dir note-pub-dir)))
    (when (y-or-n-p (format "Publish [%s] → [%s] ?" note-dir note-pub-dir))
      (im/org-publish-note force))))

(defun im/org-publish-set-default-note-directory ()
  (interactive)
  (let ((note-pair (my-get-notes-dir-interactively)))
    (setq note-directory (car note-pair))
    (setq note-publish-directory (cdr note-pair))
    (message "Default publish will be [%s] → [%s]"  note-directory note-publish-directory)))

(defun im/org-clear-timestamp-directory ()
  (interactive)
  (if (file-exists-p org-publish-timestamp-directory)
      (delete-directory org-publish-timestamp-directory t)))

(defun im/flush-lines-in-org-cache ()
  (interactive)
  (let ((file (read-file-name "File: " org-publish-timestamp-directory)))
    (if (and (file-regular-p file)
             (file-writable-p file)
             (string= (file-name-extension file) "cache"))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (call-interactively 'flush-lines)
          (save-buffer))
      (message "Cache file not available."))))

(defun im/org-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun im/org-wrap-src ()
  (interactive)
  (let ((beg "#+BEGIN_SRC") (end "#+END_SRC") (lang (read-from-minibuffer "Please input your type: ")))
    (if (use-region-p)
        (let ((regin-beg (region-beginning)) (regin-end (region-end)))
          (save-excursion
            (goto-char regin-end) (end-of-line)
            (insert (format "\n%s\n" end))
            (goto-char regin-beg) (beginning-of-line) (open-line 1)
            (insert (concat beg " " lang))
            (ignore-errors (org-edit-special) (org-edit-src-exit) (deactivate-mark))))
      (insert (format "%s %s\n\n%s\n" beg lang end))
      (forward-line -2))))



;;; Helpers

(defun my-get-notes-dir-interactively ()
  (cons (read-directory-name "Publish From: ")
        (read-directory-name "Publish To: ")))

(defun my-face-color (type)
  (if (eq type 'bg)
      (let ((bg (face-background 'default))) (if (and (env-linux-ng) (string= "unspecified-bg" bg)) "#333" bg))
    (let ((fg (face-foreground 'default))) (if (and (env-linux-ng) (string= "unspecified-fg" fg)) "#eee" fg))))

(defun my-org-clean-space (text backend _)
  "When export to HTML, delete blanks between Chinese Char."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]"))
      (setq text (replace-regexp-in-string (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp) "\\1\\2" text))             ;; Avoid `Newline' to `SPACE'
      (setq text (replace-regexp-in-string (format "\\(%s\\) +\\(<[a-z]>%s\\)" regexp regexp) "\\1\\2" text))          ;; Trim blanks before `BOLD'
      (setq text (replace-regexp-in-string (format "\\(%s</[a-z]>\\) +\\(%s\\)" regexp regexp) "\\1\\2" text)) text))) ;; Trim blanks after `BOLD'

(defun my-execute-script-after-publish ()
  (when (and note-publish-post-script (file-executable-p note-publish-post-script))
    (set-process-sentinel
     (start-process-shell-command "org-fire" "*org-publish-log-post-script*" note-publish-post-script)
     (lambda (proc event)
       (when (string-match-p "finished" event)
         (with-current-buffer (process-buffer proc)
           (view-mode -1)
           (goto-char (point-max))
           (insert (format "\n======= %s ========\n\n\n" (time)))
           (view-mode 1)))))))

(defun org-publish-sitemap-order-by-file-name (title list)
  (cl-loop for i in (cdr list)
           when (cdr (cadr i))
           do (setf (cdr (cadr i)) (seq-sort-by #'car #'string< (cdr (cadr i)))))
  (concat "#+TITLE: " title "\n\n" (org-list-to-org list)))

(defun im/org-generate-html-head ()
  (setq org-html-head
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
                        (format "pre.src { position:static; overflow:auto; background:%s; color:%s; } " (my-face-color 'bg) (my-face-color 'fg))
                        (format "pre.src:before { color:%s; } " (my-face-color 'bg))))))))

(defun im/org-generate-project-alist (note-dir note-pub-dir &optional exclude sitemap-fun)
  "Generate `org-publish-project-alist' from NOTE-DIR and NOTE-PUB-DIR."
  (if (or (not (file-exists-p note-dir))
          (not (file-writable-p note-pub-dir)))
      (error "Please give correct directory names."))
  (let* ((prefix (if (string-match "^.*/\\([^/]+\\)/*$" note-dir) (match-string 1 note-dir)))
         (org-name (concat prefix "-org"))
         (asset-name (concat prefix "-asset"))
         (exclude (or exclude note-publish-exclude-expr))
         (sitemap-fun (or sitemap-fun note-publish-sitemap-function)))
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
       :sitemap-function     ,sitemap-fun
       :sitemap-filename     ".sitemap.org")
      (,asset-name
       :base-directory       ,note-dir
       :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg\\|pdf\\|zip\\|html\\|txt"
       :exclude              ,exclude
       :publishing-directory ,note-pub-dir
       :publishing-function  org-publish-attachment
       :recursive            t)
      ("nnn" :components (,org-name ,asset-name)))))



;;; Mode Configuration

(defun im/org-configurations-0 ()
  (setq org-directory                    note-directory
        org-startup-indented             t
        org-hide-leading-stars           t
        org-hide-block-startup           t
        org-startup-with-inline-images   t
        org-image-actual-width           nil
        org-cycle-separator-lines        0
        org-pretty-entities              t
        org-src-fontify-natively         t
        org-src-tab-acts-natively        nil
        org-url-hexify-p                 nil
        org-list-allow-alphabetical      t

        org-log-into-drawer              t
        org-clock-into-drawer            t
        org-clock-out-remove-zero-time-clocks t

        org-use-sub-superscripts         '{}
        org-export-with-sub-superscripts '{}
        org-export-copy-to-kill-ring     nil
        org-blank-before-new-entry       '((heading . t) (plain-list-item . nil))

        org-html-html5-fancy             t
        org-html-doctype                 "html5"
        org-html-container-element       "section"
        org-html-validation-link         "Go ahead, never stop."
        org-html-htmlize-output-type     'inline-css
        org-html-head-include-scripts    nil)

  (setq org-default-task-file    (concat org-directory "000/task.org")
        org-default-notes-file   (concat org-directory "000/journal.org")
        org-agenda-files         (file-expand-wildcards (concat org-directory "000/*.org"))
        org-duration-format      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        org-tag-alist            '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Dodo" . ?d))
        org-todo-keywords        '((sequence "TODO(t!)" "TING(i!)" "|" "DONE(d!)" "CANCEL(c!)"))
        org-refile-targets       `((org-agenda-files . (:level . 1)))
        org-capture-templates    `(("t" "新任务" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
                                   ("d" "Diary"  plain (file+datetree ,org-default-notes-file) "%U\n\n%i%?" :empty-lines 1)
                                   ("n" "草稿箱" entry (file ,(concat org-directory "000/scratch.org")) "* %U\n\n%i%?" :prepend t :empty-lines 1))))

(defun im/org-configurations-1 ()
  ;; Publish
  (setq org-publish-list-skipped-files nil)
  (setq org-publish-timestamp-directory (concat _CACHE_ "org-publish-timestamp/"))
  (im/org-generate-html-head)
  (add-hook 'post-publish-note-hook 'my-execute-script-after-publish)

  ;; Src-Block
  (pushnew (cons "xml" 'sgml) org-src-lang-modes)
  (pushnew (cons "html" 'web) org-src-lang-modes)

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp       . t)
     (shell      . t)
     (python     . t)
     (ruby       . t)
     (haskell    . t)
     (java       . t)
     (restclient . t) ;; GET :s/hello.json
     (sql        . t)
     (gnuplot    . t)
     (ditaa      . t)
     (dot        . t)
     (plantuml   . t)))
  (setq org-confirm-babel-evaluate nil
        org-babel-default-header-args (cons '(:noweb . "no") (assq-delete-all :noweb org-babel-default-header-args)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(defun im/org-configurations-2 ()
  ;; Emphasis
  (setcar org-emphasis-regexp-components "：，。！、  \t('\"{")     ;; prematch
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- ：，。！、 \t.,:!?;'\")}\\") ;; postmatch
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"'")  ;; forbidden chars
  (setcar (nthcdr 3 org-emphasis-regexp-components) ".")            ;; body
  (setcar (nthcdr 4 org-emphasis-regexp-components) 1  )            ;; newlines
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Remove extra blank line for example block!
  (advice-add 'org-element-fixed-width-parser
              :filter-return
              (lambda (ret) (list 'fixed-width (plist-put (cadr ret) :value (string-trim (plist-get (cadr ret) :value))))))

  ;; Faces
  (defface hi-org-break `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee")))))
    "for org mode \\ break"
    :group 'imfine)
  (defun my-org-add-keywords ()
    (font-lock-add-keywords
     nil '(("\\\\\\\\$" 0 'hi-org-break)
           ("\\<\\(FIXME\\|NOTE\\|AIA\\|TODO\\):" 1 'font-lock-warning-face prepend))))
  (env-g (im/make-face-mono 'org-table))

  ;; Refresh color for html export
  (advice-add 'load-theme :after (lambda (&rest _) (im/org-generate-html-head)))
  (advice-add 'disable-theme :after (lambda (&rest _) (im/org-generate-html-head))))

(defun im/org-configurations-3 ()
  (unbind-key "C-," org-mode-map)

  ;; Hooks
  (add-hook-lambda 'org-mode-hook
    (delight 'org-indent-mode)
    (set (make-local-variable 'system-time-locale) "C")
    (my-org-add-keywords))

  (require 'org-download)
  ;; (require 'org-tempo nil t) ; old way src block easy template

  (x ox
     :config
     (add-to-list 'org-export-filter-paragraph-functions 'my-org-clean-space)))

(x org/w
   "Publish Keybinds:
   "
   "   [C-c c n] publish `note-directory'."
   "   [C-c c N] publish `note-directory', force generate."
   "   [C-c c m] Publish project interactively."
   "   [C-c c M] Publish project interactively, force generate.
   "
   " With `im/org-publish-set-default-note-directory' to reconfig `note-directory'.
   "
   " Add below to init file to permanently your private project directory:
   "
   "   (setq note-directory ...)
   "
   :bind
   (:map org-mode-map
         ("×" . "*") ("－" . "-")
         ("C-x a a" . im/org-wrap-src))

   :init
   (im/org-configurations-0)

   :config
   (im/org-configurations-1)
   (im/org-configurations-2)
   (im/org-configurations-3))



;;; Extensions - Drawing, Download, etc.

(x ob-dot
   :if (executable-find "dot") ;; choco install graphviz
   :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :if (executable-find "java")
   :config (setq org-ditaa-jar-path "~/.emacs.d/resource/ditaa.jar"))

(x ob-plantuml
   :if (and (executable-find "java") (executable-find "dot"))
   :config
   (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
   (when (and (null (file-exists-p org-plantuml-jar-path))
              (yes-or-no-p "Download plantuml.jar Now?"))
     ;; IF NOT WORK, RUN THIS IN SHELL:
     ;; wget https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar -O ~/.emacs.d/plantuml.jar
     (url-copy-file "https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar" org-plantuml-jar-path)))

(x gnuplot
   ;; TODO: Make it work on Windows.
   :if (executable-find "gnuplot")
   :init (setq gnuplot-program "gnuplot"))

(x org-download
   :config
   (setq org-download-backend (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t)
         org-download-screenshot-file (concat temporary-file-directory "clip.png"))

   (fset 'org-download-clipboard 'org-download-screenshot)

   (defun my-org-download-choose-dir (&rest _)
     (let ((dest-dir (read-directory-name "Save in directory: ")))
       (when (not (file-exists-p dest-dir))
         (make-directory dest-dir t))
       dest-dir))
   (advice-add 'org-download--dir :around 'my-org-download-choose-dir)

   (defun my-org-download-set-scrot-method (&rest _)
     (setq org-download-screenshot-method ;; linux: pacman -S xclip
           (cond
            ((executable-find "xclip") "xclip -selection clipboard -t image/png -o > %s")
            ((env-windows) "powershell -Command (Get-Clipboard -Format Image).save('%s')")
            (t (error "no proper tool, eg, xclip/powershell")))))
   (advice-add 'org-download-clipboard :before 'my-org-download-set-scrot-method)
   (advice-add 'org-download-screenshot :before 'my-org-download-set-scrot-method))



;;; Keys and Hydras

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") (lambda () (interactive) (require 'ox-publish) (imdra-org/body)))

(defhydra imdra-org (:color blue :foreign-keys warn :hint nil)
  "
   ^
   ^Org^              ^Clock^         ^Misc^
   ^───^──────────────^──^────────────^────^────────
   _c_ Capture       _d_ display      _q_  quit
   ^^                _i_ in           ^^
   _l_ Store link    _o_ out          ^^
   _L_ Stored Link   _j_ jump         ^^
   ^^                _r_ report       ^^
   _n_ Publish       _e_ effort       ^^
   _N_ Publish<f>    _C_ cancel       ^^
   _m_ Publish<i>                     ^^
   _M_ Publish<if>                    ^^
   ^^                ^^
   "
  ("q" nil)
  ("a" org-agenda)

  ("d" org-clock-display)
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("j" org-clock-goto)
  ("r" org-clock-report)
  ("e" org-clock-modify-effort-estimate)
  ("C" org-clock-cancel :color pink)

  ("l" org-store-link)
  ("L" org-insert-link)
  ("c" org-capture :exit t)

  ("n" im/org-publish-note :exit t)
  ("N" (lambda () (interactive) (im/org-publish-note t)) :exit t)
  ("m" im/org-publish-note-interactively :exit t)
  ("M" (lambda () (interactive) (im/org-publish-note-interactively t)) :exit t))


(provide 'imoox)

;;; imoox.el ends here
