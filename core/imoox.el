;;; imoox.el --- Org-Mode
;;; Commentary:

;; http://orgmode.org/manual/index.html

;;; Code:

(eval-when-compile (require 'cl))
(defvar note-dir '("~/.notes/" . "~/.cache/_notes"))


;;; Configurations

(defun im/org-configurations ()
  (setq org-directory (car note-dir)
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
        org-publish-list-skipped-files   nil
        org-blank-before-new-entry       '((heading . t) (plain-list-item . nil))

        org-html-html5-fancy             t
        org-html-doctype                 "html5"
        org-html-container-element       "section"
        org-html-validation-link         "Go ahead, never stop."
        org-html-htmlize-output-type     'inline-css
        org-html-head-include-scripts    nil)

  (im/org-config-html-head)
  (im/org-config-gtd))

(defun im/org-config-html-head ()
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
                        (format "pre.src { position:static; overflow:auto; background:%s; color:%s; } " (-my/face-color 'bg) (-my/face-color 'fg))
                        (format "pre.src:before { color:%s; } " (-my/face-color 'bg))))))))

(defun im/org-config-gtd ()
  (setq org-default-task-file    (concat org-directory "000/task.org")
        org-default-notes-file   (concat org-directory "000/journal.org")
        org-agenda-files         (file-expand-wildcards (concat org-directory "000/*.org"))
        org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        org-tag-alist            '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Dodo" . ?d))
        org-todo-keywords        '((sequence "TODO(t!)" "TING(i!)" "|" "DONE(d!)" "CANCEL(c!)"))
        org-refile-targets       `((org-agenda-files . (:level . 1)))
        org-capture-templates    `(("t" "新任务" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
                                   ("d" "Diary"  plain (file+datetree ,org-default-notes-file) "%U\n\n%i%?" :empty-lines 1)
                                   ("n" "草稿箱" entry (file ,(concat org-directory "000/scratch.org")) "* %U\n\n%i%?" :prepend t :empty-lines 1))))

(defun im/org-config-babel ()
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
     (plantuml   . t)
     ))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-default-header-args (cons '(:noweb . "no") (assq-delete-all :noweb org-babel-default-header-args)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(defun im/org-initialize-notes (from to)
  (interactive (list (read-directory-name "Publish From: ")
                     (read-directory-name "Publish To: ")))
  (setq org-publish-project-alist
        `(("org"
           :base-directory       ,from
           :base-extension       "org"
           :exclude              "^\\..*"
           :publishing-directory ,to
           :publishing-function  org-html-publish-to-html
           :recursive            t
           :headline-levels      3
           :with-toc             3
           :html-preamble        t
           :auto-sitemap         t)
          ("res"
           :base-directory       ,from
           :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg\\|pdf\\|zip"
           :exclude              "^\\..*"
           :publishing-directory ,to
           :publishing-function  org-publish-attachment
           :recursive            t)
          ("nnn" :components ("org" "res"))))
  (when (interactive-p)
    (setq note-dir (cons from to))
    (message "Will publish [%s] → [%s]" from to)))

(defun -my/face-color (type)
  (if (eq type 'bg)
      (let ((bg (face-background 'default))) (if (and (env-linux-ng) (string= "unspecified-bg" bg)) "#333" bg))
    (let ((fg (face-foreground 'default))) (if (and (env-linux-ng) (string= "unspecified-fg" fg)) "#eee" fg))))


;;; Major Mode

(x org/w :commands (org-publish)
   :bind (:map org-mode-map ( "C-x a a" . im/org-wrap-src ))
   :init (im/org-configurations)
   :config

   ;; post initialize
   (im/org-config-babel)
   (im/org-initialize-notes (car note-dir) (cdr note-dir))

   ;; Keys
   (define-key org-mode-map (kbd "×") (kbd "*"))
   (define-key org-mode-map (kbd "－") (kbd "-"))
   (unbind-key "C-," org-mode-map)

   ;; Faces
   (env-g (set-face-attribute 'org-table nil :family (im/find-ft im/probe-mono-fonts)))
   (defface hi-org-break `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee"))))) "for org mode \\ break")

   ;; Emphasis
   (setcar org-emphasis-regexp-components "：，。！、  \t('\"{")       ;; prematch
   (setcar (nthcdr 1 org-emphasis-regexp-components) "- ：，。！、 \t.,:!?;'\")}\\") ;; postmatch
   (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"'")  ;; forbidden chars
   (setcar (nthcdr 3 org-emphasis-regexp-components) ".")            ;; body
   (setcar (nthcdr 4 org-emphasis-regexp-components) 1  )            ;; newlines
   (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

   ;; Refresh color for html export
   (advice-add 'load-theme :after (lambda (&rest _) (im/org-config-html-head)))
   (advice-add 'disable-theme :after (lambda (&rest _) (im/org-config-html-head)))

   ;; Remove extra blank line for example block!
   (advice-add 'org-element-fixed-width-parser
               :filter-return
               (lambda (ret) (list 'fixed-width (plist-put (cadr ret) :value (string-trim (plist-get (cadr ret) :value))))))

   ;; Hooks
   (add-hook-lambda 'org-mode-hook
     (delight 'org-indent-mode)
     (set (make-local-variable 'system-time-locale) "C")
     (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'hi-org-break)
                                   ("\\<\\(FIXME\\|NOTE\\|AIA\\|TODO\\):" 1 'font-lock-warning-face prepend))))

   ;; Miscellaneous
   (x ox :config (add-to-list 'org-export-filter-paragraph-functions '-my/org-clean-space))
   (require 'ox-impress)
   (require 'org-download))

(defun -my/org-clean-space (text backend info)
  "When export to HTML, delete blanks between Chinese Char."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]"))
      ;; Avoid `Newline' to `SPACE'
      (setq text (replace-regexp-in-string (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp) "\\1\\2" text))
      ;; Trim blanks before `BOLD'
      (setq text (replace-regexp-in-string (format "\\(%s\\) +\\(<[a-z]>%s\\)" regexp regexp) "\\1\\2" text))
      ;; Trim blanks after `BOLD'
      (setq text (replace-regexp-in-string (format "\\(%s</[a-z]>\\) +\\(%s\\)" regexp regexp) "\\1\\2" text)) text)))


;;; Commands

(defun im/org-publish-note (&optional force)
  (interactive)
  (let ((start (current-time))
        (vc-handled-backends nil)
        (file-name-handler-alist nil)
        (gc-cons-threshold (* 10 1024 1024))
        (org-startup-folded 'showeverything))
    (with-temp-buffer
      (find-file (expand-file-name "*Notes*.org" (car note-dir))) ;; preload .dir-local.el before publish started
      (flet ((run-hooks (&rest hooks) nil)
             (run-hook-with-args (&rest args) nil))
        (org-publish "nnn" force))
      (run-hooks 'post-publish-note-hook)
      (kill-buffer))
    (message "Publish Finished in %.2f seconds!" (time-subtract-seconds (current-time) start))))

(defun im/org-publish-note-force ()
  (interactive)
  (im/org-publish-note 'force))

(defun im/org-publish-clear-cache ()
  (interactive)
  (if (file-exists-p org-publish-timestamp-directory)
      (delete-directory org-publish-timestamp-directory t)))

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

(defun im/org-reformat-src-outside ()
  (interactive)
  (save-excursion
    (forward-line)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))


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

   (defun -my/org-download-choose-dir (f &rest args)
     (let ((dest-dir (read-directory-name "Save in directory: ")))
       (when (not (file-exists-p dest-dir))
         (make-directory dest-dir t))
       dest-dir))
   (advice-add 'org-download--dir :around '-my/org-download-choose-dir)

   (defun -my/org-download-set-scrot-method (&rest args)
     (setq org-download-screenshot-method ;; linux: pacman -S xclip
           (cond
            ((executable-find "xclip") "xclip -selection clipboard -t image/png -o > %s")
            ((env-windows) "powershell -Command (Get-Clipboard -Format Image).save('%s')")
            (t (error "no proper tool, eg, xclip/powershell")))))
   (advice-add 'org-download-clipboard :before '-my/org-download-set-scrot-method)
   (advice-add 'org-download-screenshot :before '-my/org-download-set-scrot-method))

(provide 'imoox)

;;; imoox.el ends here
