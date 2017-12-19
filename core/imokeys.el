;;;========================================
;;;     keybinds   |   org-mode
;;;========================================

(bind-keys
 (  [f1]          . info                 )
 (  [f6]          . toggle-truncate-lines)
 (  [f7]          . (lambda () (interactive) (find-file-read-only "~/.emacs.d/core/immor.el")))
 (  [f8]          . calendar             )
 (  [f10]         . shrink-window        )
 (  [f11]         . enlarge-window       )
 (  [f12]         . im/toggle-fullscreen )
 (  "<insertchar>". undo                 )
 (  "<select>"    . im/toggle-dedicated  )
 (  "%"           . his-match-paren      )
 (  "C-c C-j"     . ffap                 )
 (  "M-q"         . tiny-code            )
 (  "M-w"         . im/copy-current-line )
 (  "C-h t"       . im/trans-word        )
 (  "ESC <down>"  . im/copy-lines        )
 (  "ESC <up>"    . (lambda () (interactive) (im/copy-lines 1)))
 (  "ESC <right>" . im/kill-lines        )
 (  "C-x p"       . other-frame          ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun im/init-org-n-note (notes-home &optional notes-title standalone)
  (interactive (list (read-directory-name "选择笔记目录: ")
                     (read-from-minibuffer "输入笔记标题: ")))
  (let* ((base/res "assets/") (base/out "html/")
         (download/image (concat base/res "image/"))
         (html/css (concat base/res "base.css"))
         (html/js  (concat base/res "base.js"))
         (base/res/out (concat base/out base/res))
         (notes-title (if (or (null notes-title) (string= "" notes-title)) "imfineandu" notes-title)))

    (setq org-directory             notes-home
          org-default-task-file     (concat org-directory "z.task.org")
          org-default-notes-file    (concat org-directory "z.journal.org")
          org-agenda-files          (list org-default-task-file org-default-notes-file)
          org-log-done              'time
          org-log-into-drawer       t
          org-tags-column           (* -1 (frame-width))
          org-tag-alist             '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Dodo" . ?d))
          org-todo-keywords         '((sequence "TODO(t)" "TING(i!)" "|" "DONE(d)" "CANCEL(c)"))
          org-refile-targets        `((org-agenda-files . (:level . 1))))

    (setq org-startup-indented                 t
          org-hide-leading-stars               t
          org-hide-block-startup               t
          org-startup-with-inline-images       t
          org-cycle-separator-lines            0
          org-src-fontify-natively             t
          org-src-tab-acts-natively            nil
          org-emphasis-regexp-components  '("：，。！、  \t('\"{"          ;pre
                                            "- ：，。！、 \t.,:!?;'\")}\\" ;post
                                            " \t\r\n,\"'"  "."  1)

          org-export-preserve-breaks           nil
          org-export-copy-to-kill-ring         nil
          org-export-with-section-numbers      t
          org-export-with-sub-superscripts     nil
          org-publish-list-skipped-files       nil

          org-confirm-babel-evaluate           nil
          org-babel-load-languages '((emacs-lisp . t) (dot . t) (sh . t) (sql . t)
                                     (lisp . t) (haskell . t) (python . t) (ruby . t)
                                     (java . t))

          org-html-html5-fancy                 t
          org-html-doctype                     "html5"
          org-html-container-element           "section"
          org-html-validation-link             "Go ahead, never stop."
          org-html-htmlize-output-type         'css
          org-html-head-include-scripts        nil
          org-html-head-include-default-style  nil
          org-html-head "<meta  name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n")

    (setq org-publish-project-alist
          `(("org-files"
             :base-directory       ,org-directory
             :publishing-directory ,(concat org-directory base/out)
             :headline-levels      3
             :with-toc             3
             :html-preamble        nil
             :auto-sitemap         t
             :sitemap-filename     "index.org"
             :sitemap-title        ,notes-title
             :html-link-up         "index.html"
             :html-link-home       "index.html"
             :publishing-function  org-html-publish-to-html
             :recursive            t
             :html-head
             ,(concat org-html-head
                      (if standalone
                          (format "<style>\n%s\n</style>\n<script>\n%s\n</script>\n"
                                  (if (executable-find "csstidy")
                                      (shell-command-to-string
                                       (format "csstidy %s %s --template=highest --silent=true" notes-home html/css))
                                    (with-temp-buffer (insert-file-contents (concat notes-home html/css)) (buffer-string)))
                                  (with-temp-buffer (insert-file-contents (concat notes-home html/js)) (buffer-string)))
                        (format "<link rel=\"stylesheet\" href=\"%s\">\n<script src=\"%s\"></script>\n" html/css html/js))))
            ("resources"
             :base-directory       ,(concat org-directory base/res)
             :publishing-directory ,(concat org-directory base/res/out)
             :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg"
             :publishing-function  org-publish-attachment
             :recursive            t)
            ("notes" :components ("resources" "org-files"))))

    (setq org-download-image-dir     download/image
          org-download-backend       (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t))

    (setq org-capture-templates
          `(("t" "新任务" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
            ("d" "Diary" plain (file+datetree ,org-default-notes-file) "%U\n\n%i%?" :empty-lines 1)
            ("n" "草稿箱" entry (file ,(concat (file-name-directory  org-default-notes-file) "z.scratch.org")) "* %U\n\n%i%?" :prepend t :empty-lines 1)))

    (message "%s@%s/%s" notes-home notes-title standalone)))

;; initalization
(x org/w
   :bind (( "C-c a"    . org-agenda )
          ( "C-c l"    . org-store-link )
          ( "C-c c"    . org-capture )
          ( "C-c n"    . im/org-publish-note )
          ( "C-c C-n"  . im/org-publish-note-force )
          (:map org-mode-map ( "C-x a a" . im/org-wrap-code )))
   :init
   ;; configuration
   (if (with-classroom) (im/init-org-n-note "e:/home/share/notes/" "Index")
     (im/init-org-n-note "~/.cases/notes/"))

   ;; publish
   (defun im/org-publish-note (&optional force)
     (interactive)
     (require 'ox-publish)
     (if force (ignore-errors (delete-directory org-publish-timestamp-directory t)))
     (save-excursion (org-publish "notes"))
     (message "Publish Finished!"))
   (defun im/org-publish-note-force () (interactive) (im/org-publish-note 'force))

   :config
   ;; load all
   (require 'ox-publish)

   ;; hook
   (add-hook-lambda 'org-mode-hook
     ;; faces and keywordsa
     (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'hi-org-break)
                                   ("\\<\\(FIXME\\|NOTE\\|AIA\\):" 1 'font-lock-warning-face prepend)))
     ;; fix for time
     (set (make-local-variable 'system-time-locale) "C"))

   ;; face etc
   (defface hi-org-break
     `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee"))))) "for org mode \\ break" :group 'org-faces)
   (with-windows (set-face-attribute 'org-table nil :font "fontset-table" :fontset "fontset-table"))

   ;; fix for babel
   (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot)
   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

   ;; remap keys
   (define-key org-mode-map (kbd "×") (kbd "*"))
   (define-key org-mode-map (kbd "－") (kbd "-"))

   ;; SRC wrapper
   (defun im/org-wrap-code (&optional beg end)
     (interactive)
     (let ((beg (or beg "#+BEGIN_SRC")) (end (or end "#+END_SRC")) point)
       (if (use-region-p)
           (let ((rbeg (region-beginning)) (rend (region-end)) (lang (read-from-minibuffer "Please input your type: ")))
             (save-excursion
               (goto-char rend) (end-of-line)
               (newline) (insert end) (newline)
               (goto-char rbeg) (beginning-of-line)
               (open-line 1) (insert (concat beg " " lang))
               (indent-according-to-mode)))
         (insert beg) (indent-according-to-mode) (setq point (point))
         (newline-and-indent) (insert end) (newline) (goto-char point))))

   ;; fix for org-download
   (x org-download/e :config
      (defun org-download--dir-advice (f &rest args)
        (or (file-name-base (buffer-file-name)) ""))
      ;; rename downloaded file
      (defun org-download-rename-file-at-point ()
        "Rename the downloaded file at point."
        (interactive)
        (let* ((pattern "^file:\\(.+\\)$")
               (atpoint (ffap-url-at-point))
               (origin (if (and atpoint (string-match pattern atpoint))
                           (match-string 1 atpoint)))
               (from (expand-file-name (or origin ""))))
          (if (and origin (file-exists-p from) (eq major-mode 'org-mode))
              (let ((to (format "%s/%s.%s"
                                (org-download--dir)
                                (read-string "Rename file to: " (file-name-base from))
                                (file-name-extension from))))
                (rename-file from to 1)
                (org-download-replace-all (file-name-nondirectory from)
                                          (file-name-nondirectory to))
                (org-display-inline-images))
            (message "not available"))))
      (advice-add 'org-download--dir-2 :around #'org-download--dir-advice)))






(provide 'imokeys)

;;; imokeys.el ends here
