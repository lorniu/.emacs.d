;;; iokeys.el --- Keybinds And Org-Mode
;;; Commentary:

;;; Code:

(bind-keys
 (  [f1]          . info                 )
 (  [f6]          . toggle-truncate-lines)
 (  [f7]          . (lambda () (interactive) (im/open-file-view "~/.emacs.d/core/immor.el")))
 (  [f8]          . calendar             )
 (  [f10]         . shrink-window        )
 (  [f11]         . enlarge-window       )
 (  [f12]         . im/toggle-fullscreen )
 (  "<insertchar>". undo                 )
 (  "<select>"    . im/toggle-dedicated  )
 (  "%"           . his-match-paren      )
 (  "C-c C-j"     . ffap                 )
 (  "M-q"         . im/tiny-code         )
 (  "M-w"         . im/copy-current-line )
 (  "C-h t"       . im/trans-word        )
 (  "ESC <down>"  . im/copy-lines        )
 (  "ESC <up>"    . (lambda () (interactive) (im/copy-lines 1)))
 (  "ESC <right>" . im/kill-lines        )
 (  "C-x p"       . other-frame          ))

;;;; Org-Mode

(defun im/init-org-n-note (notes-home &optional notes-title css-inline)
  "Initialize the Definitions."
  (interactive (list (read-directory-name "选择笔记目录: ")
                     (read-from-minibuffer "输入笔记标题: ")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (x) (cons x t))
           '( emacs-lisp dot sql lisp haskell python ruby java sh )))

  (let* ((base/res "assets/")
         (pull/img (concat base/res "image/"))
         (html/css (concat base/res "base.css"))
         (html/js  (concat base/res "base.js"))
         (base/out "html/")
         (base/out/res (concat base/out base/res))
         (notes-title (if (seq-empty-p notes-title) "imfineandu" notes-title)))

    (setq org-directory           notes-home
          org-default-task-file   (concat org-directory "z.task.org")
          org-default-notes-file  (concat org-directory "z.journal.org")
          org-agenda-files        (list org-default-task-file org-default-notes-file)
          org-log-into-drawer     t
          org-log-done            'time
          org-tags-column         (* -1 (frame-width))
          org-refile-targets      `((org-agenda-files . (:level . 1)))
          org-tag-alist           '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Dodo" . ?d))
          org-todo-keywords       '((sequence "TODO(t)" "TING(i!)" "|" "DONE(d)" "CANCEL(c)")))

    (setq org-startup-indented                t
          org-hide-leading-stars              t
          org-hide-block-startup              t
          org-startup-with-inline-images      t
          org-cycle-separator-lines           0
          org-src-fontify-natively            t
          org-emphasis-regexp-components      '("：，。！、  \t('\"{"          ;pre
                                                "- ：，。！、 \t.,:!?;'\")}\\" ;post
                                                " \t\r\n,\"'"  "."  1)

          org-html-html5-fancy                t
          org-html-doctype                    "html5"
          org-html-container-element          "section"
          org-html-validation-link            "Go ahead, never stop."
          org-html-htmlize-output-type        'css
          org-html-head-include-scripts       nil
          org-html-head-include-default-style nil
          org-html-head                       (im/html-viewport)

          org-use-sub-superscripts            '{}
          org-export-copy-to-kill-ring        nil
          org-publish-list-skipped-files      nil

          org-confirm-babel-evaluate          nil)

    (setq org-capture-templates
          `(("t" "新任务" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
            ("d" "Diary"  plain (file+datetree ,org-default-notes-file) "%U\n\n%i%?" :empty-lines 1)
            ("n" "草稿箱" entry (file ,(concat (file-name-directory  org-default-notes-file) "z.scratch.org")) "* %U\n\n%i%?" :prepend t :empty-lines 1)))

    (setq org-publish-timestamp-directory (concat _CACHE_ ".org-timestamps/")
          org-publish-project-alist
          `(("notes"
             :components           ("resources" "org-files"))

            ("org-files"
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
             :html-head           ,(concat org-html-head
                                           (if css-inline
                                               (format "<style>\n%s\n</style>\n<script>\n%s\n</script>\n"
                                                       (im/read-file-content (concat notes-home html/css))
                                                       (im/read-file-content (concat notes-home html/js)))
                                             (format "<link rel=\"stylesheet\" href=\"%s\">\n<script src=\"%s\"></script>\n" html/css html/js))))

            ("resources"
             :base-directory       ,(concat org-directory base/res)
             :publishing-directory ,(concat org-directory base/out/res)
             :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg"
             :publishing-function  org-publish-attachment
             :recursive            t)))

    (setq org-download-image-dir pull/img
          org-download-backend   (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t))

    (message "%s@%s/%s" notes-home notes-title css-inline)))

(x org/w :bind
   (( "C-c a"   . org-agenda )
    ( "C-c l"   . org-store-link )
    ( "C-c c"   . org-capture )
    ( "C-c n"   . im/org-publish-note )
    ( "C-c C-n" . im/org-publish-note-force )
    :map org-mode-map
    ( "C-x a a" . im/org-wrap-src))

   :init
   (defun im/org-wrap-src () (interactive)
     (let ((beg "#+BEGIN_SRC") (end "#+END_SRC")
           (lang (read-from-minibuffer "Please input your type: ")))
       (if (use-region-p)
           (let ((regin-beg (region-beginning)) (regin-end (region-end)))
             (save-excursion
               (goto-char regin-end) (end-of-line)
               (insert (format "\n%s\n" end))
               (goto-char regin-beg) (beginning-of-line) (open-line 1)
               (insert (concat beg " " lang))
               (ignore-errors
                 (org-edit-special) (org-edit-src-exit) (deactivate-mark))))
         (insert (format "%s %s\n\n%s\n" beg lang end))
         (forward-line -2))))

   (defun im/org-publish-note (&optional force)
     "For publish, FORCE for full publish."
     (interactive)
     (require 'ox-publish)
     (if force (ignore-errors (delete-directory org-publish-timestamp-directory t)))
     (save-excursion (org-publish "notes"))
     (message "Publish Finished!"))

   (defun im/org-publish-note-force ()
     (interactive)
     (im/org-publish-note 'force))

   :config
   ;; Load
   (require 'ox-publish)
   (if (env-classroom) (im/init-org-n-note "e:/home/share/notes/" "Index")
     (im/init-org-n-note "~/.cases/notes/"))

   ;; Hook
   (add-hook-lambda 'org-mode-hook
     (diminish 'org-indent-mode)
     (set (make-local-variable 'system-time-locale) "C")
     (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'hi-org-break)
                                   ("\\<\\(FIXME\\|NOTE\\|AIA\\):" 1 'font-lock-warning-face prepend))))

   ;; Faces
   (defface hi-org-break
     `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee"))))) "for org mode \\ break" :group 'org-faces)
   (env-windows (set-face-attribute 'org-table nil :font "fontset-table" :fontset "fontset-table"))

   ;; Fix babel
   (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot)
   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

   ;; Remap keys
   (define-key org-mode-map (kbd "×") (kbd "*"))
   (define-key org-mode-map (kbd "－") (kbd "-"))

   (x org-download/e
      :config
      (advice-add 'org-download--dir-2 :around
                  (lambda (_ &rest __) ;; Where to save?
                    (or (file-name-base (buffer-file-name)) "")))

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
            (message "Not available"))))))


(provide 'imokeys)

;;; imokeys.el ends here
