;;;========================================
;;;     keybinds   |   org-mode
;;;========================================

(defkey mmm/keymap
  (  [f1]           info                 )
  (  [f4]           hideshowvis-minor-mode )
  (  [f5]           hs-toggle-hiding     )
  (  [S-f5]         im/hs-toggle-all     )
  (  [f6]           toggle-truncate-lines)
  (  [f7]           (ilambda (find-file-read-only "~/.emacs.d/core/immor.el")))
  (  [f8]           calendar             )
  (  [f9]           compile              )
  (  [f10]          shrink-window        )
  (  [f11]          enlarge-window       )
  (  [f12]          im/toggle-fullscreen )
  (  "<insertchar>" undo                 )
  (  "<select>"     im/toggle-dedicated  )

  (  "%"            his-match-paren      )
  (  "C-c C-j"      ffap                 )
  (  "M-q"          tiny-code            )
  (  "M-w"          im/copy-current-line )
  (  "C-x a a"      im/org-wrap-code     )
  (  "C-h t"        im/trans-word        )
  (  "ESC <down>"   im/copy-lines        )
  (  "ESC <up>"     (ilambda (im/copy-lines 1)))
  (  "ESC <right>"  im/kill-lines        )
  (  "M-s o"        im/occur-pt          )
  (  "C-c a"        org-agenda           )
  (  "C-c l"        org-store-link       )
  (  "C-c c"        org-capture          )
  (  "M-o"          origami-recursively-toggle-node )
  (  "C-M-o"        origami-toggle-all-nodes )
  (  "C-x p"        other-frame          )
  (  "C-c m"        magit-status         )

  (  "C-x C-b"      ibuffer              )
  (  "M-x"          counsel-M-x          )
  (  "C-x d"        counsel-projectile-find-dir )
  (  "C-x C-d"      dired                )
  (  "C-x f"        counsel-projectile-find-file )
  (  "C-x C-f"      counsel-find-file    )
  (  "C-x i"        counsel-imenu        )
  (  "C-h b"        counsel-descbinds    )
  (  "C-h v"        counsel-describe-variable )
  (  "C-h f"        counsel-describe-function )
  (  "C-h S"        counsel-info-lookup-symbol)
  (  "C-c n"        im/org-publish-note       )
  (  "C-c C-n"      im/org-publish-note-force ))

(with-eval-after-load 'view
  (defkey view-mode-map
    (  "h"     backward-char  )
    (  "l"     forward-char   )
    (  "j"     next-line      )
    (  "k"     previous-line  )
    (  "C-f"   forward-sexp   )
    (  "C-b"   backward-sexp  )))

;; when occur, word under cursor first
(defun im/occur-pt (str)
  (interactive (list (aif (symbol-at-point) (symbol-name it))))
  (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history)))

;; save the window configuration before delete windows
;; return back when need [F12]
(defun im/toggle-fullscreen ()
  (interactive)
  (if (> (count-windows) 1)
      (progn (window-configuration-to-register 99)
             (delete-other-windows))
    (if (get-register 99)
        (jump-to-register 99)
      (message "never save window configurations"))))

;; toggle show/hide all
(defun im/hs-toggle-all ()
  (interactive)
  (if (and (boundp im/hs-state) im/hs-state)
      (progn (setf im/hs-state nil) (hs-show-all))
    (setf im/hs-state t) (hs-hide-all)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; initialize
(defun im/init-org-n-note (notes-home &optional notes-title standalone)
  (interactive (list (read-directory-name "选择笔记目录: ")
                     (read-from-minibuffer "输入笔记标题: ")))
  (let* ((base/res "assets/") (base/out "html/")
         (download/image (concat base/res "image/"))
         (html/css (concat base/res "base.css"))
         (html/js  (concat base/res "base.js"))
         (base/res/out (concat base/out base/res))
         (notes-title (if (or (null notes-title) (string= "" notes-title)) "IMFINEANDU" notes-title)))
    
    (setq org-directory             notes-home
          org-default-task-file     (concat org-directory "9.task.org")
          org-default-notes-file    (concat org-directory "9.journal.org")
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
          `(("t" "New Task" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
            ("n" "Note Now" entry (file+headline ,org-default-notes-file "Quick Notes") "* %i%?" :prepend t)
            ("d" "Qian Cao" entry (file+datetree ,org-default-notes-file) "* %U %^g\n%i%?" :empty-lines 1)))

    (message "%s   ##%s##   ##standalone: %s" notes-home notes-title standalone)))

;; pre config
(if (with-classroom)
    (im/init-org-n-note "e:/home/share/notes/" "Index")
  (im/init-org-n-note "~/.cases/notes/"))

;; load org
(require 'org)

;; post config
(with-eval-after-load 'org
  ;; fix for babel
  (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; fix for time
  (add-hook-lambda org-mode-hook (set (make-local-variable 'system-time-locale) "C"))

  ;; remap keys
  (define-key org-mode-map (kbd "×") (kbd "*"))
  (define-key org-mode-map (kbd "－") (kbd "-"))

  ;; fix for org-download
  (require 'org-download)
  (defun org-download--dir-advice (f &rest args)
    (or (file-name-base (buffer-file-name)) ""))
  (advice-add 'org-download--dir-2 :around #'org-download--dir-advice)
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
        (message "not available")))))

;;; functions ;;;
(defun im/org-publish-note ()
  (interactive)
  (require 'ox-md)
  (save-excursion (org-publish "notes"))
  (message "Publish Finished!"))

(defun im/org-publish-note-force ()
  (interactive)
  (ignore-errors (delete-directory org-publish-timestamp-directory t))
  (im/org-publish-note))

(defun* im/org-wrap-code (&optional (a "#+BEGIN_SRC") (z "#+END_SRC"))
  "helper to generate CODE wrapper"
  (interactive)
  (if (use-region-p)
      (let ((ra (region-beginning)) (rz (region-end))
            (lang (read-from-minibuffer "Please input your type: ")))
        (save-excursion
          (goto-char rz) (end-of-line)
          (newline) (insert z) (newline)
          (goto-char ra) (beginning-of-line)
          (open-line 1) (insert (concat a " " lang))
          (indent-according-to-mode)))
    (let (p)
      (insert a) (indent-according-to-mode) (setq p (point))
      (newline-and-indent) (insert z) (newline) (goto-char p))))






(provide 'imokeys)

;;; imokeys.el ends here
