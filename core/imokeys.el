;;;========================================
;;;    keybinds | org-mode | overrides
;;;========================================

(defkey mmm~keymap
  (  [f1]           info                 )
  (  [f5]           hs-toggle-hiding     )
  (  [f6]           toggle-truncate-lines)
  (  [f7]           (ilambda (find-file-read-only "~/.emacs.d/core/immor.el")))
  (  [f8]           calendar             )
  (  [f9]           compile              )
  (  [f10]          shrink-window        )
  (  [f11]          enlarge-window       )
  (  [f12]          im/toggle-fullscreen )
  (  "<insertchar>" undo                 )
  (  "<select>"     im/toggle-dedicated  ))

(defkey mmm~keymap
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
  (  "C-c n"        (ilambda
                     (require 'ox-md)
                     (save-excursion (org-publish "notes")))))

(defkey mmm~keymap
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
  (  "C-h S"        counsel-info-lookup-symbol))


;; when occur, word under cusor first
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




;;; view-mode
(add-hook-lambda view-mode-hook
  (defkey view-mode-map
    (  "h"     backward-char  )
    (  "l"     forward-char   )
    (  "j"     next-line      )
    (  "k"     previous-line  )
    (  "C-f"   forward-sexp   )
    (  "C-b"   backward-sexp  )))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; variables
(let* ((notes-home "~/.showcase/notes/")
       (base/res "assets/") (base/out "html/")
       (download/image (concat base/res "image/"))
       (html/css (concat base/res "base.css"))
       (html/js  (concat base/res "base.js"))
       (base/res/out (concat base/out base/res)))
  
  (setq org-directory             (or (getenv "NOTES_HOME") notes-home)
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
        org-cycle-separator-lines            0
        org-src-fontify-natively             t
        org-src-tab-acts-natively            nil

        org-export-preserve-breaks           nil
        org-export-copy-to-kill-ring         nil
        org-export-with-section-numbers      t
        org-export-with-sub-superscripts     nil
        org-publish-list-skipped-files       nil

        org-html-html5-fancy                 t
        org-html-doctype                     "html5"
        org-html-container-element           "section"
        org-html-validation-link             "Go ahead, never stop."
        org-html-htmlize-output-type         'css
        org-html-head-include-scripts        nil
        org-html-head-include-default-style  nil
        org-html-head  (format "<meta name='viewport' content='width=device-width,initial-scale=1'>\n<link rel='stylesheet' href='%s'>\n<script src='%s'></script>\n" html/css html/js))

  (setq org-publish-project-alist
        `(("notes" :components ("resources" "org-files"))
          ("org-files"
           :base-directory       ,org-directory
           :publishing-directory ,(concat org-directory base/out)
           :headline-levels      3
           :with-toc             3
           :html-preamble        nil
           :auto-sitemap         t
           :sitemap-filename     "index.org"
           :sitemap-title        ,(if (with-classroom) "List Of Contents" "IMFINEANDU")
           :html-link-up         "index.html"
           :html-link-home       "index.html"
           :publishing-function  org-html-publish-to-html
           :recursive            t)
          ("resources"
           :base-directory       ,(concat org-directory base/res)
           :publishing-directory ,(concat org-directory base/res/out)
           :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg"
           :publishing-function  org-publish-attachment
           :recursive t)))
  
  (setq org-startup-with-inline-images       t
        org-download-image-dir               download/image
        org-download-backend                 (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t)))

(defun org-download--dir-advice (f &rest args)
  (or (file-name-base (buffer-file-name)) ""))
(advice-add 'org-download--dir-2 :around #'org-download--dir-advice)

(setf org-confirm-babel-evaluate nil
      org-babel-load-languages  '((emacs-lisp . t) (python . t) (ruby . t) (sh . t)
                                  (sql . t) (lisp . t) (haskell . t) (java . t) (dot . t)))

(setq org-capture-templates
      `(("t" "New Task" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
        ("n" "Note Now" entry (file+headline ,org-default-notes-file "Quick Notes") "* %i%?" :prepend t)
        ("d" "Qian Cao" entry (file+datetree ,org-default-notes-file) "* %U %^g\n%i%?" :empty-lines 1)))

(with-eval-after-load 'org
  ;; org-download
  (require 'org-download)
  ;; fix for dot-mode
  (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot)
  ;; remaps
  (define-key org-mode-map (kbd "×") (kbd "*"))
  (define-key org-mode-map (kbd "－") (kbd "-")))

(add-hook-lambda org-mode-hook
  ;; Time English Style
  (set (make-local-variable 'system-time-locale) "C"))






(provide 'imokeys)
;;; imokeys.el ends here
