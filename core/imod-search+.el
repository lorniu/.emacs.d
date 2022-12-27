;;; imod-search+.el --- Search and replace -*- lexical-binding: t -*-

;;; Code:

(x isearch
   :config
   (setq isearch-lazy-count t) ; replace Anzu
   (setq isearch-allow-motion t)
   (define-key isearch-mode-map
               (kbd "\C-e") ; Superword-Mode when select
               (lambda ()
                 (interactive)
                 (superword-mode 1)
                 (unwind-protect
                     (call-interactively 'isearch-yank-word-or-char)
                   (superword-mode -1)))))

(x replace
   :config
   (global-set-key (kbd "M-s o") ; Word at-point first
                   (defun im/occur (str)
                     (interactive (list (im-thing-at-region-or-point)))
                     (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))
   (global-set-key (kbd "M-s O") ; use 'rg' as backend
                   (defun moccur (str)
                     (interactive (list (im-thing-at-region-or-point)))
                     (rg (read-from-minibuffer "Multi-Occurs: " str nil nil 'regexp-history)
                         (rg-read-files)
                         (read-directory-name "In directory: " nil default-directory t))
                     (aif (get-buffer-window "*rg*") (select-window it))))
   (defun:after occur$follow (&rest _) (aif (get-buffer-window "*Occur*") (select-window it))))

(x rg
   "pacman -S ripgrep"
   :ref ("dajva/rg.el"
         "ripgrep download page: BurntSushi/ripgrep/releases")
   :init
   (setq rg-group-result nil)
   (setq rg-ignore-case 'smart)
   (setq rg-align-column-number-field-length 4)

   :config
   (defun rg-rerun-toggle-group ()
     "Toggle show group in `rg-cur-search`."
     (interactive)
     (setq rg-group-result (not rg-group-result))
     (rg-rerun))

   (define-key rg-mode-map "G" 'rg-rerun-toggle-group))

(provide 'imod-search+)

;;; imod-search+.el ends here
