;;; imod-general.el --- Modules -*- lexical-binding: t -*-

;;; Code:

(x desktop
   :init
   (defvar desktop-save-default-dir (locc))
   (setq desktop-path `(,(locc) ".")))

(x winner
   :init (winner-mode 1)
   :defer-config
   (define-key winner-mode-map [C-left] 'winner-undo)
   (define-key winner-mode-map [C-right] 'winner-redo))

(x electric
   :init
   (defun my-inhibit-electric-pair (c)
     (or
      (char-equal c ?\<) ; don't complete <
      (and (equal major-mode 'org-mode) ; only src-block in org
           (if (org-in-src-block-p)
               (not (member (car (org-babel-get-src-block-info)) '("csharp" "cs" "php")))
             t))
      (electric-pair-conservative-inhibit c)))
   (setq electric-pair-inhibit-predicate 'my-inhibit-electric-pair))

(x paren
   :init
   (show-paren-mode +1))

(x autorevert
   :init
   (setq auto-revert-mode-text "")
   (global-auto-revert-mode 1))

(x savehist
   :init
   (savehist-mode +1))

(x recentf
   :init
   (setq recentf-max-saved-items 200)
   (setq recentf-exclude '(package-user-dir
                           (expand-file-name package-user-dir)
                           "-autoloads.el$"
                           ".cache"
                           ".cask"
                           "bookmarks"
                           "cache"
                           "ido.*"
                           "persp-confs"
                           "recentf"
                           "undo-tree-hist"
                           "url"
                           "COMMIT_EDITMSG\\'"))

   (defun im/recentf-clear (&optional regexp)
     (interactive (list (read-string "Regexp for flushing from recentf: ")))
     (if (zerop (length regexp))
         (setq recentf-list nil)
       (cl-delete-if (lambda (f) (string-match-p regexp f)) recentf-list))
     (recentf-save-list))

   (recentf-mode 1))

(x ibuffer/e
   :defer-config
   (setq ibuffer-show-empty-filter-groups nil
         ibuffer-saved-filter-groups
         `(("default"
            ("apple"
             (or (predicate . (eq (get major-mode 'derived-mode-parent) 'prog-mode))
                 (predicate . (member major-mode '(nxml-mode sgml-mode)))))
            ("plum"
             (or (mode . eaf-mode)))
            ("banana"
             (or (mode . org-mode)
                 (mode . tar-mode)))
            ("orange"
             (or (mode . erc-mode)
                 (mode . rcirc-mode)))
            ("melon"
             (or (name . "^\\*?magit")))
            ("peach"
             (or (name . "\\*.+\\*")))
            ("durian"
             (or (mode . dired-mode))))))

   (defun:hook ibuffer-mode-hook ()
     (ibuffer-switch-to-saved-filter-groups "default")))

(x flyspell/x
   :preface (setq ispell-program-name "ispell") ;; apt install ispell
   :if (executable-find ispell-program-name)
   :hook ((text-mode-hook . flyspell-mode)
          (prog-mode-hook . flyspell-prog-mode)
          (rcirc-mode-hook . flyspell-mode))
   :defer-config (ispell-change-dictionary "american" t))

(x calc
   :ref "info: https://www.gnu.org/software/emacs/manual/html_node/calc/index.html"
   :init
   ;; Take region content as initial input of `quick-calc'.
   (defvar quick-calc-init-string nil)
   (defun:around calc-do-quick-calc$quick-calc-with-region (f &rest args)
     (let ((quick-calc-init-string (im-thing-at-region-or-point "")))
       (apply f args)))
   (defun:around calc-do-alg-entry$quick-calc-with-region (f &rest args)
     (when (and (string= (car args) "")
                (string= (cadr args) "Quick calc: "))
       (setf (car args) quick-calc-init-string))
     (apply f args)))

(x xref/e)

(x which-func)

(x repeat
   :emacs> 29
   :init
   (repeat-mode 1))



(x ace-window
   "Can `M-x windmove-swap-states-default-keybindings' then use S-M-arrow to swap."
   :bind ("C-x w" . ace-window)
   :defer-config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(x alert
   :commands (alert)
   :defer-config
   (setq alert-default-fade-time 8)
   (setq alert-default-style (cond ((executable-find "dunstify") 'dunstify)
                                   ((executable-find "notify-send") 'libnotify)
                                   ((executable-find "powershell") 'powershell)
                                   ((executable-find "growlnotify") 'growl)
                                   ((executable-find "terminal-notifier") 'notifier))))

(x page-break-lines
   :init
   (setq page-break-lines-lighter "")
   (global-page-break-lines-mode 1)
   :defer-config
   (nconc page-break-lines-modes '(web-mode css-mode conf-mode powershell-mode prog-mode)))

(x all-the-icons/i
   "M-x all-the-icons-install-fonts."
   :ref "domtronn/all-the-icons.el")

(x ediff
   :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))

(x view
   :bind
   ((view-mode-map
     ( "h"       .  backward-char )
     ( "l"       .  forward-char  )
     ( "j"       .  next-line     )
     ( "k"       .  previous-line )
     ( "%"       .  his-match-paren )
     ( "<DEL>"   .  nil )))
   :init
   (defun:hook edebug-mode-hook ()
     (view-mode -1)))

(x so-long
   :init
   (when (fboundp 'global-so-long-mode)
     (global-so-long-mode)))

(x iy-go-to-char
   :commands (iy-go-to-char
              iy-go-to-char-backward))



;;  ________
;; < cowsay >
;;  --------
;;         \   ^__^
;;          \  (oo)\_______
;;             (__)\       )\/\
;;                 ||----w |
;;                 ||     ||
(x cowsay
   :ref "lassik/emacs-cowsay")

;;   _
;; _|_o _ | __|_
;;  | |(_||(/_|_
;;      _|
(x figlet
   :ref "http://www.figlet.org/"
   :commands (figlet figlet-samples))

(provide 'imod-general)

;;; imod-general.el ends here
