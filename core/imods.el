;;; imods.el --- Modules -*- lexical-binding: t -*-

;;; Code:

(x paren
   :init
   (show-paren-mode +1))

(x electric
   :init
   (defun im:inhibit-electric-pair (c)
     (or
      (char-equal c ?\<) ; don't complete <
      (and (equal major-mode 'org-mode) ; only src-block in org
           (if (org-in-src-block-p)
               (not (member (car (org-babel-get-src-block-info)) '("csharp" "cs" "php")))
             t))
      (electric-pair-conservative-inhibit c)))
   (setq electric-pair-inhibit-predicate 'im:inhibit-electric-pair))

(x autorevert
   :init
   (setq auto-revert-mode-text "")
   (global-auto-revert-mode 1))

(x ibuffer/e
   :config
   (setq ibuffer-show-empty-filter-groups nil
         ibuffer-saved-filter-groups
         `(("default"
            ("apple"
             (or (predicate . (eq (get major-mode 'derived-mode-parent) 'prog-mode))
                 (predicate . (member major-mode '(nxml-mode sgml-mode)))))
            ("banana"
             (or (mode . org-mode)
                 (mode . tar-mode)))
            ("orange"
             (or (mode . erc-mode)
                 (mode . rcirc-mode)))
            ("plum"
             (or (name . "^\\*dape")))
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
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (rcirc-mode . flyspell-mode))
   :config (ispell-change-dictionary "american" t))

(x xref/e)

(x which-func)

(x repeat
   :if (>= emacs-major-version 29)
   :init
   (repeat-mode 1))

(x pcre2el
   :ref "joddie/pcre2el")



(x alert
   :commands (alert)
   :config
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
   :config
   (nconc page-break-lines-modes '(web-mode css-mode conf-mode powershell-mode prog-mode)))

(x all-the-icons
   "M-x all-the-icons-install-fonts."
   :ref "domtronn/all-the-icons.el")

(x ediff
   :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))

(x view
   :bind
   ( :map view-mode-map
	 ( "h"       .  backward-char )
	 ( "l"       .  forward-char  )
	 ( "j"       .  next-line     )
	 ( "k"       .  previous-line )
	 ( "%"       .  im/match-paren )
	 ( "<DEL>"   .  nil ))
   :init
   (defun:hook edebug-mode-hook () (view-mode -1)))

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

(provide 'imods)

;;; imods.el ends here
