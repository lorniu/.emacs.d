;;; -*- lexical-binding: t -*-

;;; Code:

(xzz paren
  :init (show-paren-mode 1))

(xzz electric
  :config
  (defun im:inhibit-electric-pair (c)
    (or
     (char-equal c ?\<) ; don't complete <
     (and (equal major-mode 'org-mode) ; only src-block in org
          (if (org-in-src-block-p)
              (not (member (car (org-babel-get-src-block-info)) '("csharp" "cs" "php")))
            t))
     (electric-pair-conservative-inhibit c)))
  (setopt electric-pair-inhibit-predicate 'im:inhibit-electric-pair))

(xzz autorevert
  :init (global-auto-revert-mode 1)
  :config
  (setopt auto-revert-mode-text ""))

(xzz ibuffer/e
  :config
  (setopt ibuffer-show-empty-filter-groups nil
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

(xzz flyspell
  :preface (setq ispell-program-name "ispell") ;; apt install ispell
  :if (executable-find ispell-program-name)
  :hook ((rcirc-mode . flyspell-mode)
         (flyspell-mode . (lambda ()
                            (keymap-unset flyspell-mode-map "C-,")
                            (keymap-unset flyspell-mode-map "C-."))))
  :config (ispell-change-dictionary "american" t))

(xzz repeat
  :init (im:with-message nil (repeat-mode 1)))

(xzz view
  :bind ( :map view-mode-map
	      ( "h"       .  backward-char )
	      ( "l"       .  forward-char  )
	      ( "j"       .  next-line     )
	      ( "k"       .  previous-line )
	      ( "%"       .  im/match-paren )
	      ( "<DEL>"   .  nil )))

(xzz page-break-lines
  :init (global-page-break-lines-mode 1)
  :config
  (setopt page-break-lines-lighter ""
          page-break-lines-modes (append '(web-mode css-mode conf-mode powershell-mode prog-mode emacs-news-mode)
                                         page-break-lines-modes)))

(xzz trashed
  :ref "shingo256/trashed")



(xzz so-long
  :init (global-so-long-mode 1))

(xzz vlf
  "Open huge file, Part by Part."
  :ref "m00natic/vlfi"
  :init
  (require 'vlf-setup)
  (add-to-list 'vlf-forbidden-modes-list 'nov-mode)
  (add-to-list 'vlf-forbidden-modes-list 'doc-view-mode-maybe)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))



(xzz alert
  :commands (alert)
  :config
  (setopt alert-default-fade-time 8
          alert-default-style
          (cond (IS-MAC 'osx-notifier)
                ((executable-find "dunstify") 'dunstify)
                ((executable-find "notify-send") 'libnotify)
                ((executable-find "powershell") 'powershell)
                ((executable-find "growlnotify") 'growl)
                ((executable-find "terminal-notifier") 'notifier))))
