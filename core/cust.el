;;;
;;; packages initialize
;;;
(setq package-user-dir "~/.emacs.d/packages")
(setq package-archives
      '( ("melpa"    . "http://melpa.org/packages/")
         ;("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
         ("org-cn"   . "http://elpa.emacs-china.org/org/")
         ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")) )
(package-initialize)
(let ((packages-required
       '(ag anzu iedit origami session hideshowvis ssh
            rainbow-delimiters diminish rcirc-styles spacemacs-theme
            yasnippet counsel-projectile magit org-download
            web-mode emmet-mode htmlize js2-mode yaml-mode sass-mode sqlplus
            slime php-mode haskell-mode robe elpy c-eldoc erlang lua-mode go-mode
            kotlin-mode scala-mode clojure-mode groovy-mode
            company tern company-tern company-ghc company-php
            graphviz-dot-mode xcscope)))
  (unless package-archive-contents (package-refresh-contents))
  (mapc 'package-install (delete-if #'package-installed-p packages-required)))



;;;
;;; customs
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ssh ag xcscope graphviz-dot-mode company-ghc company-tern tern company groovy-mode clojure-mode scala-mode kotlin-mode go-mode lua-mode erlang c-eldoc elpy robe yasnippet yaml-mode web-mode sqlplus spacemacs-theme slime session sass-mode rcirc-styles rainbow-delimiters php-mode origami org-download magit js2-mode inf-ruby iedit htmlize hideshowvis haskell-mode emmet-mode diminish counsel-projectile anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;; load-path and theme-path
(dolist (dir (directory-files "~/.emacs.d/ext" t))
  (if (and (not (eq (file-name-extension dir) ""))
           (file-directory-p dir))
      (add-to-list 'load-path dir)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/ext/themes")



;;; basic variables
(setq default-directory        "~/"
      user-full-name           "imfine"
      user-mail-address        "lorniu@gmail.com"
      bbdb-file                "~/.emacs.d/.cache/_bbdb"
      diary-file               "~/.emacs.d/.cache/_diary"
      bookmark-default-file    "~/.emacs.d/.cache/_bookmark"
      abbrev-file-name         "~/.emacs.d/.cache/_abbrevs"
      recentf-save-file        "~/.emacs.d/.cache/_recentf"
      eshell-directory-name    "~/.emacs.d/.cache/eshell"
      eshell-aliases-file      "~/.emacs.d/ext/eshell-alias"
      custom-file              "~/.emacs.d/core/cust.el"
      temporary-file-directory "~/.emacs.d/.cache/temp/"
      org-publish-timestamp-directory "~/.emacs.d/.cache/.org-timestamps/"

      auto-save-list-file-prefix     nil
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq debug-on-error           nil
      inhibit-startup-message  t
      gnus-inhibit-startup-message t
      track-eol                t
      visible-bell             nil
      ring-bell-function       nil
      
      scroll-step              1
      scroll-margin            0
      hscroll-step             1
      hscroll-margin           1
      scroll-conservatively    101

      gc-cons-threshold        100000000)

(setq resize-mini-windows      t
      enable-recursive-minibuffers t
      column-number-mode       1

      kill-ring-max            200
      select-enable-clipboard  t
      help-window-select       t
      man-notify-method        'pushy
      woman-use-own-frame      nil  )

(setq-default tab-width          4 )
(setq-default indent-tabs-mode nil )
(setq-default truncate-lines     t )
(setq-default show-trailing-whitespace nil)

(setenv "TZ" "PRC")
(fset 'yes-or-no-p 'y-or-n-p)



;;; enable
(put 'narrow-to-region    'disabled nil)
(put 'narrow-to-page      'disabled nil)
(put 'downcase-region     'disabled nil)
(put 'upcase-region       'disabled nil)
(put 'set-goal-column     'disabled nil)
(put 'erase-buffer        'disabled nil)



;; init-benchmark
(defun display-startup-echo-area-message ()
  (message ">> 加载完成，耗时 %.2f 秒." (float-time (time-subtract after-init-time before-init-time))))





(provide 'cust)

;;; cust.el ends here
