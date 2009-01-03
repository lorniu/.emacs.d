
;;; packages initialize
(setq package-user-dir "~/.emacs.d/packages")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(let ((packages-required
       '(anzu iedit origami session sqlplus
              rainbow-delimiters diminish rcirc-styles spacemacs-theme
              company yasnippet counsel-projectile magit org-download
              web-mode js2-mode emmet-mode htmlize yaml-mode sass-mode
              slime php-mode groovy-mode haskell-mode robe elpy c-eldoc erlang lua-mode
              xcscope)))
  (unless package-archive-contents (package-refresh-contents))
  (mapc 'package-install (delete-if #'package-installed-p packages-required)))



;; load-path/theme-path
(dolist (dir (directory-files "~/.emacs.d/ext" t))
  (if (and (not (eq (file-name-extension dir) ""))
           (file-directory-p dir))
      (add-to-list 'load-path dir)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/ext/themes")



;;; basic variables
(setq user-full-name           "imfine"
      user-mail-address        "lorniu@gmail.com"
      bbdb-file                "~/.emacs.d/.cache/_bbdb"
      diary-file               "~/.emacs.d/.cache/_diary"
      bookmark-default-file    "~/.emacs.d/.cache/_bookmark"
      abbrev-file-name         "~/.emacs.d/.cache/_abbrevs"
      recentf-save-file        "~/.emacs.d/.cache/_recentf"
      eshell-directory-name    "~/.emacs.d/.cache/eshell"
      eshell-aliases-file      "~/.emacs.d/ext/eshell-alias"
      custom-file              "~/.emacs.d/core/cust.el"
      server-auth-dir          "~/.emacs.d/.cache/server/"
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

      sentence-end-double-space nil
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"

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
(setq default-directory "~/")



;;; syntax table
(dolist (c '(?， ?。 ?！ ?； ?？ ?： ?/))
  (modify-syntax-entry c "'" (standard-syntax-table)))


;;; coding
(set-locale-environment   "utf-8")
(set-language-environment 'utf-8)
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)



;;; enable
(put 'narrow-to-region    'disabled nil)
(put 'narrow-to-page      'disabled nil)
(put 'downcase-region     'disabled nil)
(put 'upcase-region       'disabled nil)
(put 'set-goal-column     'disabled nil)
(put 'erase-buffer        'disabled nil)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-html-infojs-options
   (quote
    ((path . "iii.js")
     (view . "info")
     (toc . :with-toc)
     (ftoc . "0")
     (tdepth . "max")
     (sdepth . "max")
     (mouse . "underline")
     (buttons . "0")
     (ltoc . "1")
     (up . :html-link-up)
     (home . :html-link-home))))
 '(package-selected-packages
   (quote
    (lua-mode sqlplus xcscope c-eldoc elpy robe haskell-mode groovy-mode php-mode slime sass-mode yaml-mode htmlize emmet-mode js2-mode web-mode org-download magit counsel-projectile yasnippet company spacemacs-theme rcirc-styles diminish rainbow-delimiters session origami iedit anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defface hi-org-break
  `((t (:foreground ,(pcase system-type
                       ('gnu/linux "#222222")
                       ('windows-nt "#eeeeee"))))) "for org mode \\ break")



;; init-benchmark
(defun display-startup-echo-area-message ()
  (message ">> 加载完成，耗时 %.2f 秒." (float-time (time-subtract after-init-time before-init-time))))




(provide 'cust)
;;; cust.el ends here
