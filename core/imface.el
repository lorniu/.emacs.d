;;; imface.el --- Faces And Encoding, ETC.
;;; Commentary:

;;; Code:

(defvar im/mono-buffer-height 108)
(defvar im/probe-mono-fonts '("Ubuntu Mono" "隶书"))  ;; pacman -S ttf-ubuntu-font-family

(defvar im/win-font-cn "微软雅黑 light")
(defvar im/win-frame-alist '(30 640 85 35 100))  ;; top:left:width:height:alpha

(defvar im/nix-font-cn "Source Han Sans CN")
(defvar im/nix-frame-alist '(110 100))           ;; height:alpha


;;; Windows

(env-windows
 (menu-bar-mode -1)
 ;; font
 (setq default-frame-alist
       `((title  . "νερό")
         (top    . ,(nth 0 im/win-frame-alist))
         (left   . ,(nth 1 im/win-frame-alist))
         (width  . ,(nth 2 im/win-frame-alist))
         (height . ,(nth 3 im/win-frame-alist))
         (alpha  . ,(nth 4 im/win-frame-alist))
         (tool-bar-lines . 0)
         (scroll-bar . nil)
         (vertical-scroll-bars . nil)
         (cursor-type  . box)
         (cursor-color . "red")))
 (set-fontset-font "fontset-default" 'unicode im/win-font-cn)
 (setq face-font-rescale-alist '(("simsun" . 1) ("隶书" . 1)))
 ;; key
 (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
 (global-set-key [C-wheel-up]   'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Linux

(env-linux-ng
 (load-theme 'origin t)
 (menu-bar-mode 0)
 (xterm-mouse-mode)
 (global-set-key [mouse-4] (lambdai (scroll-down 1)))
 (global-set-key [mouse-5] (lambdai (scroll-up 1))))

(env-linux-g
 (menu-bar-mode 0)
 (tool-bar-mode 0)
 (scroll-bar-mode 0)
 ;; font
 (setq default-frame-alist
       `((height . ,(nth 0 im/nix-frame-alist))
         (alpha  . ,(nth 1 im/nix-frame-alist))))
 (set-fontset-font "fontset-default" 'unicode im/nix-font-cn)
 ;; key
 (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
 (global-set-key [C-mouse-4] 'text-scale-increase)
 (global-set-key [C-mouse-5] 'text-scale-decrease))


;;; Encoding

(set-locale-environment   "utf-8")
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)

(env-windows
 ;; generic encoding
 (set-language-environment "chinese-gbk")
 (prefer-coding-system 'utf-8)

 ;; specified encoding
 (setq file-name-coding-system 'gbk)
 (set-terminal-coding-system 'gbk)
 (modify-coding-system-alist 'process "*" 'gbk)

 (defun im/cp936-encoding ()
   (set-buffer-file-coding-system 'gbk)
   (set-buffer-process-coding-system 'gbk 'gbk))

 (add-hook 'shell-mode-hook 'im/cp936-encoding))


;;; Miscellaneous

(setq-default
 mode-line-format
 '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification
   mode-line-buffer-identification
   "   " mode-line-position
   mode-line-modes mode-line-misc-info mode-line-end-spaces (vc-mode vc-mode)))

(setq sentence-end-double-space nil)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table)))
      '( ?， ?。 ?！ ?； ?？ ?： ?/ ))

(provide 'imface)

;;; imface.el ends here
