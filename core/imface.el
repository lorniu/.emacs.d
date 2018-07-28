;;; imface.el --- Faces And Encoding, ETC.
;;; Commentary:

;;; Code:

;; pacman -S ttf-ubuntu-font-family
(defvar im/mono-height 108)
(setq im/probe-mono-fonts '("Ubuntu Mono" "Courier New"))


;;; Windows

(env-windows

 (defun im/win-font (&optional font-size)
   (interactive (list (string-to-number
                       (read-from-minibuffer "font size: " "1"))))
   (let* ((size (or font-size 14)) (en "Consolas") (zh "微软雅黑 light"))
     (set-frame-font (font-spec :name (im/find-ft en) :size size) t)
     (set-fontset-font "fontset-default" 'unicode (font-spec :name (im/find-ft zh)))
     (add-to-list 'face-font-rescale-alist '(("Consolas"  1)))))

 (setq default-frame-alist
       '((title . "νερό")
         (top . 30) (left . 640)
         (width . 85) (height . 40)
         ;; (line-spacing . 0.11)
         (tool-bar-lines . 0)
         (scroll-bar . nil)
         (vertical-scroll-bars . nil)
         (cursor-type  . box)
         (cursor-color . "red")
         (alpha . 92)))

 (menu-bar-mode -1)
 (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
 (global-set-key [C-wheel-up]   'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Linux

(env-linux
 (menu-bar-mode 0))

(env-linux-ng
 (load-theme 'origin t)
 (xterm-mouse-mode)
 (global-set-key [mouse-4] (lambdai (scroll-down 1)))
 (global-set-key [mouse-5] (lambdai (scroll-up 1))))

(env-linux-g
 (tool-bar-mode 0)
 (scroll-bar-mode 0)
 ;; key
 (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
 (global-set-key [C-mouse-4] 'text-scale-increase)
 (global-set-key [C-mouse-5] 'text-scale-decrease)
 ;; font
 (setq default-frame-alist '((height . 110) (alpha . 100)))
 (set-fontset-font "fontset-default" 'unicode "Source Han Sans CN")
 )


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
