;;; imface.el --- Faces And Encoding, ETC.
;;; Commentary:

;;; Code:

;;; Windows

(env-windows

 (defun im/win-font (&optional font-size)
   (interactive (list (string-to-number (read-from-minibuffer "font size: " "1"))))
   (let* ((size (or font-size 14))
          (en "Consolas") (zh "微软雅黑 light") (tz "楷体"))
     (set-frame-font (font-spec :name (im/find-ft en) :size size) t)
     (set-fontset-font "fontset-default" 'unicode (font-spec :name (im/find-ft zh)))
     (set-face-attribute 'mode-line nil :height 110)
     (add-to-list 'face-font-rescale-alist '(("Consolas"  1)))
     (create-fontset-from-fontset-spec (format "-*-%s-normal-r-normal-*-%d-*-*-*-c-*-fontset-table,
                                        unicode:-*-%s-normal-r-normal-*-%d-*-*-*-c-*-iso8859-1"
                                               en (pcase font-size (14 13) (30 33) (_ size))
                                               tz (pcase font-size (14 14) (30 35) (_ (* size 1.2)))))))

 (setq default-frame-alist
       '((title . "TimKeeper")
         (top . 30) (left . 640)
         (width . 85) (height . 40)
         (line-spacing . 0.11)
         (tool-bar-lines . 0)
         (scroll-bar . nil)
         (vertical-scroll-bars . nil)
         (cursor-type  . box)
         (cursor-color . "red")
         (alpha . 92)))

 (setq file-name-coding-system 'gbk
       default-process-coding-system '(cp936-dos . utf-8-unix)
       mouse-wheel-scroll-amount '(1 ((control) . 5)))

 (menu-bar-mode -1)
 (global-set-key [C-wheel-up]   'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease)

 (when (not (env-classroom))
   (add-hook 'focus-in-hook 'im/win-font))

 (when (env-classroom)
   (setf (alist-get 'height default-frame-alist) '35)
   (setf (alist-get 'width default-frame-alist)  '70)
   (setf (alist-get 'left default-frame-alist)   '850)
   (add-hook 'focus-in-hook (lambda () (im/win-font 30)))))

;;; Linux

(env-linux-g
 (tool-bar-mode 0) (menu-bar-mode 0)
 (set-face-attribute 'default nil :height 110))

(env-linux-vps
 (load-theme 'origin t)
 (menu-bar-mode 0) (xterm-mouse-mode)
 (global-set-key [mouse-4] (lambdai (scroll-down 1)))
 (global-set-key [mouse-5] (lambdai (scroll-up 1))))

;;; Encoding

(set-locale-environment   "utf-8")
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)

;;; Miscellaneous

(setq sentence-end-double-space nil)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table)))
      '( ?， ?。 ?！ ?； ?？ ?： ?/ ))


(provide 'imface)

;;; imface.el ends here
