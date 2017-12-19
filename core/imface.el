;;;==============================
;;;   faces | encoding | etc
;;;==============================


;;;;;;;;;;;;;;;;;;;;
(defun im/win-font (font-size)
  "font config on windows"
  (interactive (list (string-to-number (read-from-minibuffer "font size: " "1"))))
  (let* ((en "Consolas") (zh "微软雅黑 light") (tz "楷体"))
    ;; default
    (set-frame-font (font-spec :name (im/find-ft en) :size font-size) t)
    (set-fontset-font "fontset-default" 'unicode (font-spec :name (im/find-ft zh)))
    ;; mode-line
    (set-face-attribute 'mode-line nil :height 110)
    ;; rescale
    (add-to-list 'face-font-rescale-alist '(("Consolas"  1)))
    ;; new fontset
    (create-fontset-from-fontset-spec
     (format "-*-%s-normal-r-normal-*-%d-*-*-*-c-*-fontset-table,
         unicode:-*-%s-normal-r-normal-*-%d-*-*-*-c-*-iso8859-1"
             en (pcase font-size (14 13) (30 33) (_ font-size))
             tz (pcase font-size (14 14) (30 35) (_ (* font-size 1.2)))))))

(defun im/win-font-wrapper (&optional font-size) ;; hack...
  (im/win-font (or font-size 14)))



;;;;;;;;;;;;;;;;;;;;
(with-windows
 (setq file-name-coding-system 'gbk
       default-process-coding-system '(cp936-dos . utf-8-unix)
       mouse-wheel-scroll-amount '(1 ((control) . 5))
       default-frame-alist '((title . "TimKeeper") (width . 100) (height . 45) (top . 60) (left . 540) (line-spacing . 0.11)
                             (scroll-bar . nil) (tool-bar-lines . 0) (menu-bar-lines . 0) (vertical-scroll-bars . nil)
                             (cursor-type . box) (cursor-color . "red") (alpha . 92)))
 ;; mouse control scale
 (global-set-key [C-wheel-up]   'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease))

;;;;;;;;;;;;;;;;;;;;
(when (and (with-windows) (not (with-classroom)))
  ;;(load-theme 'spacemacs-dark t)
  (add-hook 'focus-in-hook #'im/win-font-wrapper))

;;;;;;;;;;;;;;;;;;;;
(with-classroom
 (setf (alist-get 'height default-frame-alist) '35)
 (setf (alist-get 'width default-frame-alist)  '70)
 (setf (alist-get 'left default-frame-alist)   '850)
 (add-hook 'focus-in-hook (lambda () (im/win-font-wrapper 30))))

;;;;;;;;;;;;;;;;;;;;
(with-linux-g
 (tool-bar-mode 0) (menu-bar-mode 0)
 (set-face-attribute 'default nil :height 110))

;;;;;;;;;;;;;;;;;;;;
(with-linux-vps
 (load-theme 'origin t)
 (menu-bar-mode 0) (xterm-mouse-mode)
 (global-set-key [mouse-4] (ilambda (scroll-down 1)))
 (global-set-key [mouse-5] (ilambda (scroll-up 1))))




;;;
;;; encoding
;;;
(set-locale-environment   "utf-8")
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)




;;;
;;; syntax and so on
;;;
;; sentence
(setq sentence-end-double-space nil
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; syntax table
(dolist (c '(?， ?。 ?！ ?； ?？ ?： ?/))
  (modify-syntax-entry c "." (standard-syntax-table)))





(provide 'imface)

;;; file ends here
