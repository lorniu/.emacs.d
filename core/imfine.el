;;; This is my personal emacs configuration.

;; Copyright 2008 by imfine. All rights reserved.

;; Author: lorniu@gmail.com
;; Version: 0.01
;; License: GPLv3

;;; Code:

(require 'cl)
(require 'cust)
(require 'imutil)
(require 'immor)


(defcustom mmm/modes-to-load
  '(global-origami-mode
    beacon-mode ivy-mode global-anzu-mode
    yas-global-mode show-paren-mode
    global-auto-revert-mode projectile-mode)
  "the modes should be enabled")

(defcustom mmm/list-to-dim
  '((yas-minor-mode " Y")
    ivy-mode auto-revert-mode abbrev-mode beacon-mode beacon-mode
    projectile-mode company-mode anzu-mode hs-minor-mode )
  "dim lighter of minor modes")

(defcustom mmm/keywords-to-add
  '((org-mode-hook
     ("\\<\\(FIXME\\|NOTE\\):" 1 'font-lock-warning-face prepend)
     ("\\\\\\\\$"              0 'hi-org-break)))
  "define keywords for current mode")

(defvar mmm~keymap (make-sparse-keymap))

(define-minor-mode mmm~mode "The Main Mode of This Configuration"
  :global t :keymap mmm~keymap
  (if (not mmm~mode)
      (mmm/load-modes -1)
    (mmm/load-modes)
    (mmm/diminish)
    (mmm/add-keywords)
    (add-hook 'after-save-hook 'im/el-autocompile)))



;;; for windows
(when (eq system-type 'windows-nt)

  (setq default-frame-alist
        '((top . 30) (left . 780) (height . 38) (width . 75)
          (alpha . 95) (cursor-type . bar)
          (line-spacing . 1) (tool-bar-lines . 0) (menu-bar-lines . 20 )))

  (setq global-hl-line-mode   t
        mouse-wheel-scroll-amount '(1 ((control) . 5))
        default-process-coding-system '(cp936-dos . utf-8-unix))

  (global-set-key [C-wheel-up]   'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease)

  (let ((e (find-if 'x-list-fonts '("Source Code Pro" "Monaco" "Consolas" "Courier New"))))
    (set-frame-font (font-spec :family e :size 16 :weight 'normal) t))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "微软雅黑 Light" :weight 'extra-light)))
  (setq face-font-rescale-alist '(("微软雅黑 Light" . 1) ("SimSun" . 2)))

  
  (cond
   ((string= user-login-name "Administrator")
    (setf (alist-get 'fullscreen default-frame-alist) 'maximized)
    (set-face-attribute 'default nil :height 120))
   
   (t (load-theme 'spacemacs-dark t))))



;;; for linux
(when (eq system-type 'gnu/linux)
  
  (when (display-graphic-p)
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (set-face-attribute 'default nil :height 110))
  
  (cond
   ((string= (system-name) "remote")
    (load-theme 'origin t)
    (menu-bar-mode 0)
    (xterm-mouse-mode)  ;; enable mouse
    (global-set-key [mouse-4] (ilambda (scroll-down 1)))
    (global-set-key [mouse-5] (ilambda (scroll-up 1))))))




(require 'imokeys)
(mmm~mode 1)



(require 'server)
(if (null (server-running-p)) (server-start))



(provide 'imfine)
;;; file ends here
