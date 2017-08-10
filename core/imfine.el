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


;;;
;;; environments definition
(define-environments
  ((with-windows    . (eq system-type 'windows-nt))
   (with-classroom  . (string= user-login-name "lol"))
   (with-linux      . (eq system-type 'gnu/linux))
   (with-linux-g    . (and (eq system-type 'gnu/linux) (display-graphic-p)))
   (with-debian-vps . (string= (system-name) "remote"))))


;;;
;;; different environments
;;;
(with-windows   ;; for windows
 (setq global-hl-line-mode   t
       mouse-wheel-scroll-amount '(1 ((control) . 5))
       default-process-coding-system '(cp936-dos . utf-8-unix))

 (global-set-key [C-wheel-up]   'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease)

 (setq default-frame-alist
       '((width . 90) (height . 35) (top . 35) (left . 320)
         (alpha . 95) (cursor-type . bar)
         (line-spacing . 1) (tool-bar-lines . 0) (menu-bar-lines . 20 ))))

(defun im/config-font (font-size) ;; font config
  (interactive (list (cl-parse-integer (read-from-minibuffer "font size: "))))
  (let* ((en '("Source Code Pro" "Monaco" "Consolas" "Courier New"))
         (zh (font-spec :family "微软雅黑 Light" :weight 'extra-light)))
    (set-frame-font (font-spec :family (find-if 'x-list-fonts en) :size font-size) t)
    (set-fontset-font "fontset-default" 'unicode zh)
    (setq face-font-rescale-alist '(("微软雅黑 Light" . 1)))))

(with-classroom          ;; classroom environment
 (setf (alist-get 'height default-frame-alist) '23)
 (setf (alist-get 'width default-frame-alist)  '60)
 (im/config-font 30))

(when (and (with-windows) (not (with-classroom))) ;; other wins
  (load-theme 'spacemacs-dark t)
  (im/config-font 16))

(with-linux-g            ;; linux graphic environment
 (tool-bar-mode 0) (menu-bar-mode 0)
 (set-face-attribute 'default nil :height 110))

(with-debian-vps         ;; debian vps environment
 (load-theme 'origin t)
 (menu-bar-mode 0)
 (xterm-mouse-mode)
 (global-set-key [mouse-4] (ilambda (scroll-down 1)))
 (global-set-key [mouse-5] (ilambda (scroll-up 1))))



(require 'imokeys)
(mmm~mode 1)



(require 'server)
(if (null (server-running-p)) (server-start))



(provide 'imfine)
;;; file ends here
