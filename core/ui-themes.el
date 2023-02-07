;;; -*- lexical-binding: t -*-

;;; Code:

(defun:around load-theme (fn theme &optional no-confirm no-enable)
  "Keep only one theme & no confirm."
  (when (called-interactively-p 'any)
    (mapc #'disable-theme custom-enabled-themes))
  (funcall fn theme t no-enable))

(xzz nano-theme
  :ref "rougier/nano-theme"
  :config
  (setopt nano-light-highlight "#eeeeee"))

(xzz gruvbox-theme
  :ref "greduan/emacs-theme-gruvbox")

(xzz modus-themes
  :ref "protesilaos/modus-themes"
  :config
  (setopt modus-themes-mode-line '(1 borderless)))

(xzz zero-dark-theme
  :ref "NicolasPetton/zerodark-theme"
  :config
  (setopt zero-dark-use-paddings-in-mode-line 3))

(add-to-list 'custom-theme-load-path (loce "themes"))

(if-let* ((theme (f/get :theme))) (load-theme theme t))
