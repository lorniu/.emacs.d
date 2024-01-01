;;; -*- lexical-binding: t -*-

;;; Code:

(defvar im:emskin-path "~/Source/emacs-emskin/elisp")

(xzz emskin
  :ref "loyalpartner/emskin"
  :if (and IS-LINUX-G (file-exists-p im:emskin-path))
  :commands (emskin-open-app emskin-open-native-app emskin-toggle-crosshair emskin-rofi)
  :init (add-to-list 'load-path im:emskin-path)
  :config
  (defun emskin-rofi ()
    (interactive)
    (require 'emskin)
    (start-process "rofi" nil
                   "setsid" "rofi"
                   "-show" "combi"
                   "-combi-modi" "drun,ssh"
                   "-terminal" "alacritty"
                   "-show-icons" "-i")))
