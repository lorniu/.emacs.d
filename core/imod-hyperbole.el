;;; -*- lexical-binding: t -*-

;; Emulate `left click / context menu` in GUI.

;; 2025-05-05, remove embark.

;;; Code:

(xzz hyperbole/d
  :ref ("https://tilde.town/~ramin_hal9001/articles/intro-to-hyperbole.html")
  :bind (:map imfine-mode-map ("C-h h" . hyperbole)) ; C-h h to call hkey-either
  :config (setopt hkey-init nil) ; inhibit default keybinds
  :commands (hkey-either))

(defun im/smart-ctrl+return ()
  (interactive)
  (cond ((equal major-mode 'inferior-fsharp-mode)
         (call-interactively #'fsharp-comint-send))
        ((string-match-p "^calc-.*mode$" (symbol-name major-mode))
         (call-interactively #'calc-last-args))
        ((equal (buffer-name) "*gt-chat-taker*")
         (call-interactively #'gt-chat-send-current))
        (t (require 'hyperbole)
           (call-interactively #'hkey-either))))

(define-key imfine-mode-map (kbd "C-<return>") #'im/smart-ctrl+return)
