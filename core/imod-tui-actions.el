;;; -*- lexical-binding: t -*-

;; Emulate `left click / context menu` in GUI.

;; M-Ret for Hyperbole, and C-Ret for Embark.

;;; Code:

(x hyperbole/d
   :ref ("https://tilde.town/~ramin_hal9001/articles/intro-to-hyperbole.html")
   :bind (:map imfine-mode-map ("C-h h" . hyperbole)) ; C-h h to call hkey-either
   :config (setopt hkey-init nil) ; inhibit default keybinds
   :commands (hkey-either))



(x embark/e
   :ref "oantolin/embark"
   :bind (:map embark-file-map (("l"   . vlf)))
   :config
   (require 'embark-consult)
   (setopt embark-cycle-key "C-<return>") ; C-RET to call `embark-act'
   (define-key embark-region-map "f" #'gt-do-translate)
   (define-key embark-collect-mode-map (kbd "C-l") #'consult-preview-at-point)
   (embark-define-thingatpt-target defun prog-mode))



(defun im/smart-alt+return ()
  (interactive)
  (cond ((equal major-mode 'inferior-fsharp-mode)
         (call-interactively #'fsharp-comint-send))
        ((string-match-p "^calc-.*mode$" (symbol-name major-mode))
         (call-interactively #'calc-last-args))
        (t (require 'hyperbole)
           (call-interactively #'hkey-either))))

(defun im/smart-ctrl+return ()
  (interactive)
  (if (equal (buffer-name) "*gt-chat-taker*")
      (call-interactively #'gt-chat-send-current)
    (call-interactively
     (if current-prefix-arg #'embark-dwim #'embark-act))))

(define-key imfine-mode-map (kbd "M-<return>") #'im/smart-alt+return)
(define-key imfine-mode-map (kbd "C-<return>") #'im/smart-ctrl+return)
