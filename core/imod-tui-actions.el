;;; imod-tui-actions.el --- Hyperbole and Embark -*- lexical-binding: t -*-

;; Emulate `left click / context menu` in GUI.

;; M-Ret for Hyperbole, and C-Ret for Embark.

;;; Code:

(x hyperbole/d
   :ref ("https://tilde.town/~ramin_hal9001/articles/intro-to-hyperbole.html")
   :init (setq hkey-init nil) ; inhibit default keybinds
   :bind (:map imfine-mode-map ("C-h h" . hyperbole)) ; C-h h to call hkey-either
   :commands (hkey-either))



(x embark/e
   :ref "oantolin/embark"
   :bind (:map embark-file-map (("l"   . vlf)))
   :init
   (setq embark-cycle-key "C-<return>") ; C-RET to call `embark-act'
   :config
   (require 'embark-consult)
   (define-key embark-region-map "f" #'gts-do-translate)
   (define-key embark-collect-mode-map (kbd "C-l") #'consult-preview-at-point)
   (embark-define-thingatpt-target defun prog-mode))



(defun im/smart-alt+return ()
  (interactive)
  (cond ((equal major-mode 'inferior-fsharp-mode)
         (call-interactively #'fsharp-comint-send))
        (t (require 'hyperbole)
           (call-interactively #'hkey-either))))

(defun im/smart-ctrl+return ()
  (interactive)
  (call-interactively
   (if current-prefix-arg #'embark-dwim #'embark-act)))

(define-key imfine-mode-map (kbd "M-<return>") #'im/smart-alt+return)
(define-key imfine-mode-map (kbd "C-<return>") #'im/smart-ctrl+return)

(provide 'imod-tui-actions)

;;; imod-tui-actions.el ends here
