;;; imod-tui-actions.el --- Hyperbole and Embark -*- lexical-binding: t -*-

;; Emulate `left click / context menu` in GUI.

;;; Code:

(x hyperbole/d
   :ref ("https://tilde.town/~ramin_hal9001/articles/intro-to-hyperbole.html")
   :init (setq hkey-init nil) ; inhibit default keybinds
   :bind (:map im-keys-mode-map ("C-h h" . hyperbole)) ; C-RET to call hkey-either
   :commands (hkey-either))



(x embark/e
   :ref "oantolin/embark"
   :bind (:map embark-file-map (("l"   . vlf)))
   :init
   (setq embark-cycle-key (kbd "C-<return>")) ; C-RET to call `embark-act'
   :config
   (require 'embark-consult)
   (define-key embark-region-map "f" #'gts-do-translate)
   (define-key embark-collect-mode-map (kbd "C-l") #'consult-preview-at-point)
   (embark-define-thingatpt-target defun prog-mode))



;; [embark demo] custom actions for *scratch* buffer.

(defun my-embark-target-scratch-buffer ()
  (when (equal "*scratch*" (buffer-name))
    (cons 'scratch (buffer-name))))

(defvar my-embark-scratch-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'ignore)
    map))

;;(add-to-list 'embark-target-finders #'my-embark-target-scratch-buffer)
;;(add-to-list 'embark-keymap-alist '(scratch . my-embark-scratch-buffer-map))

(provide 'imod-tui-actions)

;;; imod-tui-actions.el ends here
