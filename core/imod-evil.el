;;; imod-evil.el --- Emulate VI -*- lexical-binding: t -*-

;;; Code:

(x evil
   :ref "emacs-evil/evil"
   :commands 'evil-local-mode
   :init
   (setq evil-mode-line-format '(after . mode-line-modes)))

(provide 'imod-evil)

;;; imod-evil.el ends here
