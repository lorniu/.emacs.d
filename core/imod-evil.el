;;; imod-evil.el --- Emulate VI -*- lexical-binding: t -*-

;; Viper: Basic Vi emulation mode offered by Emacs, builtin.
;; Evil:  A completely new Vim emulation mode for Emacs and is said to be the successor of both Viper and Vimpulse mode.

;;; Code:

(x viper
   :init
   (setq viper-inhibit-startup-message t)
   (setq viper-expert-level 5))

(x evil
   :ref "emacs-evil/evil"
   :commands 'evil-local-mode
   :init
   (setq evil-mode-line-format '(after . mode-line-modes)))

(provide 'imod-evil)

;;; imod-evil.el ends here
