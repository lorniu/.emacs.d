;;; -*- lexical-binding: t -*-

;; Viper: Basic Vi emulation mode offered by Emacs, builtin.
;; Evil:  A completely new Vim emulation mode for Emacs and is said to be the successor of both Viper and Vimpulse mode.

;;; Code:

(xzz viper
  :config
  (setopt viper-inhibit-startup-message t
          viper-expert-level 5))

(xzz evil
  :ref "emacs-evil/evil"
  :commands 'evil-local-mode
  :config
  (setopt evil-mode-line-format '(after . mode-line-modes)))
