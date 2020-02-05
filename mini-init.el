;;; -*- lexical-binding: t -*-

;;; Code:

(prog1 :mini
  (package-initialize)
  (add-to-list 'load-path (locate-user-emacs-file "core"))
  (setq debug-on-error t)
  (tool-bar-mode -1))

(prog1 :vertico
  (require 'vertico)
  (vertico-mode 1))

(prog1 :corfu
  (require 'corfu)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (corfu-mode 1))

(prog1 :package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents))

;;; mini-init.el ends here
