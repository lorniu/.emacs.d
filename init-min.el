;;; -*- lexical-binding: t -*-

;;; Code:

(prog1 :basic
  (setq debug-on-error t
        make-backup-files nil
        auto-save-default nil
        custom-file (make-temp-file "custom-file-" nil ".el"))
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (tool-bar-mode -1))

(prog1 :vertico
  (use-package vertico :ensure t)
  (vertico-mode 1))

(prog1 :corfu
  (use-package corfu :ensure t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (corfu-mode 1))

;;; mini-init.el ends here
