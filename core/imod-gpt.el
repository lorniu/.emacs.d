;;; -*- lexical-binding: t -*-

;;; Code:

(xzz gptel
  :config
  (defun:hook gptel-mode-hook () (toggle-truncate-lines -1))
  (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send))
