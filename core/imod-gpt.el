;;; -*- lexical-binding: t -*-

;;; Code:

(x gptel
   :config
   (defun:hook gptel-mode-hook () (toggle-truncate-lines -1))
   (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send))
