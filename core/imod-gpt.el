;;; -*- lexical-binding: t -*-

;;; Code:

(x gptel
   :config
   (setopt gptel-default-mode 'org-mode
           ;; gptel-backend "MyChat" :host . :key .. :stream t :models '(...))
           gptel-prompt-prefix-alist '((markdown-mode . "## ") (org-mode . "** ") (text-mode . "## ")))
   (defun:hook gptel-mode-hook () (toggle-truncate-lines -1))
   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
   (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
   (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send))
