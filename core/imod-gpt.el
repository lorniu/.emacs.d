;;; imod-gpt.el --- ChatGPT -*- lexical-binding: t -*-

;;; Code:

(x gptel
   :config
   ;; (setq-default gptel-backend "MyChat" :host . :key .. :stream t :models '(...))
   (setq gptel-default-mode 'org-mode)
   (setq gptel-prompt-prefix-alist '((markdown-mode . "## ") (org-mode . "** ") (text-mode . "## ")))
   (defun:hook gptel-mode-hook () (toggle-truncate-lines -1))
   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
   (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
   (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send))

(provide 'imod-gpt)

;;; imod-gpt.el ends here
