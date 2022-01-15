;;; imod-code.el --- Prog-Mode -*- lexical-binding: t -*-

;;; Code:

(x prog-mode
   :bind
   ((prog-mode-map
     ("C-c C-u" . backward-up-list)))
   :init
   (defun:hook prog-mode-hook ()
     (setq-local show-trailing-whitespace (bound-and-true-p org-src-mode))
     (abbrev-mode 1)
     (rainbow-delimiters-mode 1)
     (which-function-mode 1)))

(x eldoc/d
   :init
   (setq eldoc-echo-area-use-multiline-p nil))

(x flymake/i)

(x flycheck/i)

(provide 'imod-code)

;;; imod-code.el ends here
