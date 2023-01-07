;;; icode.el --- Prog-Mode -*- lexical-binding: t -*-

;;; Code:

(x treesit
   "Should:
 1) install tree-sitter
 2) build emacs --with-tree-sitter
 3) compile emacs module for langs"
   :ref ("https://github.com/tree-sitter"
         "script to build modules: casouri/tree-sitter-module")
   :if (executable-find "tree-sitter")
   :init
   (setq treesit-extra-load-path (list (locc "treesitter"))))

(x prog-mode
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list) ("M-q" . nil))
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

(provide 'icode)

;;; icode.el ends here
