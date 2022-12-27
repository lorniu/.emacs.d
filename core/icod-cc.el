;;; icod-cc.el --- C family -*- lexical-binding: t -*-

;; - CEDET + ECB is a choice
;; - Irony for C++ is another choice.
;; - cquery is used LSP (yay -S cquery-git)
;; - eglot + clangd

;;; Code:

(x cc-mode/i
   "For C family languages, such as Java/C/Cpp..."
   :config
   (setq-default c-basic-offset 4
                 gdb-many-windows t gdb-show-main t)

   (prog1 'define-indent-styles
     (c-add-style "microsoft"
                  '("stroustrup"
                    (c-offsets-alist
                     (innamespace . -)
                     (inline-open . 0)
                     (inher-cont . c-lineup-multi-inher)
                     (arglist-cont-nonempty . +)
                     (template-args-cont . +))))
     (c-add-style "openbsd"
                  '("bsd"
                    (c-backspace-function . delete-backward-char)
                    (c-syntactic-indentation-in-macros . nil)
                    (c-tab-always-indent . nil)
                    (c-hanging-braces-alist
                     (block-close . c-snug-do-while))
                    (c-offsets-alist
                     (arglist-cont-nonempty . *)
                     (statement-cont . *))
                    (indent-tabs-mode . t)))
     (c-add-style "doom"
                  '((c-comment-only-line-offset . 0)
                    (c-hanging-braces-alist (brace-list-open)
                                            (brace-entry-open)
                                            (substatement-open after)
                                            (block-close . c-snug-do-while)
                                            (arglist-cont-nonempty))
                    (c-cleanup-list brace-else-brace)
                    (c-offsets-alist
                     (knr-argdecl-intro . 0)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (statement-cont . +)
                     (case-label . +)
                     ;; align args with open brace OR don't indent at all (if open
                     ;; brace is at eolp and close brace is after arg with no trailing comma)
                     (brace-list-intro . 0)
                     (brace-list-close . -)
                     (arglist-intro . +)
                     (arglist-close +cc-lineup-arglist-close 0)
                     ;; don't over-indent lambda blocks
                     (inline-open . 0)
                     (inlambda . 0)
                     ;; indent access keywords +1 level, and properties beneath them another level
                     (access-label . -)
                     (inclass +cc-c++-lineup-inclass +)
                     (label . 0)))))

   (defun my-c-hook ()
     (c-turn-on-eldoc-mode)
     (set (make-local-variable 'compile-command)
          (if (file-exists-p "Makefile") "make"
            (format "cc %s -g %s -o %s"
                    (buffer-name)
                    (or (getenv "CFLAGS") "-std=c99 -Wall")
                    (file-name-sans-extension (buffer-name))))))

   (defun my-cpp-hook ()
     (set (make-local-variable 'compile-command)
          (if (file-exists-p "Makefile")
              "make" "clang++ -Wall -Wextra -std=c++14")))

   (add-hook 'c-mode-hook 'my-c-hook)
   (add-hook 'c++-mode-hook 'my-c-hook)
   (add-hook 'c++-mode-hook 'my-cpp-hook))

(x semantic
   :after (cc-mode)
   :config
   (global-semanticdb-minor-mode 1)
   (global-semantic-idle-scheduler-mode 1)
   (global-semantic-stickyfunc-mode 1))

(provide 'icod-cc)

;;; icod-cc.el ends here
