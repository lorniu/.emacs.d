;;; -*- lexical-binding: t -*-

;; - Rinari is a collection to develop RoR
;; - Inf-ruby provide a REPL

;;; Code:

(xzz ruby-mode
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (ruby-mode . robe-mode)))

(xzz inf-ruby
  :after ruby-mode
  :config
  (define-key inf-ruby-minor-mode-map (kbd "C-c C-s")
              (lambdi ()  ;; open rails console, fails then irb
                (or (ignore-errors (inf-ruby-console-auto))
                    (ignore-errors (inf-ruby)))
                (robe-start))))
