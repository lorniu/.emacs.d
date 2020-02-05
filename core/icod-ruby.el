;;; icod-ruby.el --- Ruby -*- lexical-binding: t -*-

;; - Rinari is a collection to develop RoR
;; - Inf-ruby provide a REPL

;;; Code:

(x ruby-mode
   :hook ((ruby-mode-hook . inf-ruby-minor-mode)
          (ruby-mode-hook . robe-mode))
   :defer-config
   (x inf-ruby
      :defer-config
      (define-key inf-ruby-minor-mode-map (kbd "C-c C-s")
                  (lambdai  ;; open rails console, fails then irb
                   (or (ignore-errors (inf-ruby-console-auto))
                       (ignore-errors (inf-ruby)))
                   (robe-start)))))

(provide 'icod-ruby)

;;; icod-ruby.el ends here
