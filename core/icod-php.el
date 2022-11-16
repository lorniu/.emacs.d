;;; icod-php.el --- PHP -*- lexical-binding: t -*-

;;; Code:

(x php-mode
   :defer-config
   (defun my-php-lookup ()
     (interactive)
     (let ((symbol (symbol-at-point)))
       (if (not symbol)
           (message "No symbol at point.")
         (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol))))))

   (defun:hook php-mode-hook ()
     (electric-pair-local-mode 1)
     (local-set-key (kbd "C-h d") 'my-php-lookup)))

(provide 'icod-php)

;;; icod-php.el ends here
