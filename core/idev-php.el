;;; idev-php.el --- PHP -*- lexical-binding: t -*-

;;; Code:

(x php-mode)

(defun:hook php-mode-hook ()
  (electric-pair-local-mode 1)
  (local-set-key (kbd "C-h d") 'im:php-lookup))

(defun im:php-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")
      (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol))))))

(provide 'idev-php)

;;; idev-php.el ends here
