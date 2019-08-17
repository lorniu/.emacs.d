;;; misc.el --- Miscellaneous or Temporary Contents -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;;; Hook for COMMAND `im/git-commit-and-push'

(defun what-to-do-after-git-commit-and-push ()
  (let ((project (cdr (project-current))))
    (cond ((and (boundp '*vps-reload-url*) ; need add reload-url to init-file.
                (string= project "~/.notes/"))
           (url-retrieve (symbol-value '*vps-reload-url*)
                         (lambda (_)
                           (re-search-forward "\n\n")
                           (message "%s" (buffer-substring (point) (point-max)))
                           (kill-buffer)))))))

(add-hook 'gac/commit-and-push-hook 'what-to-do-after-git-commit-and-push)


(provide 'misc)

;;; misc.el ends here
