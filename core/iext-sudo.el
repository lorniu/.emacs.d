;;; iext-sudo.el --- sudo -*- lexical-binding: t -*-

;;; Code:

(cl-loop with sudo-face
         = (lambda ()
             (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
               (setq header-line-format (propertize " Edit as ROOT! " 'face '(:foreground "white" :background "red3")))))
         for hook in '(find-file-hook dired-mode-hook)
         do (add-hook hook sudo-face)
         finally
         (defun su ()
           "Edit file as Super User."
           (interactive)
           (let ((pt (point)) (old-buf (current-buffer))
                 (buf-name (expand-file-name (or buffer-file-name default-directory))))
             (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
             (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
               (find-file buf-name))
             (kill-buffer-if-not-modified old-buf)
             (goto-char pt))))

(provide 'iext-sudo)

;;; iext-sudo.el ends here
