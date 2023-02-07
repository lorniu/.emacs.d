;;; -*- lexical-binding: t -*-

;;; Code:

(catch 'sudo
  (cl-loop with sudo-face
           = (lambda ()
               (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
                 (setq header-line-format (propertize " Edit as ROOT! " 'face '(:foreground "white" :background "red3")))))
           for hook in '(find-file-hook dired-mode-hook)
           do (add-hook hook sudo-face)
           finally
           (defun sudo ()
             "Edit file as Super User."
             (interactive)
             (if IS-WIN
                 (if-let* ((f (buffer-file-name)))
                     (start-process-shell-command (format-time-string "sudo-%s") nil (concat "wsudo notepad " f))
                   (user-error "No buffer-file found"))
               (let ((pt (point)) (old-buf (current-buffer))
                     (buf-name (expand-file-name (or buffer-file-name default-directory))))
                 (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
                 (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
                   (find-file buf-name))
                 (kill-buffer-if-not-modified old-buf)
                 (goto-char pt))))))
