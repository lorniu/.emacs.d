;;; -*- lexical-binding: t -*-

;;; Code:

(defun im:setup-sudo-face ()
  "Highlight header line when editing as root."
  (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
    (setq header-line-format
          (propertize " Edit as ROOT! " 'face '(:foreground "white" :background "red3")))))

(dolist (hook '(find-file-hook dired-mode-hook))
  (add-hook hook #'im:setup-sudo-face))

(defun sudo ()
  "Edit file as Super User."
  (interactive)
  (if IS-WIN
      (if-let* ((f (buffer-file-name)))
          (progn
            (start-process-shell-command
             (format-time-string "sudo-%s") nil
             (concat "wsudo notepad " f))
            (message "Opened %s with elevated privileges" f))
        (user-error "No buffer file found"))
    (let* ((pt (point))
           (old-buf (current-buffer))
           (buf-name (expand-file-name (or buffer-file-name default-directory)))
           (sudo-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name))))
      (condition-case err
          (progn
            (cl-letf (((symbol-function 'server-buffer-done)
                       (lambda (&rest _) nil)))
              (find-file sudo-name))
            (kill-buffer-if-not-modified old-buf)
            (goto-char pt))
        (error (message "Failed to open with sudo: %s" (error-message-string err)))))))
