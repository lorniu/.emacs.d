;; auto commit and auto push to remote

(defvar gac/ask-for-summary-p t)

(defun gac/process-filter (proc string)
  "Provide password if neccessary."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t) ask)
      (cond
       ((or (string-match "^Enter passphrase for key '\\(.*\\)': $" string)
            (string-match "^\\(.*\\)'s password:" string))
        (setq ask (format "Password for '%s': " (match-string 1 string))))
       ((string-match "^[pP]assword.*:" string)
        (setq ask "Password :")))
      (when ask
        (process-send-string proc (concat (read-passwd ask nil) "\n"))))))

(defun gac/commit-msg (filename)
  "Get a commit message. Default to FILENAME."
  (let ((relative-filename (file-name-nondirectory filename)))
    (if (not gac/ask-for-summary-p)
        relative-filename
      (read-string "Summary: " relative-filename))))

(defun im/git-commit ()
  (interactive)
  (let* ((buffer-file (or (buffer-file-name) (buffer-name)))
         (commit-msg (gac/commit-msg buffer-file))
         (default-directory (or (projectile-project-root) (file-name-directory buffer-file))))
    (shell-command "git add .")
    (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))))

(defun im/git-commit-and-push ()
  (interactive)
  (im/git-commit)
  (let ((proc (start-process "git" "*git-auto-push*" "git" "push")))
    (set-process-filter proc 'gac/process-filter)
    (set-process-sentinel proc (lambda (proc status) (message "Git push %s !" (substring status 0 -1))))))

(provide 'git-auto-commit)
