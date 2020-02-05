;;; name.el --- auto commit and auto push to remote -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'project)

(defvar gac/ask-for-summary-p t)

(defun gac/prompt-password (proc string)
  "Provide password if neccessary."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t) ask)
      (cond
       ((or (string-match "^Enter passphrase for key '\\(.*\\)': $" string)
            (string-match "^\\(.*\\)'s password:" string))
        (setq ask (format "Password for '%s': " (match-string 1 string))))
       ((string-match "^[pP]assword.*:" string)
        (setq ask "Password: ")))
      (when ask
        (process-send-string proc (concat (read-passwd ask nil) "\n"))))))

(defun gac/get-commit-message (filename)
  "Get a commit message. Default to FILENAME."
  (let ((relative-filename (file-name-nondirectory filename)))
    (if (not gac/ask-for-summary-p)
        relative-filename
      (read-string "Summary: " relative-filename))))

(defun im/git-commit (&optional commit-msg)
  (interactive)
  (let* ((buffer-file (or (buffer-file-name) (buffer-name)))
         (commit-msg (if (zerop (length commit-msg)) (gac/get-commit-message buffer-file) commit-msg))
         (default-directory (or (project-root (project-current)) (file-name-directory buffer-file))))
    (shell-command "git add .")
    (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))))

(defun im/git-commit-and-push (&optional commit-msg)
  (interactive)
  (im/git-commit commit-msg)
  (let ((proc (start-process "git" "*git-auto-push*" "git" "push")))
    (set-process-filter proc 'gac/prompt-password)
    (set-process-sentinel proc (lambda (_ status)
                                 (run-hooks 'gac/commit-and-push-hook)
                                 (message "Git *PUSH* %s !" (substring status 0 -1))))))


(provide 'git-auto-commit)

;;; git-auto-commit.el ends here
