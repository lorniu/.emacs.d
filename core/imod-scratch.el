;;; -*- lexical-binding: t -*-

;;; Code:

;;; Add the ability to resume *scratch* after a restart

(defvar im/scratch-location (locc))
(defvar im/scratch-file-prefix "scratch_")
(defvar im/scratch-keeps 15)

(defun im/scratch-saves ()
  (sort (directory-files im/scratch-location t (concat "^" im/scratch-file-prefix "[0-9]\\{8\\}\\.*[0-9]\\{6\\}$")) #'string>))

(defun scratch-buffer-save ()
  "Save current *scratch* buffer to file."
  (interactive)
  (condition-case err
      (let* ((filename (format "%s%s%s"
                               im/scratch-location
                               im/scratch-file-prefix
                               (format-time-string "%4Y%2m%2d.%H%M%S")))
             (saves (im/scratch-saves))
             (contents (with-current-buffer "*scratch*" (string-trim (buffer-string)))))
        (if (or (< im/scratch-keeps 1)
                (and saves
                     (or
                      (< (length contents) 10)
                      (string= contents (string-trim (substitute-command-keys initial-scratch-message)))
                      (string= contents (with-temp-buffer (insert-file-contents (car saves)) (string-trim (buffer-string)))))))
            (message "No need to save")
          (with-temp-file filename (princ contents (current-buffer)))
          (setq saves (cons filename saves))
          (when (>= (length saves) im/scratch-keeps)
            (cl-loop for f in saves for i from 1
                     if (> i im/scratch-keeps) do (delete-file f)))
          (message "Saved to %s" filename)))
    (error (message "%s" err))))

(defun scratch-buffer-restore ()
  "Restore *scratch* buffer from file."
  (interactive)
  (let* ((files (or (im/scratch-saves) (user-error "No saved scratches found")))
         (file (completing-read "Restore from: "
                                (lambda (input pred action)
                                  (if (eq action 'metadata)
                                      `(metadata (display-sort-function . identity)
                                                 (annotation-function
                                                  . (lambda (f) (concat "  " (marginalia--time (file-attribute-modification-time (file-attributes f)))))))
                                    (complete-with-action action files input pred))))))
    (cond ((string-empty-p file)
           (with-current-buffer (find-file im/scratch-location)
             (ignore-errors (if (search-forward "scratch") (backward-word)))))
          ((file-exists-p file)
           (with-current-buffer "*scratch*"
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert-file-contents file)
               (set-buffer-modified-p nil)))))))

(defun scratch-buffer ()
  "This sends you to the *scratch* buffer."
  (interactive)
  (let ((name "*scratch*"))
    (unless (get-buffer name)
      (progn
        (get-buffer-create name)
        (with-current-buffer name
          (insert (substitute-command-keys initial-scratch-message))
          (funcall initial-major-mode))))
    (pop-to-buffer name '(display-buffer-pop-up-window) t)))

(add-hook 'kill-emacs-hook #'scratch-buffer-save)



(transient-define-prefix im/assist-*scratch* ()
  [[("m"
     (lambda () (concat "Change to " (if (equal major-mode 'lisp-interaction-mode) "Org-Mode" "Lisp-Interaction-Mode")))
     (lambda () (interactive) (if (equal major-mode 'lisp-interaction-mode) (org-mode) (lisp-interaction-mode))))]
   [("s"  "Save"     scratch-buffer-save)]
   [("r"  "Restore"  scratch-buffer-restore)]])
