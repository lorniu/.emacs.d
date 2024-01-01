;;; -*- lexical-binding: t -*-

;;; Code:

;;; Add the ability to resume *scratch* after a restart

(defvar im.scratch-keep-days 20)
(defvar im.scratch-location (locc "scratch"))

(defun im:scratch-saves ()
  (make-directory im.scratch-location t)
  (cl-loop with deadline = (time-subtract (current-time) (days-to-time im.scratch-keep-days))
           for f in (sort (directory-files im.scratch-location t "\\.scratch$") :reverse t)
           if (time-less-p (date-to-time (file-name-base f)) deadline) do (delete-file f)
           else collect f into saves
           finally return saves))

(defun scratch-save ()
  "Save current *scratch* buffer to file."
  (interactive)
  (condition-case err
      (let ((saves (im:scratch-saves))
            (contents
             (string-trim
              (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (if (equal (buffer-name) "*scratch*")
                    (buffer-string)
                  (error "Region not found"))))))
        (if (or (< im.scratch-keep-days 1)
                (and saves
                     (or (< (length contents) 10)
                         (string= contents (string-trim (substitute-command-keys initial-scratch-message)))
                         (string= contents (with-temp-buffer (insert-file-contents (car saves)) (string-trim (buffer-string)))))))
            (error "No scratch content is need to be saved")
          (let ((filename (expand-file-name (format-time-string "%Y%m%dT%H%M%S.scratch") im.scratch-location)))
            (with-temp-file filename (princ contents (current-buffer)))
            (message "Saved to %s" filename))))
    (error (message "scratch error: %s" (error-message-string err)))))

(defun scratch-restore ()
  "Restore *scratch* buffer from file."
  (interactive)
  (unless (equal (buffer-name) "*scratch*")
    (user-error "Should invoked in *scratch* buffer"))
  (let* ((files (or (im:scratch-saves)
                    (user-error "No saved scratches found")))
         (cands (mapcar (lambda (f)
                          (let* ((n (file-name-sans-extension (file-name-nondirectory f)))
                                 (p (propertize n 'display (replace-regexp-in-string "T" "_" n))))
                            (cons p f)))
                        files))
         (annfn (lambda (name)
                  (when-let* ((f (cdr (assoc name cands)))
                              (attrs (file-attributes f))
                              (size (file-attribute-size attrs))
                              (len (min 100 (max 40 (- (frame-width) 25))))
                              (prev (with-temp-buffer
                                      (insert-file-contents f nil (max 0 (- size (* 3 len))))
                                      (let ((s (string-trim (replace-regexp-in-string
                                                             "[ \n]+" " " (buffer-string)))))
                                        (string-truncate-left (substring s (max 0 (- (length s) len))) (- len 20))))))
                    (format "%s  %12s %5s  %s"
                            (make-string (max 0 (- 15 (length name))) ?\s)
                            (propertize (marginalia--time (file-attribute-modification-time attrs))
                                        'face 'font-lock-string-face)
                            (file-size-human-readable size)
                            (propertize prev 'face 'font-lock-comment-face)))))
         (choice (minibuffer-with-setup-hook
                     (lambda ()
                       (use-local-map (make-composed-keymap nil (current-local-map)))
                       (local-set-key (kbd "C-c C-k")
                                      (lambda ()
                                        (interactive)
                                        (run-at-time 0 nil
                                                     (lambda (n)
                                                       (let ((f (expand-file-name n im.scratch-location)))
                                                         (when (and (file-exists-p f)
                                                                    (y-or-n-p (format "Delete file %s?" n)))
                                                           (delete-file f))
                                                         (scratch-restore)))
                                                     (concat (im:completion-compat :current) ".scratch"))
                                        (abort-recursive-edit)))
                       (local-set-key (kbd "C-c C-o")
                                      (lambda ()
                                        (interactive)
                                        (run-at-time 0 nil
                                                     (lambda (pat)
                                                       (find-file im.scratch-location)
                                                       (ignore-errors
                                                         (goto-char (point-min))
                                                         (search-forward pat)))
                                                     (im:completion-compat :current))
                                        (abort-recursive-edit))))
                   (completing-read "Restore scratch: "
                                    (lambda (input pred action)
                                      (if (eq action 'metadata)
                                          `(metadata (display-sort-function . ,#'identity)
                                                     (annotation-function . ,annfn))
                                        (complete-with-action action cands input pred)))))))
    (when-let ((path (cdr (assoc choice cands))))
      (with-current-buffer "*scratch*"
        (erase-buffer)
        (insert-file-contents path)
        (set-buffer-modified-p nil)))))

(defvar scratch-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") #'scratch-save)
    (define-key map (kbd "C-x C-v") #'scratch-restore)
    map))

(define-minor-mode scratch-minor-mode
  "Minor mode for *scratch* buffer."
  :keymap scratch-minor-mode-map)

(defun:hook after-change-major-mode-hook/scratch-buffer ()
  (when (string= (buffer-name) "*scratch*")
    (scratch-minor-mode 1)))

(add-hook 'kill-emacs-hook #'scratch-save)



(transient-define-prefix im/assist-*scratch* ()
  [[("m"
     (lambda () (concat "Change to " (if (equal major-mode 'lisp-interaction-mode) "Org-Mode" "Lisp-Interaction-Mode")))
     (lambda () (interactive) (if (equal major-mode 'lisp-interaction-mode) (org-mode) (lisp-interaction-mode))))]
   [("s"  "Save"     scratch-save)]
   [("r"  "Restore"  scratch-restore)]])
