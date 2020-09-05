;;; im-dired-plus.el --- Extra utils for Dired-Mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun idp/dired-du-size ()
  "Count size of marked dir/files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-scb" files)
      (message
       "Size of all %s marked files: %s"
       (length files)
       (progn
         (re-search-backward "^\\([ 0-9.,]+[A-Za-z]*\\).*\\(total\\|总用量\\)$")
         (file-size-human-readable (string-to-number (match-string 1))))))))

(defun idp/dired-rsync (dest)
  "Send marked files with rsync."
  (interactive
   (list (progn
           (require 'dired-aux)
           (expand-file-name
            (read-file-name "Rsync to:" (dired-dwim-target-directory))))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        (rsync-command "rsync -arvz --progress "))
    (dolist (file files)
      (setq rsync-command
            (concat rsync-command (shell-quote-argument file) " ")))
    (setq rsync-command (concat rsync-command (shell-quote-argument dest)))
    (async-shell-command rsync-command "*rsync*")
    (other-window 1)))



;;; Dired Quick Sort, need `ls-lisp' support.

(defvar dired-quick-sort-list '((:name "" -1) (:time "-t" -1) (:size "-S" -1) (:ext "-X" 1)))
(defvar dired-quick-sort-current nil)
(defvar dired-quick-sort-group-dir 1)
(make-variable-buffer-local 'dired-quick-sort-current)
(make-variable-buffer-local 'dired-quick-sort-group-dir)

(defun dired-quick-sort-next (name)
  (let* ((name (or name :name))
         (reverse (if (and dired-quick-sort-current (eq (car dired-quick-sort-current) name))
                      (* -1 (cdr dired-quick-sort-current))
                    (caddr (assoc name dired-quick-sort-list)))))
    (setq dired-quick-sort-current (cons name reverse))))

;;;###autoload
(defun dired-quick-sort (name &optional group-dir)
  "Sort dired by the given criteria."
  (interactive)
  (dired-quick-sort-next name) ; if name present, switch action
  (if group-dir (setq dired-quick-sort-group-dir group-dir))
  (let ((switches
         (format "%s %s %s"
                 dired-listing-switches
                 (cadr (assoc (car dired-quick-sort-current) dired-quick-sort-list))
                 (if (plusp (cdr dired-quick-sort-current)) "" "-r")))
        (ls-lisp-dirs-first (plusp dired-quick-sort-group-dir)))
    (dired-sort-R-check switches)
    (setq dired-actual-switches switches)
    (when (eq major-mode 'dired-mode)
      (setq mode-name (format "Dired by %s (%s)" (car dired-quick-sort-current) (if (plusp (cdr dired-quick-sort-current)) "u" "n")))
      (force-mode-line-update))
    (revert-buffer)))


(provide 'im-dired-plus)

;;; im-dired-plus.el ends here
