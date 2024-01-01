;;; -*- lexical-binding: t -*-

;;; Code:

(require 'project)

(add-to-list 'project-find-functions #'im:project-try-special-file t)

(cl-defmethod project-root ((_project (eql nil))) nil)

(cl-defmethod project-root ((project (head flg))) (cdr project))

(defun im:project-try-special-file (dir)
  (cl-flet ((files-match-p (regexp d) (cl-find-if (lambda (f) (string-match-p regexp f)) (directory-files d))))
    (cl-macrolet ((gor (&rest rest) `(let ((f (locate-dominating-file dir (lambda (d) (or ,@rest))))) (if f (cons 'flg f)))))
      (gor
       (file-exists-p (expand-file-name ".project" d))
       (file-exists-p (expand-file-name ".projectile" d))
       (file-exists-p (expand-file-name ".idea" d))
       (file-exists-p (expand-file-name ".vscode" d))))))

(defun im/project-find-directory()
  (interactive)
  (cl-labels
      ((dirs (project)
         (delete-dups
          (delq-nil
           (mapcar (lambda (f)
                     (cl-subseq (file-name-directory f)
                                (length (expand-file-name (project-root project)))))
                   (project-files project))))))
    (let* ((project (or (project-current)
                        (user-error "No project detected")))
           (root (project-root project))
           (dirs (dirs project))
           (dir (completing-read (format "Directories (%s): " root) dirs)))
      (dired (expand-file-name dir root)))))



(xzz editorconfig/d
  :hook (after-init . editorconfig-mode))
