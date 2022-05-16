;;; imod-project.el --- Project -*- lexical-binding: t -*-

;;; Code:

(x project/e
   :init
   (defvar project-mode-line '(:eval (my-project-mode-line)))
   (add-to-list 'project-find-functions #'my-project-try-flag-file t)

   (cl-defmethod project-root ((_project (eql nil))) nil)
   (cl-defmethod project-root ((project (head flg))) (cdr project)))

(defun my-project-mode-line ()
  (let ((pname (project-root (project-current))) (mname " "))
    (when pname
      (setq mname
            (if-let ((r (file-remote-p default-directory)))
                (format "{%s}  " (directory-file-name r))
              (format "<%s>  " (file-name-nondirectory (directory-file-name pname))))))
    (propertize mname 'face (if (facep 'project-mode-line-face) 'project-mode-line-face 'font-lock-comment-face))))

(defun my-project-try-flag-file (dir)
  (cl-flet ((files-match-p (regexp d) (cl-find-if (lambda (f) (string-match-p regexp f)) (directory-files d))))
    (cl-macrolet ((gor (&rest rest) `(let ((f (locate-dominating-file dir (lambda (d) (or ,@rest))))) (if f (cons 'flg f)))))
      (gor
       (file-exists-p (expand-file-name ".project" d))
       (file-exists-p (expand-file-name ".projectile" d))
       (files-match-p "\\.csproj$" d)))))

(defun im/project-find-directory()
  (interactive)
  (cl-labels
      ((dirs (project)
         (delete-dups
          (delq nil
                (mapcar (lambda (f)
                          (cl-subseq (file-name-directory f)
                                     (length (expand-file-name (project-root project)))))
                        (project-files project))))))
    (let* ((project (or (project-current)
                        (user-error "No project detected.")))
           (root (project-root project))
           (dirs (dirs project))
           (dir (completing-read (format "Directories (%s): " root) dirs)))
      (dired (expand-file-name dir root)))))



(x treemacs
   :ref "Alexander-Miller/treemacs"
   :defer-config
   (setq treemacs-position 'left
         treemacs-persist-file (locc "treemacs-persist")
         treemacs-width 35
         treemacs-indentation 2
         treemacs-no-png-images nil)
   (treemacs-without-messages (treemacs-resize-icons 16))
   (treemacs-follow-mode t)
   (treemacs-filewatch-mode t)
   (treemacs-fringe-indicator-mode t))

(x editorconfig/d
   :ref ("editorconfig/editorconfig-emacs" "https://editorconfig.org/")
   :init
   (editorconfig-mode 1)
   (add-to-list 'editorconfig-exclude-regexps "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

(provide 'imod-project)

;;; imod-project.el ends here
