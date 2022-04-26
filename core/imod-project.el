;;; imod-project.el --- Project -*- lexical-binding: t -*-

;;; Code:

(x project/e
   :init
   (cl-defmethod project-root ((_project (eql nil))) nil)
   (cl-defmethod project-root ((project (head flg))) (cdr project))

   (defvar project-mode-line
     '(:eval (condition-case _
                 (let* ((pname (project-root (project-current)))
                        (mname (if pname
                                   (format (if (file-remote-p default-directory) "{%s}  " "<%s>  ")
                                           (file-name-nondirectory (if (string-suffix-p "/" pname)
                                                                       (cl-subseq pname 0 (1- (length pname)))
                                                                     pname)))
                                 " ")))
                   (propertize mname 'face (if (facep 'project-mode-line-face)
                                               'project-mode-line-face
                                             'font-lock-comment-face)))
               (error ""))))

   (defun my-project-try-flg-file (dir)
     (cl-flet ((files-match-p (regexp d) (cl-find-if (lambda (f) (string-match-p regexp f)) (directory-files d))))
       (cl-macrolet ((gor (&rest rest) `(let ((f (locate-dominating-file dir (lambda (d) (or ,@rest))))) (if f (cons 'flg f)))))
         (gor
          (file-exists-p (expand-file-name ".project" d))
          (file-exists-p (expand-file-name ".projectile" d))
          (files-match-p "\\.csproj$" d)))))
   (add-to-list 'project-find-functions #'my-project-try-flg-file t)

   (defun my-project-directories(project)
     (delete-dups
      (delq nil
            (mapcar (lambda (f)
                      (cl-subseq (file-name-directory f)
                                 (length (expand-file-name (project-root project)))))
                    (project-files project)))))
   (defun im/project-find-dir()
     (interactive)
     (let* ((project (or (project-current)
                         (user-error "No project detected.")))
            (root (project-root project))
            (dirs (my-project-directories project))
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
