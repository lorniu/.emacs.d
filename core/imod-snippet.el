;;; -*- lexical-binding: t -*-

;;; Code:

(xzz abbrev/d
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(xzz dabbrev
  "What M-/ bound."
  :config
  (setopt dabbrev-case-fold-search 'case-fold-search
          dabbrev-case-replace nil))

(xzz hippie-exp
  :config
  ;; (global-set-key (kbd "M-/") #'hippie-expand)
  (setopt hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-visible
            try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
            try-complete-file-name-partially try-complete-file-name
            try-expand-all-abbrevs try-expand-list try-expand-line
            try-complete-lisp-symbol-partially try-complete-lisp-symbol)))



(xzz autoinsert
  "Auto insert content after create new file."
  :init (auto-insert-mode 1)
  :config
  (setopt auto-insert-query nil
          auto-insert-alist nil
          auto-insert-directory (loce "snippets/Init/"))
  (define-auto-insert "\\.ccls$"    [".ccls" im:autoinsert-yas-expand])
  (define-auto-insert "\\.service$" ["systemd" im:autoinsert-yas-expand])
  (define-auto-insert "\\.desktop$" ["xdg-desktop" im:autoinsert-yas-expand])
  (define-auto-insert "\\.md$"      (lambda () (im:yas-insert-by-name "init"))))

(xzz yasnippet/ed
  :ref ("joaotavora/yasnippet"
        "DOC: https://joaotavora.github.io/yasnippet/index.html")
  :bind (:map yas-keymap ("C-m" . yas-next-field-or-maybe-expand))
  :init
  (setq yas-verbosity 2 yas--basic-extras '(fundamental-mode))
  (yas-global-mode 1)
  :config
  (setopt yas-snippet-dirs  (cl-remove-if 'null (list (loco "snippets" t) (loce "snippets"))))
  (diminish 'yas-minor-mode)
  (defun:hook yas-minor-mode-hook ()
    (mapc 'yas-activate-extra-mode yas--basic-extras)))

(defun im/yas--clear-extra-mode ()
  (interactive)
  (dolist (mode yas--extra-modes)
    (unless (member mode yas--basic-extras)
      (yas-deactivate-extra-mode mode))))

(defun:override yas--prompt-for-template//pp (templates &optional prompt)
  "Pretty display snippets when `yas-insert-snippet'."
  (when templates
    (setq templates
          (sort templates (lambda (t1 t2) (string< (yas--template-key t1) (yas--template-key t2)))))
    (let* ((collection (cl-loop for tpl in templates
                                for str = (format "%-10s %s"
                                                  (propertize (yas--template-key tpl) 'face 'font-lock-keyword-face)
                                                  (yas--template-name tpl))
                                collect (cons str tpl)))
           (choosen (completing-read (or prompt "Choose a snippet: ") (im:completion-table collection) nil t)))
      (cdr (assoc choosen collection)))))

(defun:override yas--get-snippet-tables//org-src-block (&optional mode)
  "Fixup `yas-insert-snippet' used on org-mode src block."
  (when (and (equal major-mode 'org-mode) (org-in-src-block-p t))
    (let* ((l (car (org-babel-get-src-block-info)))
           (m (assoc l org-src-lang-modes)))
      (setq mode (intern (concat (if m (symbol-name (cdr m)) l) "-mode")))))
  (remove nil (mapcar #'(lambda (name) (gethash name yas--tables))
                      (yas--modes-to-activate mode))))

(defun:override yas--guess-snippet-directories//completion (&optional table)
  "Hack. Choose possible snippet dir when create snippet."
  (let ((main-dir (let ((dirs (or (yas-snippet-dirs)
                                  (setq yas-snippet-dirs (list yas--default-user-snippets-dir)))))
                    (completing-read "Snippet dir: " dirs nil t nil nil (car dirs)))) ;; this one
        (tables (if table (list table) (yas--get-snippet-tables))))
    (unless table
      (let ((major-mode-table (yas--table-get-create major-mode)))
        (cl-callf2 delq major-mode-table tables)
        (push major-mode-table tables)))
    (mapcar #'(lambda (table)
                (cons table (mapcar #'(lambda (subdir) (expand-file-name subdir main-dir))
                                    (yas--guess-snippet-directories-1 table))))
            tables)))

(defun im:yas-map-void (check replace-t replace-f)
  (if (string= check "") replace-t replace-f))

(defun im:yas-insert-by-name (name)
  "Insert snippet whose name is NAME."
  (when-let* ((template
               (and yas-minor-mode
                    (cl-find name (yas--all-templates (yas--get-snippet-tables))
                             :key #'yas--template-name :test #'equal))))
    (yas-expand-snippet (yas--template-content template))))

(defun im:autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))



(defvar im:license-templates nil)

(defun im:license-templates ()
  (or im:license-templates
      (setq im:license-templates
            (cl-coerce (pdd "https://api.github.com/licenses" :sync t) 'list))))

(defun im:license-template-info (name)
  (let* ((licenses (im:license-templates))
         (license (cl-find-if (lambda (l) (equal name (alist-get 'key l))) licenses)))
    (pdd (alist-get 'url license) :sync t)))

(defun im/license-template-insert (name)
  (interactive
   (list (completing-read "License template: "
                          (im:completion-table
                           (mapcar (lambda (y) (alist-get 'key y)) (im:license-templates)))
                          nil t)))
  (insert (alist-get 'body (im:license-template-info name))))

(defun im/license-template-new-file (name &optional dir)
  (interactive
   (list (completing-read "License template: "
                          (im:completion-table
                           (mapcar (lambda (y) (alist-get 'key y)) (im:license-templates)))
                          nil t)
         (if current-prefix-arg
             (read-directory-name "Create license in directory: ")
           default-directory)))
  (let ((file (expand-file-name "LICENSE" dir)))
    (when (file-exists-p file)
      (user-error "Can't create '%s', because it already exists" (abbreviate-file-name file)))
    (write-region (alist-get 'body (im:license-template-info name)) nil file)))



(defvar im:gitignore-templates nil)

(defun im:gitignore-templates ()
  (or im:gitignore-templates
      (setq im:gitignore-templates
            (pdd "https://api.github.com/gitignore/templates"
              :headers '(("Accept" . "application/vnd.github.v3+json"))
              :done (lambda (rs) (cl-coerce rs 'list)) :sync t))))

(defun im:gitignore-template-content (name)
  (unless (member name (im:gitignore-templates))
    (user-error "Invalid name %s" name))
  (pdd (concat "https://api.github.com/gitignore/templates/" name)
    :done (lambda (rs) (alist-get 'source rs)) :sync t))

(defun im/gitignore-template-insert (name)
  (interactive
   (list (completing-read ".gitignore template: "
                          (im:completion-table (im:gitignore-templates))
                          nil t)))
  (insert (im:gitignore-template-content name)))

(defun im/gitignore-template-new-file (name &optional dir)
  (interactive
   (list (completing-read ".gitignore template: "
                          (im:completion-table (im:gitignore-templates))
                          nil t)
         (if current-prefix-arg
             (read-directory-name "Create .gitignore in directory: ")
           default-directory)))
  (let ((file (expand-file-name ".gitignore" dir)))
    (when (file-exists-p file)
      (user-error "Can't create '%s', because it already exists" (abbreviate-file-name file)))
    (write-region (im:gitignore-template-content name) nil file)))



(transient-define-prefix im/transient-yasnippet ()
  [[("n" "new-snippet"           yas-new-snippet)
    ("f" "open-snippet"          yas-visit-snippet-file)]
   [("e" "activate-extra-mode"   yas-activate-extra-mode)
    ("c" "clear-all-extra-modes" im/yas--clear-extra-mode)]
   [("i" "pick-snippet"          yas-insert-snippet)
    ("l" "view-yas-tables"       (lambda () (interactive)
                                   (yas-describe-tables)
                                   (pop-to-buffer "*YASnippet Tables*")))]
   [("a" "reload-all"            yas-reload-all)
    ("d" "load-from-directory"   yas-load-directory)]])
