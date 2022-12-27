;;; imod-snippet.el --- Snippets -*- lexical-binding: t -*-

;;; Code:

(x abbrev/d
   :config
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file)))

(x dabbrev
   "What M-/ bound."
   :init
   (setq dabbrev-case-fold-search 'case-fold-search)
   (setq dabbrev-case-replace nil))

(x hippie-exp
   :init
   ;; (global-set-key (kbd "M-/") #'hippie-expand)
   (setq hippie-expand-try-functions-list
         '(try-expand-dabbrev
           try-expand-dabbrev-visible
           try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
           try-complete-file-name-partially try-complete-file-name
           try-expand-all-abbrevs try-expand-list try-expand-line
           try-complete-lisp-symbol-partially try-complete-lisp-symbol)))



(x yasnippet/ed
   :ref ("joaotavora/yasnippet"
         "DOC: https://joaotavora.github.io/yasnippet/index.html")
   :bind (:map yas-keymap ("C-m" . yas-next-field-or-maybe-expand))
   :init
   (setq yas-verbosity 2
         yas--basic-extras '(fundamental-mode)
         yas-snippet-dirs  (cl-remove-if 'null (list (loco "snippets" t) (loce "snippets"))))

   (defun im/yas--clear-extra-mode ()
     (interactive)
     (dolist (mode yas--extra-modes)
       (unless (member mode yas--basic-extras)
         (yas-deactivate-extra-mode mode))))

   (defun:hook yas-minor-mode-hook ()
     (delight 'yas-minor-mode)
     (mapc 'yas-activate-extra-mode yas--basic-extras))

   (defun:override yas--prompt-for-template$pp (templates &optional prompt)
     "Pretty display snippets when `yas-insert-snippet'."
     (when templates
       (setq templates
             (sort templates #'(lambda (t1 t2)
                                 (string< (yas--template-key t1)
                                          (yas--template-key t2)))))
       (cl-some (lambda (fn)
                  (let (vertico-sort-function)
                    (funcall fn (or prompt "Choose a snippet: ")
                             templates
                             (lambda (tpl)
                               (format "%-10s %s"
                                       (propertize (yas--template-key tpl) 'face 'font-lock-keyword-face)
                                       (yas--template-name tpl))))))
                yas-prompt-functions)))

   (defun:override yas--guess-snippet-directories$completion (&optional table)
     "Hack. Choose posible snippet dir when create snippet."
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

   (defun:override yas--get-snippet-tables$org-src-block (&optional mode)
     "Fixup `yas-insert-snippet' used on org-mode src block."
     (when (and (equal major-mode 'org-mode) (org-in-src-block-p t))
       (let* ((l (car (org-babel-get-src-block-info)))
              (m (assoc l org-src-lang-modes)))
         (setq mode (intern (concat (if m (symbol-name (cdr m)) l) "-mode")))))
     (remove nil (mapcar #'(lambda (name) (gethash name yas--tables))
                         (yas--modes-to-activate mode))))

   (yas-global-mode 1))

(x autoinsert
   "Auto insert content after create new file."
   :init
   (auto-insert-mode 1)
   (setq auto-insert-query nil)
   (setq auto-insert-alist nil)
   (setq auto-insert-directory (loce "snippets/Init/"))
   (define-auto-insert "\\.ccls$"    [".ccls" im-autoinsert-yas-expand])
   (define-auto-insert "\\.service$" ["systemd" im-autoinsert-yas-expand])
   (define-auto-insert "\\.desktop$" ["xdg-desktop" im-autoinsert-yas-expand])
   (define-auto-insert "\\.md$"      (lambda () (im-yas-insert-by-name "init"))))



(defun im-yas-map-void (check replace-t replace-f)
  (if (string= check "") replace-t replace-f))

(defun im-yas-insert-by-name (name)
  "Insert snippet whose name is NAME."
  (when-let ((template
              (and yas-minor-mode
                   (cl-find name (yas--all-templates (yas--get-snippet-tables))
                            :key #'yas--template-name :test #'equal))))
    (yas-expand-snippet (yas--template-content template))))

(defun im-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(provide 'imod-snippet)

;;; imod-snippet.el ends here
