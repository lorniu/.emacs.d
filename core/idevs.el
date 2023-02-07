;;; -*- lexical-binding: t -*-

;;; Code:

(x prog-mode
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list) ("M-q" . nil))
   :config
   (defun:hook prog-mode-hook ()
     (setq-local show-trailing-whitespace (bound-and-true-p org-src-mode))
     (abbrev-mode 1)
     (rainbow-delimiters-mode 1)
     (which-function-mode 1)))

(x treesit
   "Should:
 1) install tree-sitter
 2) emacs with build flag: --with-tree-sitter  (treesit-available-p)
 3) download tree-sitter grammers: treesit-extra-load-path  (treesit-language-available-p 'c)"
   :ref ("TreeSitter Home: https://github.com/tree-sitter/tree-sitter"
         "Precompiled Grammers: https://github.com/emacs-tree-sitter/tree-sitter-langs/releases")
   :config
   (when (treesit-available-p)
     (setopt major-mode-remap-alist '((yaml-mode . yaml-ts-mode)))))

(x eldoc/d
   :config
   (setopt eldoc-echo-area-use-multiline-p nil))

(x flymake)

(x checkdoc
   :config
   (defun:around checkdoc-sentencespace-region-engine//use-the-default-value (fn beg end)
     (let ((sentence-end-double-space t))
       (funcall fn beg end))))

(x which-func)

(x editorconfig/d
  :hook (after-init . editorconfig-mode))



(x sgml-mode
   :mode "\\.\\(props\\|targets\\|resx\\)\\'"
   :config
   (setopt sgml-basic-offset 4)
   (unbind-key "\C-c/" sgml-mode-map))

(x nxml-mode
   :config
   (setopt nxml-child-indent 4
           nxml-attribute-indent 4)
   (defun im:nxml-where ()
     "Display the hierarchy of XML elements the point is on as a path."
     (let ((path nil))
       (save-excursion
         (save-restriction
           (widen)
           (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                       (condition-case nil
                           (progn (nxml-backward-up-element) t)
                         (error nil)))
             (setq path (cons (xmltok-start-tag-local-name) path)))
           (if (called-interactively-p t)
               (message "/%s" (mapconcat 'identity path "/"))
             (format "/%s" (mapconcat 'identity path "/")))))))
   (defun:hook nxml-mode-hook ()
     ;; Show /x/y/z in modeline
     (add-hook 'which-func-non-auto-modes 'nxml-mode)
     (which-function-mode t)
     (add-hook 'which-func-functions 'im:nxml-where t t)))

(x yaml-mode)

(x properties-mode
   "Java Properties file."
   :mode "\\.properties\\'"
   :ref "iquiw/properties-mode")



(x xref/e
   :config
   (when (executable-find "rg")
     (setq xref-search-program 'ripgrep)))

(x citre
   :ref (tags "universal-ctags/ctags" "universal-ctags/citre")
   :init
   (require 'citre-config)
   (global-set-key (kbd "C-x c p") 'citre-ace-peek)
   (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

   :config
   (setopt citre-default-create-tags-file-location 'global-cache
           citre-auto-enable-citre-mode-modes '(prog-mode))

   (define-key citre-peek-keymap (kbd "M-.") 'citre-peek-through)

   (defun:override citre-relative-path//truename (path &optional root)
     "/home/vip to /datax/vip"
     (let ((root (or root (citre-project-root))))
       (if (and root (file-in-directory-p path root))
           (file-relative-name (file-truename path) (file-truename root))
         path))))

(defun xref-find-definitions+citre ()
  (interactive)
  (let ((ofn (lambda ()
               (let* ((xref-prompt-for-identifier nil))
                 (call-interactively #'xref-find-definitions)))))
    (if (bound-and-true-p eglot--cached-server)
        (funcall ofn)
      (condition-case _
          (citre-jump)
        (error (funcall ofn))))))

(defun im:push-point-to-xref-marker-stack (&rest _r)
  (xref-push-marker-stack (point-marker)))

(dolist (func '(find-function citre-jump))
  (advice-add func :before 'im:push-point-to-xref-marker-stack))
