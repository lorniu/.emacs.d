;;; -*- lexical-binding: t -*-

;;; Code:

(xzz prog-mode
  :bind (:map prog-mode-map ("C-c C-u" . backward-up-list) ("M-q" . nil))
  :config
  (defun:hook prog-mode-hook ()
    (setq-local show-trailing-whitespace (bound-and-true-p org-src-mode))
    (abbrev-mode 1)
    (which-function-mode 1)
    (ignore-errors (rainbow-delimiters-mode 1))))

(xzz eldoc/d
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(xzz flymake
  :config
  (setq elisp-flymake-byte-compile-load-path load-path))

(xzz checkdoc)

(xzz which-func)



(xzz sgml-mode
  :mode "\\.\\(props\\|targets\\|resx\\)\\'"
  :config
  (setopt sgml-basic-offset 4)
  (unbind-key "\C-c/" sgml-mode-map))

(xzz nxml-mode
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

(xzz yaml-mode)

(xzz properties-mode
  "Java Properties file."
  :mode "\\.properties\\'"
  :ref "iquiw/properties-mode")



(xzz xref/e
  :config
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

(xzz citre
  :ref (tags "universal-ctags/ctags" "universal-ctags/citre")
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

  :config
  (setq citre-default-create-tags-file-location 'global-cache
        citre-auto-enable-citre-mode-modes '(cc-mode))
  (define-key citre-peek-keymap (kbd "M-.") 'citre-peek-through))

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
