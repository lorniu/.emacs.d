;;; icod-tags.el --- Ctags Support -*- lexical-binding: t -*-

(defreference tags
  "universal-ctags/ctags"
  "universal-ctags/citre")



(x citre
   :init
   (require 'citre-config)
   (global-set-key (kbd "C-x c p") 'citre-ace-peek)
   (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

   :config
   (setq citre-default-create-tags-file-location 'global-cache)
   (setq citre-auto-enable-citre-mode-modes '(prog-mode))

   (define-key citre-peek-keymap (kbd "M-.") 'citre-peek-through)

   (defun:override citre-relative-path$truename (path &optional root)
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
    (if (or (bound-and-true-p lsp-buffer-uri)
            (bound-and-true-p eglot--cached-server))
        (funcall ofn)
      (condition-case _
          (citre-jump)
        (error (funcall ofn))))))

(defun my--push-point-to-xref-marker-stack (&rest _r)
  (xref-push-marker-stack (point-marker)))

(dolist (func '(find-function citre-jump))
  (advice-add func :before 'my--push-point-to-xref-marker-stack))

(provide 'icod-tags)

;;; icod-tags.el ends here
