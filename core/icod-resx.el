;;; icod-resx.el --- Resource file -*- lexical-binding: t -*-

;;; Code:

(x sgml-mode
   :mode "\\.\\(props\\|targets\\|resx\\)\\'"
   :init
   (setq sgml-basic-offset 4))

(x nxml-mode
   :init
   (setq nxml-child-indent 4 nxml-attribute-indent 4)

   (defun nxml-where ()
     "Display the hierarchy of XML elements the point is on as a path."
     (interactive)
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

   ;; show /x/y/z in modeline
   (defun:hook nxml-mode-hook ()
     (add-hook 'which-func-non-auto-modes 'nxml-mode)
     (which-function-mode t)
     (add-hook 'which-func-functions 'nxml-where t t)))

(x yaml-mode)

(x properties-mode
   "Java Properties file."
   :mode "\\.properties\\'"
   :ref "iquiw/properties-mode")

(provide 'icod-resx)

;;; icod-resx.el ends here
