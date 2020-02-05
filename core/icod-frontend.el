;;; icod-frontend.el --- Front End -*- lexical-binding: t -*-

;; 2021/12/15, remove tide, use lsp instead.

;;; Code:

(x web-mode/i
   :ref "fxbois/web-mode"
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|tpl\\|vue\\|tsx\\|jsx\\|cshtml\\)\\'"
   :defer-config
   (setq web-mode-enable-auto-indentation nil
         web-mode-markup-indent-offset    2
         web-mode-css-indent-offset       4
         web-mode-code-indent-offset      4
         web-mode-enable-css-colorization t
         web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'")))

   (defun:hook web-mode-hook ()
     ;; per-file-type
     (pcase (file-name-extension (or (buffer-file-name) ""))
       ("jsx"
        (flycheck-mode 1)
        (electric-pair-local-mode 1)
        (setq-local web-mode-enable-auto-quoting nil))
       ("tsx"
        (setq-local web-mode-enable-auto-quoting nil))
       ("cshtml"
        (setq-local web-mode-indent-style 1)
        (setq-local web-mode-markup-indent-offset 4)
        (setq-local web-mode-script-padding 4)
        (setq-local web-mode-style-padding 4)
        (setq-local web-mode-code-indent-offset 4))
       (_
        (hs-minor-mode -1)))

     ;; smart-capf
     (defvar-local im-web-mode-capf-cache nil)
     (add-hook 'post-command-hook
               (lambda ()
                 (let* ((lang (web-mode-language-at-pos))
                        (mode (intern (format "%s-mode" lang)))
                        (fs (or (cdr (assoc mode im-web-mode-capf-cache))
                                (cdar (cl-pushnew (cons mode (im-capf-functions-of-mode mode)) im-web-mode-capf-cache :key #'car)))))
                   (setq major-mode mode)
                   ;;(cw mode fs)
                   (setq-local completion-at-point-functions fs)))
               nil t)))

(x mhtml-mode
   :init
   (defun:hook mhtml-mode-hook ()
     (setq-local sgml-basic-offset 2)))

(x css-mode
   :init
   (add-hook 'which-func-non-auto-modes 'css-mode))

(x emmet-mode/d
   :hook (web-mode-hook rjsx-mode-hook mhtml-mode-hook html-mode-hook)
   :init (setq emmet-move-cursor-between-quotes t))

(x web-beautify
   "
- XML (emacs native): M-x sgml-pretty-print
- XML (libxml2):      M-| xmllint --format -
- XML (web-beautify): M-x web-beautify-html
")

(provide 'icod-frontend)

;;; icod-frontend.el ends here
