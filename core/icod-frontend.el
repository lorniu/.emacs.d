;;; icod-frontend.el --- Front End -*- lexical-binding: t -*-

;; 2021/12/15, remove tide, use lsp instead.

;;; Code:

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
   ;; - XML (emacs native): M-x sgml-pretty-print
   ;; - XML (libxml2):      M-| xmllint --format -
   ;; - XML (web-beautify): M-x web-beautify-html
   )


;;; Web-Mode

(x web-mode/i
   :ref "fxbois/web-mode"
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|tpl\\|tsx\\|jsx\\|cshtml\\)\\'"
   :defer-config
   (setq web-mode-enable-auto-indentation nil
         web-mode-markup-indent-offset    2
         web-mode-css-indent-offset       4
         web-mode-code-indent-offset      4
         web-mode-enable-css-colorization t)
   (setq web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'"))))

(defun:hook web-mode-hook/smart-capf ()
  (defvar-local im-web-mode--last-lang nil)
  (defvar-local im-web-mode--capf-cache nil)
  (add-hook 'post-command-hook
            (lambda ()
              (condition-case err
                  (let ((lang (web-mode-language-at-pos)))
                    (unless (string-equal im-web-mode--last-lang lang)
                      (setq im-web-mode--last-lang lang)
                      (let* ((mode (intern (format "%s-mode" lang)))
                             (funcs (or (cdr (assoc mode im-web-mode--capf-cache))
                                        (let ((cfom (im-capf-functions-of-mode mode)))
                                          (cl-pushnew (cons mode cfom) im-web-mode--capf-cache :key #'car)
                                          cfom))))
                        (setq-local completion-at-point-functions funcs)
                        (if (ignore-errors lsp-mode) (lsp-completion-mode 1)
                          (ignore-errors (lsp-completion-mode -1)))
                        (when (ignore-errors eglot--managed-mode)
                          (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)))))
                (error (message "[error] web-mode-post-hook: %s" err))))
            nil t))

(defun:hook web-mode-hook/per-file-type-config ()
  (pcase (file-name-extension (or (buffer-file-name) ""))
    ("jsx"
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
    (_ (hs-minor-mode -1))))


;;; Vue.js

;;  1. web-mode
;;  2. polymode
;;  3. vue-mode (not so good)

(defcustom ic/vue-mode 'imvue-mode "Which style to use for editing Vue.js" :type 'symbol)

(add-to-list 'auto-mode-alist `("\\.vue\\'" . ,ic/vue-mode))

(define-derived-mode imvue-mode web-mode "WebVue" "Edit vue with web-mode")

;; sudo npm install -g vls
(eglot-set-server imvue-mode "vls")

(provide 'icod-frontend)

;;; icod-frontend.el ends here
