;;; -*- lexical-binding: t -*-

;; 2021/12/15, remove tide, use lsp instead.

;;; Code:

(x mhtml-mode
   :config
   (defun:hook mhtml-mode-hook ()
     (setq-local sgml-basic-offset 2)))

(x css-mode
   :config
   (add-hook 'which-func-non-auto-modes 'css-mode))

(x emmet-mode/d
   :hook (web-mode rjsx-mode mhtml-mode html-mode)
   :config (setopt emmet-move-cursor-between-quotes t))

(x web-beautify
   ;; - XML (emacs native): M-x sgml-pretty-print
   ;; - XML (libxml2):      M-| xmllint --format -
   ;; - XML (web-beautify): M-x web-beautify-html
   )

(x liveload
   :commands (liveload liveload-and-view))


;;; Web-Mode

(x web-mode
   :ref "fxbois/web-mode"
   :mode "\\.\\(html?\\|.erb\\|.blade\\|[aj]sp\\|tpl\\|cshtml\\)\\'"
   :config
   (setopt web-mode-enable-auto-indentation nil
           web-mode-markup-indent-offset    2
           web-mode-css-indent-offset       4
           web-mode-code-indent-offset      4
           web-mode-enable-css-colorization t)
   (setopt web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
           web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                          ("ruby"   . "\\.html\\.erb\\'"))))

(defun:hook web-mode-hook/smart-capf ()
  (defvar-local ln:web-mode--last-lang nil)
  (defvar-local ln:web-mode--capf-cache nil)
  (add-hook 'post-command-hook
            (lambda ()
              (condition-case err
                  (let ((lang (web-mode-language-at-pos)))
                    (unless (string-equal ln:web-mode--last-lang lang)
                      (setq ln:web-mode--last-lang lang)
                      (let* ((mode (intern (format "%s-mode" lang)))
                             (funcs (or (cdr (assoc mode ln:web-mode--capf-cache))
                                        (let ((cfom (ln:capf-functions-of-mode mode)))
                                          (cl-pushnew (cons mode cfom) ln:web-mode--capf-cache :key #'car)
                                          cfom))))
                        (setq-local completion-at-point-functions funcs)
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

(define-derived-mode ivue-mode web-mode "WebVue" "Edit vue with web-mode")

(x ivue-mode
   "Volar is the replacement of vls (Vetur)\n\n  npm i @volar/vue-language-server -g"
   :ref "Volar: vuejs/language-tools"
   :mode "\\.vue\\'"
   :init (with-eval-after-load 'web-mode (require 'eglot-volar)))
