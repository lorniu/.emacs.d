;;; eglot-volar.el --- Volar: Vue language server -*- lexical-binding: t -*-

;; Volar is the replacemenet of vls (Vetur)
;;
;;  https://github.com/vuejs/language-tools
;;  sudo npm i @volar/vue-language-server -g

;;; Code:

(require 'eglot)

(defclass eglot-volar-server (eglot-lsp-server) ()
  :documentation "Volar, the Vue language server.")

(defvar eglot-volar-program "vue-language-server")
(defvar eglot-volar-server-mode (if (memq system-type '(cygwin windows-nt ms-dos)) 2 0))

(defun eglot-volar-tsdk-path ()
  (let (tsdk-path)
    (let ((path (expand-file-name "node_modules/typescript/lib" (project-root (project-current)))))
      (when (file-exists-p path)
        (setq tsdk-path path)))
    (when-let* ((dir (and (null tsdk-path) (executable-find "tsserver"))))
      (setq tsdk-path
            (expand-file-name
             (concat (if (memq system-type '(cygwin windows-nt ms-dos)) "../" "../../lib/") "node_modules/typescript/lib")
             dir)))
    (and tsdk-path (file-exists-p tsdk-path) tsdk-path)))

(cl-defmethod eglot-initialization-options ((server eglot-volar-server))
  ;; https://github.com/volarjs/volar.js/blob/master/packages/language-server/src/types.ts
  `( :serverMode ,eglot-volar-server-mode ; I don't know why ServerMode.Semantic not working on Windows..
     :diagnosticModel 1
     :textDocumentSync 2
     :typescript (:tsdk ,(eglot-volar-tsdk-path))
     ))

(defun eglot-volar-contract (_interactive)
  (unless (executable-find eglot-volar-program)
    (user-error "You should install Volar first:  sudo npm install @volar/vue-language-server -g"))
  (unless (eglot-volar-tsdk-path)
    (user-error "Please install typescript or add typescript to the project"))
  (cons 'eglot-volar-server (list eglot-volar-program "--stdio")))

(add-to-list 'eglot-server-programs '((vue-mode ivue-mode) . eglot-volar-contract))

(provide 'eglot-volar)

;;; eglot-volar.el ends here
