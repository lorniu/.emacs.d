;;; eglot-cls.el --- csharp-ls -*- lexical-binding: t -*-

;;
;;  - https://github.com/razzmatazz/csharp-language-server
;;  - dotnet tool install -g csharp-ls
;;

;;; Code:

(require 'eglot)

(defvar eglot-cls-solution-file nil)


;;; Contact

(defclass eglot-cls-server (eglot-lsp-server) ()
  :documentation "CSharp Language Server.")

(cl-defmethod eglot-initialization-options ((_server eglot-cls-server))
  `(:automaticWorkspaceInit t))

(cl-defmethod eglot-handle-notification ((_server eglot-cls-server) (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics &allow-other-keys)
  "Suppress some boring diagnostic messages."
  (let* ((fn (lambda (diag)
               (let ((msg (plist-get diag :message)))
                 (string-match-p "obj/cls-metadata.*conflicts with the imported type" msg))))
         (diags (cl-delete-if fn diagnostics)))
    (cl-call-next-method _server _method :uri uri :diagnostics diags)))

(defun eglot-cls-contact (_interactive)
  (let ((cmd (append (list "csharp-ls")
                     (when eglot-cls-solution-file
                       (list "-s" eglot-cls-solution-file)))))
    (cons 'eglot-cls-server cmd)))

(add-to-list 'eglot-server-programs '(csharp-mode . eglot-cls-contact))


;;; URI resolver

(defun eglot-cls-resolve-uri (uri)
  (cl-assert (string-prefix-p "csharp:" uri))
  (let* ((buffer-name (file-name-nondirectory uri))
         (file-location (concat (project-root (project-current)) "obj/cls-metadata/" buffer-name)))
    (unless (file-readable-p file-location)
      (let ((path (file-name-directory file-location)))
        (unless (file-directory-p path)
          (make-directory path t)))
      (let ((res (jsonrpc-request (eglot--current-server-or-lose) :csharp/metadata `(:textDocument (:uri ,uri))))
            (meta-location (concat (file-name-sans-extension file-location) ".metadata-uri")))
        (with-temp-file file-location
          (insert (plist-get res :source)))
        (with-temp-file meta-location
          (insert uri))))
    file-location))

(defun eglot-cls-handle-uri (fn url)
  (if (and (stringp url)
           (string-prefix-p "csharp:" url))
      (eglot-cls-resolve-uri url)
    (funcall fn url)))

(advice-add 'eglot--uri-to-path :around #'eglot-cls-handle-uri)

(provide 'eglot-cls)

;;; eglot-cls.el ends here
