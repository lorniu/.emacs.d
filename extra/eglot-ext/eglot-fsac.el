;;; eglot-fsac.el --- fsharp language server -*- lexical-binding: t -*-

;; fsautocomplete:
;;  - https://github.com/fsharp/FsAutoComplete
;;  - dotnet tool install -g fsautocomplete
;;
;; Issue exists, not solved, in eglot completion shows 'No Typecheck results' error, but lsp-mode not.
;;   - https://github.com/fsharp/emacs-fsharp-mode/issues/285
;;   - https://github.com/fsharp/emacs-fsharp-mode/issues/235
;; At last position the issue: (!eq (truename file) file), cause by the symbol link.
;;

;;; Code:

(require 'fsharp-mode)
(require 'eglot)

(defvar eglot-fsac-log-verbose nil)


;;; Contract

(defclass eglot-fsac-server (eglot-lsp-server) ()
  :documentation "FSharp Language Server.")

(cl-defmethod eglot-initialization-options ((_server eglot-fsac-server))
  `(:automaticWorkspaceInit t))

(cl-defmethod jsonrpc-connection-send :after ((server eglot-fsac-server) &key _id method _params)
  (when (eql method :initialized)
    (jsonrpc-notify
     server :workspace/didChangeConfiguration
     `(:settings
       (:FSharp
        (:KeywordsAutocomplete t
         :ExternalAutocomplete nil
         :Linter t
         :UnionCaseStubGeneration t
         :UnionCaseStubGenerationBody "failwith \"Not Implemented\""
         :RecordStubGeneration t
         :RecordStubGenerationBody "failwith \"Not Implemented\""
         :InterfaceStubGeneration t
         :InterfaceStubGenerationObjectIdentifier "this"
         :InterfaceStubGenerationMethodBody "failwith \"Not Implemented\""
         :UnusedOpensAnalyzer t
         :UnusedDeclarationsAnalyzer t
         :SimplifyNameAnalyzer nil
         :ResolveNamespaces t
         :EnableReferenceCodeLens nil
         :GenerateBinlog nil))))))

(defun eglot-fsac-contract (_interactive)
  (let ((fsac "fsautocomplete"))
    (unless (executable-find fsac)
      (user-error "You should install fsac first:  dotnet tool install -g %s" fsac))
    (cons 'eglot-fsac-server
          (append `(,fsac "--adaptive-lsp-server-enabled")
                  (if eglot-fsac-log-verbose '("-v"))))))

(add-to-list 'eglot-server-programs '(fsharp-mode . eglot-fsac-contract))

(provide 'eglot-fsac)

;;; eglot-fsac.el ends here
