;;; -*- lexical-binding: t -*-

;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;  - 20181120, Servers as Elnode is more powerful but too old. Simpled-Httpd is simple and enough.
;;  - 20181120, Use Livereload replace Impatient! Websocket has a better experience than iframe.
;;  - 20191026, try lsp, still tooooo slow! remove js2, web-mode is better.
;;  - 20240527, remove lsp-mode and lsp-bridge, eglot is enough now

;;; Code:

(xzz eglot
  :ref ("joaotavora/eglot"
        "Specification: https://microsoft.github.io/language-server-protocol/specifications/specification-current/")
  :config
  (setopt eglot-autoshutdown t)
  (require 'eglot-cls)  ; use csharp-ls for C# dev:      dotnet tool install -g csharp-ls
  (require 'eglot-fsac) ; use fsautocomplete for F# dev: dotnet tool install -g fsautocomplete
  (require 'eglot-jdt))

(defmacro eglot-set-server (mode bin &rest args)
  `(with-eval-after-load 'eglot
     (when (executable-find ,bin)
       (add-to-list 'eglot-server-programs '(,mode . (,bin ,@args)))
       (cons ',mode ',bin))))
