;;; dist.el --- Package Initialization -*- lexical-binding: t -*-

;; use-package -> leaf

;;; Code:

;; If not set here, error raised on Windows.
;; Maybe a BUG, someday test and remove it.
(setq package-user-dir (loce "elpa"))

(p/loads leaf leaf-keywords
  ;; face
  blackout ; lighter
  rainbow-mode rainbow-delimiters ; colorful
  page-break-lines
  xterm-color
  erc-hl-nicks
  all-the-icons ; icons
  posframe ; childframe
  nano-theme modus-themes gruvbox-theme srcery-theme ; themes

  ;; utils
  bbdb
  vundo
  session
  syntax-subword
  wgrep
  rg ; ripgrep
  dired-dups
  ztree ; dir diff
  engine-mode
  macrostep
  aes ; encrypt
  hide-lines ; like occur
  vlf ; view large file
  memory-usage
  pyim pyim-basedict rime sis ; ime
  mpv emms ; media
  keycast ; show key-pressed
  evil ; viper -> evil-local-mode
  cowsay ;; figlet ; ascii art
  kubernetes docker dockerfile-mode ; container
  magit git-timemachine ssh-agency git-modes ghub forge ; git
  uuidgen
  htmlize
  package-lint
  sx ; stackoverflow

  ;; complete, search, nav and project
  corfu cape
  hyperbole embark embark-consult
  vertico orderless consult marginalia
  which-key
  yasnippet
  treemacs
  ace-window
  gitignore-templates license-templates ; templates
  citre ; ctags
  editorconfig

  ;; org
  org org-contrib
  org-present
  org-roam org-roam-ui
  ox-pandoc
  pdf-tools org-noter org-noter-pdftools nov ; read & note

  ;; exporting
  gnuplot
  graphviz-dot-mode
  plantuml-mode
  auctex ; latex

  ;; lsp
  eglot consult-eglot
  lsp-mode dap-mode lsp-treemacs lsp-ui

  ;; mmm
  web-mode edit-indirect
  polymode poly-org poly-markdown

  ;; modes
  nhexl-mode ; binary
  emmet-mode web-beautify sass-mode ; html
  typescript-mode ob-typescript ; typescript
  websocket know-your-http-well ; http
  restclient ob-restclient httprepl ; rest
  c-eldoc cmake-mode ; c/c++
  rust-mode ; rust
  php-mode ; php
  go-mode ; go
  robe ; ruby
  erlang ; erlang
  alchemist ; elixir
  lua-mode ; lua
  haskell-mode hindent attrap ; haskell
  csharp-mode csproj-mode fsharp-mode sharper ob-fsharp ; dotnet
  kotlin-mode clojure-mode groovy-mode scala-mode ; jvm
  lsp-java ; better than ever
  jdecomp ; java decompile, use idea's fernflower.jar
  android-mode ; easy to run android tools
  markdown-mode markdown-toc ; md
  yaml-mode csv-mode
  sql-indent ; sql
  powershell ob-powershell
  systemd udev-mode ; for linux

  ;; freelazy
  go-translate
  ox-spectacle)


;;; x

(require 'leaf)
(require 'leaf-keywords)
(defreference leaf "conao3/leaf.el")
(leaf-keywords-init)

(defmacro x (NAME &rest args)
  "Flags: e/demand d/blackout i/incremental x/disabled -/ignore."
  (pcase-let* ((`(,name ,flags) (split-string (symbol-name NAME) "/"))
               (mode-name (replace-regexp-in-string "-mode" "" name))
               (doc-strings (cl-loop for i in args until (keywordp i) collect i))
               (options (cl-set-difference args doc-strings))
               (refs (prog1 (plist-get options :ref) (cl-remf options :ref)))
               (fopts (cl-loop for (c . p) in '((?d . (:blackout t))
                                                (?e . (:require t))
                                                (?x . (:disabled t)))
                               when (cl-find c flags) append p))
               (active-p (if (cl-find ?- flags) nil t))
               (name (intern name)))
    (delq nil
          `(progn
             (if idebug (message "Loading %s..." ',name))
             ,(if (cl-find ?i flags)
                  (let ((c (plist-get options :if)))
                    `(if ,(or (eq c nil) c) (load-packages-incrementally '(,name)))))
             ,(if doc-strings `(defhelper ,(intern mode-name) ,@doc-strings))
             ,(if refs `(defreference ,(intern mode-name) ,refs))
             ,(if active-p `(leaf ,name ,@fopts ,@options)
                ;; still run :init when inactive
                `(progn ,@(plist-get options :init)))))))


;;; Lazy/Incremental Load

(defcustom ic/incremental-packages '(timer calendar find-func format-spec
                                           org-macs org-compat org-faces
                                           org-entities org-list org-pcomplete
                                           org-src org-footnote org-macro ob
                                           org org-agenda org-capture ox-publish
                                           emacsql easymenu tree-widget
                                           (git-commit . git)
                                           (transient . git)
                                           (magit . git))
  "Packages to load incrementally."
  :type 'list)

(defun load-packages-incrementally (packages &optional now)
  "Register PACKAGES to be loaded incrementally."
  (if (not now)
      (setq ic/incremental-packages (append ic/incremental-packages packages))
    (while packages
      (let ((default-directory "~/")
            (gc-cons-threshold most-positive-fixnum)
            (req (pop packages)) (existp t))
        (when (consp req)
          (setq existp (executable-find (symbol-name (cdr req))))
          (setq req (car req)))
        (unless (or (featurep req) (not existp))
          (message "[loading] %s" req)
          (condition-case-unless-debug e
              (or (while-no-input
                    (let ((inhibit-message t) file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            (error
             (message "Failed to load %S package incrementally, because: %s" req e)))
          (if (not packages)
              (progn (run-hooks 'load-packages-incrementally-hook)
                     (message "Incremental loading Done !")
                     (message ""))
            (run-with-idle-timer 0.75 nil #'load-packages-incrementally packages t)
            (setq packages nil)))))))

(defun:hook emacs-startup-hook/incrementally-load ()
  (run-with-timer 10 nil (lambda () (run-with-idle-timer
                                     10 nil #'load-packages-incrementally (cdr ic/incremental-packages) t))))

(provide 'dist)

;;; dist.el ends here
