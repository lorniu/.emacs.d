;;; dist.el --- Packages -*- lexical-binding: t -*-

;; Set special packages demo:
;;
;;   (with-eval-after-load 'predist
;;     (p/special
;;      (sly   . "~/source/sly")
;;      (go-translate . "~/source/go-translate")
;;      (company . nil)
;;    ))

;; Logic of package.el:
;;
;;   1. find [package-user-dir] and [package-directory-list], add to [package-alist]
;;   2. load [package-archives] to [package-archive-contents] as all packages in elpa
;;   3. according [package-load-list], load pkg, refresh [load-path] and [package-activated-list]

;;; Code:

(defvar p/elpa-repos
  '(:origin
    (setq package-archives
          `(("gnu"    . "https://elpa.gnu.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("melpa"  . "https://melpa.org/packages/")))
    :ustc
    (setq package-archives
          `(("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
            ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
            ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    :tsinghua
    (setq package-archives
          `(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu")
            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu")
            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa")))))

(defun p/repo (&optional upstream)
  (interactive (list (intern (completing-read "package-archives: " (cl-loop for m in p/elpa-repos by #'cddr collect (symbol-name m)) nil t))))
  (unless upstream (setq upstream (car p/elpa-repos)))
  (eval (plist-get p/elpa-repos upstream))
  (if (called-interactively-p 'any) (message "%s" package-archives) (message "[repo:%s]" upstream)))

(defvar ic/elpa :origin)
(p/repo ic/elpa) ; init package-archives



(require 'package)
(setq package-user-dir (loce "elpa"))
(package-initialize)
(add-hook 'package-menu-mode-hook (lambda () (hl-line-mode 1)))

(cl-loop for root in (append ic/external-load-path-tops (list (loce "extra") (loce "site-lisp")))
         for dirs = (ignore-errors (directory-files root t))
         do (cl-loop for dir in dirs
                     if (and (not (eq (file-name-extension dir) "")) (file-directory-p dir)) do
                     (add-to-list 'load-path dir)))

(add-to-list 'custom-theme-load-path (loce "themes"))

(when NATIVECOMP
  (add-to-list 'native-comp-eln-load-path (locc "eln/"))
  (setq native-comp-bootstrap-deny-list (list ".*/subr--trampoline.*")))



(defmacro p/loads (&rest packages-required)
  `(let ((tip "\n(progn\n  (im/proxy (quote SOCK))\n  (setq package-check-signature nil)\n  (p/repo :origin)\n  (p/install)\n)\n"))
     (defun p/packages ()
       (cl-loop for o in ',packages-required
                for p = (if (listp o) (if (eval (cadr o)) (car o)) o)
                if (and p (not (package-disabled-p p nil))) collect p))
     (defun p/packages-lacks ()
       (cl-remove-if #'package-installed-p (p/packages)))
     (defun p/install (&optional dont-select)
       (interactive "P")
       (package-refresh-contents)
       (dolist (p (p/packages-lacks)) (package-install p dont-select))
       (message "Install Finished."))
     (when-let ((lacks (p/packages-lacks)))
       (if (= (length lacks) (length (p/packages)))
           (warn "Have you installed any packages?\n\nMaybe:\n%s" tip))
       (warn "Total %s packages missing:\n\n%s\n\nPlease install first:\n%s\n\n"
             (length lacks)
             (with-temp-buffer
               (insert (format "%s" lacks))
               (fill-region (point-min) (point-max))
               (buffer-string))
             tip))))

(p/loads (use-package (< emacs-major-version 29))
  delight ; lighter
  rainbow-mode rainbow-delimiters ; colorful
  page-break-lines
  xterm-color
  erc-hl-nicks
  all-the-icons ; icons
  posframe ; childframe
  nano-theme modus-themes gruvbox-theme srcery-theme ; themes

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
  (vterm nil) eat ; terminal emulator
  simple-httpd

  corfu cape
  hyperbole embark embark-consult
  vertico orderless consult marginalia
  which-key
  yasnippet gitignore-templates license-templates ; templates
  treemacs
  ace-window
  citre ; ctags
  editorconfig

  org org-contrib
  org-present
  org-roam org-roam-ui
  ox-pandoc
  pdf-tools org-noter org-noter-pdftools nov ; read & note

  gnuplot
  graphviz-dot-mode
  plantuml-mode
  auctex ; latex

  (eglot (< emacs-major-version 29)) consult-eglot
  lsp-mode dap-mode lsp-treemacs lsp-ui

  web-mode edit-indirect
  polymode poly-org poly-markdown

  nhexl-mode ; binary
  emmet-mode web-beautify sass-mode ; html
  ob-typescript ; typescript
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
  (csharp-mode (< emacs-major-version 29)) csproj-mode fsharp-mode sharper ob-fsharp ; dotnet
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


;;; x-wrapper

(require 'use-package)
(require 'bind-key)
(require 'delight)

(defmacro x (NAME &rest args)
  "Flags: e/demand d/delight i/incremental x/disabled."
  (pcase-let* ((`(,name ,flags) (split-string (symbol-name NAME) "/"))
               (mode-name (replace-regexp-in-string "-mode" "" name))
               (doc-strings (cl-loop for i in args until (keywordp i) collect i))
               (options (cl-set-difference args doc-strings))
               (refs (prog1 (plist-get options :ref) (cl-remf options :ref)))
               (fopts (delq nil
                            (list (if (seq-contains flags ?e) :demand :defer) t
                                  (if (seq-contains flags ?d) :delight)
                                  (if (seq-contains flags ?x) :disabled))))
               (name (intern name)))
    (delq nil
          `(progn
             (if idebug (message "Loading %s..." ',name))
             ,(if (cl-find ?i flags)
                  (let ((c (plist-get options :if)))
                    `(if ,(or (eq c nil) c) (load-packages-incrementally '(,name)))))
             ,(if doc-strings `(defhelper ,(intern mode-name) ,@doc-strings))
             ,(if refs `(defreference ,(intern mode-name) ,refs))
             (use-package ,name ,@fopts ,@options)))))


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


;;; Auxiliaries

(defmacro p/special (&rest pkg-list)
  "(p/special (delight . path) (simple-httpd . (1 5 1)) (company . nil))"
  `(progn
     ,@(cl-loop for (p . v) in pkg-list
                if (consp v) collect `(add-to-list 'package--builtin-versions '(,p ,@v)) ; pretend installed
                else if (stringp v) collect `(push ,v load-path) ; use this instead of elpa's one
                else if (null v) collect `(add-to-list 'package-load-list '(,p . nil)) ; dont load this
                else collect `',p)))

(defun p/clean-dups (&optional dry-run)
  (interactive "P")
  (let* ((dirs (directory-files package-user-dir nil "-"))
         (packages (mapcar (lambda (d)
                             (string-match "\\(.*\\)\\(-[0-9].*\\)" d)
                             (cons (match-string 1 d)
                                   (cl-subseq (match-string 2 d) 1)))
                           (cl-remove-if (lambda (d) (string-match-p "signed$" d)) dirs)))
         (packages-dup
          (cl-loop for p in packages
                   if (> (length (cl-remove-if-not (lambda (d) (string= (car p) (car d))) packages)) 1)
                   collect p))
         (packages-preserved
          (cl-remove-duplicates packages-dup :test 'string-equal :key 'car))
         (packages-should-removed
          (cl-set-difference packages-dup packages-preserved)))
    (if packages-should-removed
        (cl-loop for p in packages-should-removed
                 for d = (expand-file-name (concat (car p) "-" (cdr p)) package-user-dir)
                 do (progn
                      (message "Deleting %s %s" d (if dry-run "[dry-run]" ""))
                      (unless dry-run (delete-directory d t))))
      (message "Nothing to clean."))))

(provide 'dist)

;;; dist.el ends here
