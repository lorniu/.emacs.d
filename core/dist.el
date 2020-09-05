;;; dist.el --- Package Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(p/refresh-packages
    ;; basic
    use-package bind-key

    ;; looking
    delight
    rainbow-mode rainbow-delimiters
    page-break-lines
    xterm-color
    erc-hl-nicks
    all-the-icons
    posframe company-posframe ; childframe style

    ;; edit and utils
    session
    syntax-subword
    wgrep
    dired-dups
    engine-mode
    youdao-dictionary
    vlf ; view-large-file
    ztree ; dir diff
    ;; vterm ; i will remove it, must be.
    fold-this ; fold the region
    memory-usage
    pdf-tools org-noter org-noter-pdftools ; read & note
    pyim ; chinese input method
    rg ; ripgrep
    mpv emms ; media

    ;; complete, search and nav
    selectrum consult marginalia embark which-key
    selectrum-prescient embark-consult

    ;; projects
    treemacs
    company
    yasnippet
    magit git-timemachine ssh-agency ; git
    ace-window

    ;; exporting
    gnuplot
    graphviz-dot-mode
    plantuml-mode
    auctex company-math company-auctex ; latex

    ;; org
    org-plus-contrib ; latest
    ob-restclient
    ox-pandoc
    org-present
    org-roam org-roam-server

    ;; lsp. slow slow slow
    lsp-mode dap-mode lsp-treemacs lsp-ui
    eglot ; another choice

    ;; frontend
    web-mode company-web
    tide ; the best javascript backends now
    emmet-mode htmlize web-beautify
    yaml-mode sass-mode
    websocket
    know-your-http-well
    restclient company-restclient ; rest

    ;; languages
    c-eldoc cquery ; c/c++
    php-mode company-php ; php
    go-mode company-go ; go
    elpy ; python
    robe ; ruby
    erlang ; erlang
    alchemist ; elixir
    dante hindent attrap ; haskell
    csharp-mode csproj-mode omnisharp ; C♯
    powershell lua-mode
    kotlin-mode clojure-mode groovy-mode ; jvm
    lsp-java ; better than ever
    scala-mode ; scala
    jdecomp ; java decompile, use idea's fernflower.jar
    markdown-mode markdown-toc ; md

    ;; miscellanueous
    uuidgen udev-mode
    bbdb)


;;; Use-Package

(setq use-package-enable-imenu-support t)
(setq use-package-form-regexp-eval
      `(concat ,(eval-when-compile
                  (concat "^\\s-*("
                          (regexp-opt
                           '("use-package" "x" "require")
                           t)
                          "\\s-+\\("))
               (or (bound-and-true-p lisp-mode-symbol-regexp) "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)"))

(require 'use-package)

(defmacro x (NAME &rest args)
  "Flags: e/demand d/delight i/incremental x/disabled -/ignore."
  (let* ((name-arr (split-string (symbol-name NAME) "/"))
         (name (intern (car name-arr)))
         (mode-name (replace-regexp-in-string "-mode" "" (car name-arr)))
         (flags (cadr name-arr))
         (doc-strings (cl-loop for i in args until (keywordp i) collect i))
         (x-options (list-nn (if (seq-contains flags ?e) :demand :defer) t
                             (if (seq-contains flags ?d) :delight)
                             (if (seq-contains flags ?x) :disabled)))
         (active-p (if (seq-contains flags ?-) nil t))
         (options (cl-set-difference args doc-strings))
         (refs (plist-get options :ref))
         (options* (progn (cl-remf options :ref)
                          (use-package-normalize-keywords name options))))
    (list-nn 'progn
             (if (seq-contains flags ?i)
                 (let ((c (plist-get options* :if)))
                   `(if ,(or (eq c nil) c) (load-packages-incrementally '(,name)))))
             (when doc-strings
               `(defhelper ,(intern mode-name) ,@doc-strings))
             (when refs
               `(defreference ,(intern mode-name) ,refs))
             (if active-p
                 `(use-package ,name ,@x-options ,@options)
               `(progn ,@(plist-get options* :init))) ; still run :init when inactive
             )))


;;; Lazy/Incremental Load

(defcustom ic/incremental-delay 10
  "Idle time to triggle incremental loading.")

(defcustom ic/incremental-packages '(dash f s timer
                                          calendar find-func format-spec
                                          org-macs org-compat org-faces
                                          org-entities org-list org-pcomplete
                                          org-src org-footnote org-macro ob
                                          org org-agenda org-capture ox-publish
                                          emacsql easymenu tree-widget lv
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
      (let ((req (pop packages)) (existp t))
        (when (consp req)
          (setq existp (executable-find (symbol-name (cdr req))))
          (setq req (car req)))
        (unless (or (featurep req) (not existp))
          (message "[loading] %s" req)
          (condition-case e
              (or (while-no-input
                    (let ((gc-cons-threshold most-positive-fixnum) file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            ((error debug)
             (message "Failed to load %S package incrementally, because: %s" req e))))
        (if (not packages)
            (progn (run-hooks 'load-packages-incrementally-hook)
                   (message "Incremental loading Done !")
                   (message ""))
          (run-with-idle-timer 0.75
                               nil #'load-packages-incrementally
                               packages t)
          (setq packages nil))))))

(defun-hook emacs-startup-hook/incrementally-load ()
  (run-with-timer 10 nil (lambda ()
                           (run-with-idle-timer ic/incremental-delay
                                                nil #'load-packages-incrementally
                                                (cdr ic/incremental-packages) t))))


;;; Misc

(x package
   :ref ("Melpa: https://melpa.org/#/"
         "Melpa Github: melpa/melpa")
   :init
   (defun-hook package-menu-mode-hook() (hl-line-mode 1))
   (defun p/elpa-clean-dup-packages (&optional dry-run)
     (interactive "P")
     (let* ((dirs (directory-files package-user-dir nil "-"))
            (packages (mapcar (lambda (d)
                                (string-match "\\(.*\\)\\(-[0-9].*\\)" d)
                                (cons (match-string 1 d)
                                      (subseq (match-string 2 d) 1)))
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
         (message "Nothing to clean.")))))


(provide 'dist)

;;; dist.el ends here
