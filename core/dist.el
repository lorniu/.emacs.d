;;; dist.el --- Package Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(p/refresh-packages
    use-package
    bind-key key-chord delight

    ;; looking
    rainbow-delimiters
    page-break-lines
    rcirc-styles
    xterm-color
    posframe company-posframe ; childframe style
    all-the-icons

    ;; edit and utils
    session
    syntax-subword
    expand-region
    wgrep
    dired-dups
    engine-mode
    google-translate youdao-dictionary
    vlf ; view-large-file
    ztree ; dir diff
    vterm
    markdown-mode markdown-toc
    fold-this ; fold the region
    memory-usage

    ;; exporting
    graphviz-dot-mode
    gnuplot
    plantuml-mode
    auctex company-math company-auctex ; latex

    ;; org
    org-plus-contrib ; latest
    ob-restclient
    ox-pandoc
    org-superstar
    org-present
    org-roam org-roam-server company-org-roam

    ;; search and nav
    rg amx
    hydra ace-window
    selectrum selectrum-prescient

    ;; projects
    projectile
    treemacs
    company
    yasnippet
    magit git-timemachine ; git

    ;; lsp, eglot is another choice, slow slow slow
    lsp-mode dap-mode
    company-lsp lsp-treemacs lsp-ui

    ;; frontend
    web-mode company-web
    tide ; the best javascript backends now
    emmet-mode htmlize web-beautify
    yaml-mode sass-mode json-mode
    websocket
    know-your-http-well
    restclient company-restclient ; restful

    ;; languages
    c-eldoc cquery ; c/c++
    php-mode company-php ; php
    go-mode company-go ; go
    elpy ; python
    robe ; ruby
    erlang ; erlang
    alchemist ; elixir
    dante hindent company-ghc attrap ; haskell
    powershell csharp-mode lua-mode
    kotlin-mode clojure-mode groovy-mode ; jvm
    lsp-java ; better than ever
    scala-mode ; scala
    jdecomp ; java decompile, use idea's fernflower.jar

    ;; miscellaneous
    emms elfeed elfeed-org uuidgen)


;;; Use-Package

(setq use-package-enable-imenu-support t)
(setq use-package-form-regexp-eval
      `(concat ,(eval-when-compile
                  (concat "^\\s-*(" (regexp-opt '("use-package" "x" "require") t) "\\s-+\\("))
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
                                          calendar find-func format-spec org-macs org-compat org-faces
                                          org-entities org-list org-pcomplete org-src org-footnote org-macro ob
                                          org org-agenda org-capture ox-publish
                                          easymenu tree-widget with-editor lv
                                          (git-commit . git)
                                          (transient . git)
                                          (magit . git)
                                          (tide . node))
  "Packages to load incrementally."
  :type 'list)

(defun load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally."
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
            (progn (message "Incremental loading Done !")
                   (message ""))
          (run-with-idle-timer 0.75
                               nil #'load-packages-incrementally
                               packages t)
          (setq packages nil))))))

(add-hook-fun emacs-startup-hook/incrementally-load ()
  (run-with-timer 10 nil (lambda ()
                           (run-with-idle-timer ic/incremental-delay
                                                nil #'load-packages-incrementally
                                                (cdr ic/incremental-packages) t))))


(provide 'dist)

;;; dist.el ends here
