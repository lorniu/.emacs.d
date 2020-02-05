;;; imcoding.el --- Happy Coding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x yasnippet/ed
   :bind
   (:map yas-keymap
         ("C-m" . 'yas-next-field-or-maybe-expand))
   :init
   (setq yas-verbosity 2
         yas--basic-extras '(fundamental-mode)
         yas-snippet-dirs  (cl-remove-if 'null (list (loco "z.temp/snippets" t) (loce "snippets"))))

   (defun im/yas--clear-extra-mode ()
     (interactive)
     (dolist (mode yas--extra-modes)
       (unless (member mode yas--basic-extras)
         (yas-deactivate-extra-mode mode))))

   (add-hook-fun yas-minor-mode-hook ()
     (delight '((yas-minor-mode "" yasnippet)))
     (mapc 'yas-activate-extra-mode yas--basic-extras))

   (yas-global-mode 1))

(x company
   :ref "company-mode/company-mode"
   :after yasnippet
   :hook ((prog-mode . company-mode)
          (sqlplus-mode . company-mode)
          (org-mode . company-mode)
          (eshell-mode . company-mode))
   :bind (:map company-active-map
               ("C-c"   . company-abort)
               ([tab] . smarter-yas-expand-next-field-complete)
               ("TAB" . smarter-yas-expand-next-field-complete))
   :custom
   (company-lighter-base "")
   (company-idle-delay 0.1)
   (company-minimum-prefix-length 1)
   (company-require-match 'never)

   (company-dabbrev-downcase nil)
   (company-dabbrev-ignore-case nil)
   :init
   (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))

   (make-variable-buffer-local 'company-backends)
   (setq-default company-backends
                 '(company-bbdb company-clang company-cmake
                   ;; company-xcode company-eclim
                   company-capf company-files
                   (company-dabbrev-code company-gtags company-keywords)
                   company-dabbrev))

   (defun company-local-add (backend)
     (cl-pushnew backend company-backends)
     (company-with-yasnippet-support))

   (defun company-local-set (backends)
     (setq company-backends backends)
     (company-with-yasnippet-support))

   (defun company-with-yasnippet-support ()
     (setq company-backends (mapcar (lambda (backend)
                                      (unless (consp backend)
                                        (setq backend (list backend)))
                                      (if (or (member 'company-files backend)
                                              (member 'company-yasnippet backend))
                                          backend
                                        (append backend '(:with company-yasnippet))))
                                    company-backends)))

   (defun smarter-yas-expand-next-field-complete ()
     "Try to `yas-expand' and `yas-next-field' at current cursor position. If failed try to complete the common part with `company-complete-common'"
     (interactive)
     (if yas-minor-mode
         (let ((old-point (point))
               (old-tick (buffer-chars-modified-tick)))
           (yas-expand)
           (when (and (eq old-point (point))
                      (eq old-tick (buffer-chars-modified-tick)))
             (ignore-errors (yas-next-field))
             (when (and (eq old-point (point))
                        (eq old-tick (buffer-chars-modified-tick)))
               (company-complete-common))))
       (company-complete-common)))

   :config
   (add-hook-fun after-change-major-mode-hook/with-yas ()
     (company-with-yasnippet-support))

   (defun:override company-yasnippet--completions-for-prefix$ (prefix key-prefix tables)
     "Filter company-yasnipet candidates, by yasnipet CONDITION."
     (cl-mapcan
      (lambda (table)
        (let ((keyhash (yas--table-hash table)) res)
          (when keyhash
            (maphash
             (lambda (key value)
               (when (and (stringp key) (string-prefix-p key-prefix key))
                 (maphash
                  (lambda (name template)
                    (let ((condition (yas--template-condition template)))
                      (when (or (null condition) (eval condition))
                        (push (propertize key
                                          'yas-annotation name
                                          'yas-template template
                                          'yas-prefix-offset (- (length key-prefix) (length prefix)))
                              res))))
                  value)))
             keyhash))
          res))
      tables)))

(x posframe
   :init
   (with-eval-after-load 'company---disabled-now
     (setq company-posframe-lighter "")
     (company-posframe-mode 1)))


;;; Language Server Protocol

(x lsp-mode
   :init
   (defun lsp-and-future ()
     (interactive)
     (lsp)
     (add-hook (intern (format "%s-hook" (symbol-name major-mode))) 'lsp))
   (defun lsp-and-future-cancel ()
     (interactive)
     (lsp-mode -1)
     (remove-hook (intern (format "%s-hook" (symbol-name major-mode))) 'lsp))
   :config
   (setq lsp-auto-guess-root t)
   (setq lsp-diagnostic-package :auto))

(x company-lsp)

(x lsp-ui
   :after lsp-mode
   :config
   (setq lsp-ui-doc-enable nil
         lsp-ui-sideline-enable nil
         lsp-ui-flycheck-enable t))

(x dap-mode/x
   :after lsp-mode
   :config
   (dap-mode t)
   (dap-ui-mode t))


;;; Front-End
;;
;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;  - 20181120, Servers as Elnode is more powerful but too old. Simpled-Httpd is simple and enough.
;;  - 20181120, Use Livereload replace Impatient! Websocket has a better experience than iframe.
;;  - 20191026, try lsp, still tooooo slow! remove js2, web-mode is better.

(x web-mode/i
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|tpl\\|vue\\|tsx\\|jsx\\)\\'"

   :config
   (setq web-mode-markup-indent-offset    2
         web-mode-css-indent-offset       2
         web-mode-code-indent-offset      2
         web-mode-enable-css-colorization t
         web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'")))

   (add-hook-fun web-mode-hook ()
     (pcase (file-name-extension (or (buffer-file-name) ""))
       ("js" (im/tide-enable))
       ("jsx"
        (im/tide-enable)
        (flycheck-mode 1)
        (electric-pair-local-mode 1)
        (setq-local web-mode-enable-auto-quoting nil)
        (company-local-set '(company-files (company-tide :separate company-yasnippet) company-capf)))
       ("tsx"
        (im/tide-enable)
        (setq-local web-mode-enable-auto-quoting nil))
       (_
        (company-local-add 'company-web-html)
        (hs-minor-mode -1))))

   (defun:before yas--maybe-expand-key-filter$ (&rest _)
     "Yasnippet in in SCRIPT block."
     (when (equal major-mode 'web-mode)
       (im/yas--clear-extra-mode)
       (yas-activate-extra-mode
        (pcase (web-mode-language-at-pos)
          ("html"           'html-mode)
          ("css"            'css-mode)
          ("javascript"     'js2-mode)
          ("jsx"            'js2-mode))))))

(x tide/i
   "Make sure jsconfig.json or tsconfig.json under root of project."
   :delight " ť"
   :if (executable-find "node")
   :init
   (setq tide-default-mode "JS")
   (setq tide-sync-request-timeout 10)

   (defun im/tide-enable (&optional _)
     (interactive)
     (when (executable-find "node")
       (tide-setup)
       (eldoc-mode 1)
       (tide-hl-identifier-mode 1)
       (set (make-local-variable 'company-tooltip-align-annotations) t) t))

   :config
   (defun:around company-tide$ (f &rest args)
     "Complete with tide in SCRIPT block."
     (let ((tide-mode (or (derived-mode-p 'js2-mode)
                          (and (equal major-mode 'web-mode)
                               (or (string= (web-mode-language-at-pos) "javascript")
                                   (string= (web-mode-language-at-pos) "jsx"))))))
       (apply f args))))

(x json-mode
   :config
   (add-hook-fun json-mode-hook ()
     (make-local-variable 'js-indent-level)
     (setq js-indent-level 2)))

(x emmet-mode/d
   :hook (web-mode rjsx-mode mhtml-mode html-mode)
   :init (setq emmet-move-cursor-between-quotes t))

(x web-beautify
   :if (executable-find "js-beautify"))

(x livereload
   :commands (liveview liveload))

(x restclient
   :config
   (add-hook-fun restclient-mode-hook ()
     (company-mode 1))
   (company-local-add 'company-restclient))

(x nxml-mode
   :init
   (defun nxml-where ()
     "Display the hierarchy of XML elements the point is on as a path."
     (interactive)
     (let ((path nil))
       (save-excursion
         (save-restriction
           (widen)
           (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                       (condition-case nil
                           (progn (nxml-backward-up-element) t)
                         (error nil)))
             (setq path (cons (xmltok-start-tag-local-name) path)))
           (if (called-interactively-p t)
               (message "/%s" (mapconcat 'identity path "/"))
             (format "/%s" (mapconcat 'identity path "/")))))))

   ;; show /x/y/z in modeline
   (add-hook-fun nxml-mode-hook ()
     (add-hook 'which-func-non-auto-modes 'nxml-mode)
     (which-function-mode t)
     (add-hook 'which-func-functions 'nxml-where t t))

   ;; hs-minor-mode
   (hs-add-rule 'nxml-mode
                "<!--\\|<[^/>]*[^/]>" "-->\\|</[^/>]*[^/]>" "<!--"
                'nxml-forward-element))

(x mhtml-mode
   :init
   (hs-add-rule 'mhtml-mode
                "{\\|<[^/>]*?"
                "}\\|</[^/>]*[^/]>"
                "<!--"
                (lambda (_)
                  (pcase (get-text-property (point) `mhtml-submode)
                    (`nil (sgml-skip-tag-forward 1))
                    (`submode (forward-sexp))))))


;;; C Family

(x prog-mode
   :bind
   (:map prog-mode-map
         ("C-c C-u" . backward-up-list))
   :init
   (add-hook-fun prog-mode-hook ()
     (setq show-trailing-whitespace t)
     (abbrev-mode 1)
     (rainbow-delimiters-mode 1)
     (which-function-mode 1))
   :config
   (x eldoc/d)
   (x flycheck/i))

(x cc-mode/i
   "For C family languages, such as Java/C/Cpp..."
   :config
   (setq-default
    c-basic-offset 4
    gdb-many-windows t
    gdb-show-main t)

   (defun im/lsp-cquery-enable-prob ()
     (if (executable-find "cquery")
         (progn
           (require 'cquery)
           (lsp-ui-mode 1)
           (lsp)
           (when (>= emacs-major-version 26)
             (lsp-ui-doc-mode 1)))
       nil))

   (defun my-c-hook ()
     (c-turn-on-eldoc-mode)
     (flycheck-mode 1)
     (unless (im/lsp-cquery-enable-prob)
       ;; (semantic-mode)
       (cscope-minor-mode))
     (set (make-local-variable 'compile-command)
          (if (file-exists-p "Makefile")
              "make"
            (format "cc %s -g %s -o %s"
                    (buffer-name)
                    (or (getenv "CFLAGS") "-std=c99 -Wall")
                    (file-name-sans-extension (buffer-name))))))

   (defun my-cpp-hook ()
     (setq compile-command (if (file-exists-p "Makefile")
                               "make" "clang++ -Wall -Wextra -std=c++14")))

   (add-hook 'c-mode-hook 'my-c-hook)
   (add-hook 'c++-mode-hook 'my-c-hook)
   (add-hook 'c++-mode-hook 'my-cpp-hook))

(x cquery
   "If use lsp, need cquery support
   "
   "   $ yaourt -S cquery-git
   "
   "CEDET + ECB is a choice, and Irony for C++ is another choice.
   "
   :after (cc-mode)
   :config
   (setq cquery-cache-dir ".cqindex")
   (setq cquery-extra-args (list (format "--log-file=%scquery.log" temporary-file-directory)))
   (setq cquery-extra-init-params '(:cacheFormat "msgpack" :completion (:detailedLabel t) :xref (:container t)
                                    :diagnostics (:frequencyMs 5000)))
   (add-to-list 'projectile-globally-ignored-directories cquery-cache-dir))

(x semantic
   :after (cc-mode)
   :config
   (global-semanticdb-minor-mode 1)
   (global-semantic-idle-scheduler-mode 1)
   (global-semantic-stickyfunc-mode 1))


;;; Lisp Family

(x lisp-mode ; Slime/Sly
   "Choose one plan before you begin:
   "
   " 1. Install ros, then exec 'ros install sly' to initial the env."
   " 2. Install sbcl/ccl manually, then install `sly' package in emacs.
   "
   "Introduction:
   "
   "  - SLIME (The Superior Lisp Interaction Mode for Emacs) maybe the best IDE for lisp."
   "  - Sly is a fork and improved version of SLIME. I switched to it in 20190122."
   "  - Roswell is tool like 'pip/gem', it can help to maintain lisp env and convenience to exec lisp script.
   "
   :config

   ;; keep lisp-mode clean before interactive 'sly' command executed !
   (remove-hook 'lisp-mode-hook 'sly-editing-mode)

   (if (executable-find "ros") ; ros install sly
       (cl-loop with init-file = (expand-file-name ".roswell/helper.el" (if IS-WIN (getenv "USERPROFILE") "~/"))
          for cmd in '(sly sly-connect)
          do (autoload cmd init-file nil t))
     ;; When no roswell support, use inner sly and local lisp-bin
     (when (setq inferior-lisp-program (seq-find #'executable-find '("sbcl" "ccl" "clisp" "ecl")))
       (unless (package-installed-p 'sly) (package-install 'sly))
       (require 'sly)
       (add-hook 'sly-mode-hook '%lisp/ensure-quicklisp)))

   (setq org-babel-lisp-eval-fn 'sly-eval)
   (add-hook 'sly-mrepl-mode-hook '%lisp/init-repl-buffer)
   (add-hook 'sly-connected-hook '%lisp/after-connected)
   (add-hook 'sly-mode-hook '%sly-custom-company)

   (defun %lisp/init-repl-buffer ()
     (company-mode-on)
     (local-set-key (kbd "C-r") 'isearch-backward)
     (local-set-key (kbd "C-c M-o") 'sly-mrepl-clear-repl))

   (defun %lisp/after-connected ()
     (when (file-exists-p ic/lisp-init-file)
       (sly-eval-async
        `(cl:unless (cl:member :im-loaded cl:*features*)
                    (slynk:load-file (cl:merge-pathnames ,ic/lisp-init-file))))))

   (defun %lisp/ensure-quicklisp ()
     "Quicklisp installation and initalization."
     (let ((remote-url "http://beta.quicklisp.org/quicklisp.lisp")
           (local-url (expand-file-name "~/.quicklisp/quicklisp.lisp")))
       (unless (file-exists-p local-url)
         (files--ensure-directory (file-name-directory local-url))
         (url-copy-file remote-url local-url)
         (with-temp-buffer
           (insert-file-contents local-url)
           (goto-char (point-max))
           (insert (format "\n(quicklisp-quickstart:install :path \"%s\")" (file-name-directory local-url)))
           (insert "(let ((*do-not-prompt* t)) (ql:add-to-init-file))")
           (sly-eval-buffer)))))

   (defun %sly-custom-company ()
     (setq-local company-idle-delay 0.5)
     (setq-local company-transformers (list (apply-partially #'cl-remove-if (lambda (c) (string-match-p "[0-9]+" c)))))
     (company-local-add '(company-yasnippet :separate company-capf))))

;; Geiser is the slime of Scheme, but too heavy.
;; so use `run-scheme' function instead, enough for me.

(x cmuscheme
   "Run scheme with `run-scheme' command directly."
   :commands run-scheme)


;;; On JVM

;; use Ensime to support both Java and Scala Dev.
;; choose one build tool, maven/gradle or sbt
;; 2019-11-18, use lsp/Metals instead.

;; use Meghanada to support Java Dev, maybe a better choice?
;; or use Lsp-Java? Not so good currently.

;; use jdecomp-mode and cfr/fernflower to auto decompile/show .class file

(x lsp-java
   :init
   (setq lsp-java-server-install-dir (locc "lsp-server-java"))
   (setq lsp-java-save-action-organize-imports nil)

   (add-hook-fun java-mode-hook ()
     (require 'lsp-java)
     (require 'dap-java)
     (flycheck-mode 1)
     (company-mode 1)
     ;; arglist indent
     (c-set-offset 'arglist-intro '+)))

(x scala-mode
   "Can use [metals+lsp] to support Scala development:

First, build metals (https://scalameta.org/metals/):

  curl -L -o coursier https://git.io/coursier
  chmod +x coursier
  ./coursier bootstrap --java-opt -Xss4m --java-opt -Xms100m -130va-opt -Dmetals.client=emacs org.scalameta:metals_2.12:0.7.6 -r bintray:scalacenter/releases -r sonatype:snapshots -o /usr/local/bin/metals-emacs -f

  # or use ~/.emacs.d/bin/build-metals-for-scala.sh to build.

Then, enable it with 'lsp' command.

note: Ensime is deprecated.
   ")

(x jdecomp
   :init
   (setq jdecomp-decompiler-type 'fernflower)
   (setq jdecomp-decompiler-paths (list (cons 'fernflower (loce "share/fernflower.jar"))))
   (defun:override jdecomp--fernflower-decompile-file$ (file &optional extracted-p)
     (jdecomp--ensure-decompiler 'fernflower)
     (with-temp-buffer
       (let* ((classpath (or (file-name-directory file) default-directory))
              (destination (if extracted-p
                               (file-name-directory file)
                             (jdecomp--make-temp-file (concat "jdecomp"
                                                              (replace-regexp-in-string "\\(\\w\\):" "/\\1" (file-name-sans-extension file))) t))))
         ;; See: http://stackoverflow.com/a/39868281/864684
         (apply #'call-process "java" nil nil nil
                `(,@(jdecomp--decompiler-options 'fernflower)
                    "-cp" ,classpath
                    "-cp" ,(expand-file-name (jdecomp--decompiler-path 'fernflower))
                    "org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler"
                    ,file
                    ,destination))
         (insert-file-contents (cl-first (jdecomp--java-files destination)))
         (buffer-string))))
   (defun:override jdecomp-archive-hook-function$ ()
     "Fixup the path for Windows."
     (let ((arr (split-string (buffer-file-name) ":")) jar file)
       (setq file (car (last arr)))
       (setq jar (string-join (butlast arr) ":"))
       (when (and jdecomp-mode
                  (jdecomp--classfile-p file))
         (kill-buffer (current-buffer))
         (jdecomp-decompile-and-view file jar))))
   (define-derived-mode class-mode fundamental-mode "" "")
   (jdecomp-mode 1))



;; Haskell

(x haskell
   "Basic usage, with interactive-mode
   "
   "Advanced usage, with Intero, IDE-like, based on Stack:
   "
   "  pacman -S stack
   "
   "Intero will take a long time to initialize for the first time, download a lot!
   "
   "Get out, intero! Toooo much disk space used! Change to Dante 20180513, saved 3G space...
   "
   :init
   (setq haskell-tags-on-save nil
         haskell-process-log t
         haskell-process-show-debug-tips nil
         haskell-process-type 'auto
         haskell-process-suggest-remove-import-lines nil)

   (add-hook-fun haskell-mode-hook ()
     (interactive-haskell-mode)
     (flycheck-mode -1)
     (if (executable-find "cabal") (dante-mode)))

   (hs-add-rule 'haskell-mode
                "^[a-zA-Z]+" "" "--"
                (lambda (_)
                  (let ((beg (point)))
                    (if (and (equal (char-before) 10) (equal (char-after) 12))
                        (forward-line)
                      (forward-paragraph)
                      (search-forward-regexp "^[-a-zA-Z]" nil t)
                      (backward-char 2)
                      (if (>= beg (point)) (goto-char (point-max)))
                      (if (equal (char-before) 10) (backward-char))))))
   :config
   (define-key haskell-interactive-mode-map (kbd "C-c M-o") 'haskell-interactive-mode-clear)
   (define-key haskell-interactive-mode-map (kbd "C-a") 'haskell-interactive-mode-beginning))

(x dante
   :commands dante-mode
   :after haskell-mode
   :bind
   (:map hs-minor-mode-map ("C-c '" . dante-eval-block))
   :init
   (setq dante-repl-command-line  '("cabal" "new-repl"))
   (setq dante-load-flags '(;; defaults:
                            "+c"
                            "-Wwarn=missing-home-modules"
                            "-fno-diagnostics-show-caret"
                            ;; necessary to make attrap-attrap useful:
                            "-Wall"
                            ;; necessary to make company completion useful:
                            "-fdefer-typed-holes"
                            "-fdefer-type-errors"))
   (add-hook-fun dante-mode-hook ()
     (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))
   :config
   (unbind-key "C-c /" dante-mode-map)
   (require 'patch-haskell))

;; Erlang
;; - Distel is said a good IDE. try some day.
;; - other setups later too.

(x erlang
   :config
   (unbind-key "M-q" erlang-mode-map))

;; Elixir

(x alchemist
   :config
   (setq alchemist-hooks-test-on-save nil)
   (setq alchemist-hooks-compile-on-save nil))

;; Python-Elpy

(x python
   "Install these modules for more features
    - pip install jedi importmagic
    - pip install flake8 autopep8"

   :interpreter ("python" . python-mode)
   :init
   (setq python-indent-guess-indent-offset t)
   (setq python-indent-guess-indent-offset-verbose nil)
   :config
   (when (executable-find "python")
     (elpy-enable)
     (when IS-WIN
       (setq python-shell-completion-native-enable nil)
       (add-hook 'inferior-python-mode-hook 'im/local-encoding))))

;; Ruby-Mode

(x ruby-mode
   "Rinari is a collection to develop RoR
   "
   "Inf-ruby provide a REPL
   "
   "Robe-mode provide company backend.
   "
   "  - gem install pry
   "
   :hook ((ruby-mode . inf-ruby-minor-mode)
          (ruby-mode . robe-mode))
   :init
   (hs-add-rule 'ruby-mode
                "def\\|do\\|{" "end\\|}" "#"
                (lambda (_) (ruby-end-of-block)))
   :config
   (company-local-add 'company-robe)

   (x inf-ruby :config
      (define-key inf-ruby-minor-mode-map (kbd "C-c C-s")
        (lambdai  ;; open rails console, fails then irb
         (or (ignore-errors (inf-ruby-console-auto))
             (ignore-errors (inf-ruby)))
         (robe-start)))))

;; Golang

(x go-mode
   :hook ((go-mode . smartparens-mode))
   :config
   (x company-go
      :bind (:map go-mode-map ("M-." . godef-jump))))

;; PHP

(x php-mode
   :init
   (defun my-php-toggle-mode ()
     (interactive)
     (cond ((and buffer-file-name
                 (string-equal (file-name-extension buffer-file-name) "php"))
            (with-message nil
              (if (eq major-mode 'php-mode)
                  (web-mode)
                (php-mode)))
            (message "Toggled to %s" major-mode))))

   (global-set-key (kbd "C-c C-v") 'my-php-toggle-mode)

   :config
   (defun php-mark-as-root ()
     (interactive)
     (with-temp-file ".ac-php-conf.json"))

   (defun my-php-lookup ()
     (interactive)
     (let ((symbol (symbol-at-point)))
       (if (not symbol)
           (message "No symbol at point.")
         (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol))))))

   (if (executable-find "php") (require 'company-php))

   (add-hook-fun php-mode-hook ()
     (flycheck-mode 1)
     (electric-pair-local-mode 1)
     (local-set-key (kbd "<f1>") 'my-php-lookup)

     (when (featurep 'company-php)
       (ac-php-core-eldoc-setup) ;; enable eldoc
       (company-local-add 'company-ac-php-backend))))


(provide 'imcoding)

;;; imcoding.el ends here
