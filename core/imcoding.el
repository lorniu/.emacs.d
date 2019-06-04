;;; imcoding.el --- Happy Coding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom lisp/init-file "~/.emacs.d/scripts/conf/sly.lisp"
  "Common Lisp init file."
  :type 'file :group 'imfine)



;;; Tools

(x company/w
   :hook ((prog-mode   . company-mode))
   :bind (:map company-active-map
               ("C-c"   . company-abort)
               ("<SPC>" . my-insert-blank))
   :config
   (setq company-minimum-prefix-length 1)
   (setq company-idle-delay 0.1)
   (setq company-lighter-base "")
   (defun my-insert-blank () (interactive) (company-abort) (insert " ")))

(x yasnippet/ed
   :bind
   (:map yas-keymap ("C-m" . 'yas-next-field-or-maybe-expand))
   :init
   (setq yas-verbosity 2
         yas-alias-to-yas/prefix-p nil
         yas--basic-extras '(fundamental-mode))

   (defun my-yas--clear-extra-mode ()
     (dolist (mode yas--extra-modes)
       (unless (member mode yas--basic-extras)
         (yas-deactivate-extra-mode mode))))

   (defun my-yasnippet-hook ()
     (delight '((yas-minor-mode "" yasnippet)))
     (mapc 'yas-activate-extra-mode yas--basic-extras))

   (add-hook 'yas-minor-mode-hook 'my-yasnippet-hook)

   (yas-global-mode 1))

(x auto-highlight-symbol
   :bind (:map ahs-mode-map
               ("M-p" . ahs-backward)
               ("M-n" . ahs-forward)
               ("M-r" . ahs-change-range))
   :init
   (setq ahs-idle-interval 0.3
         ahs-case-fold-search nil
         ahs-default-range 'ahs-range-defun
         ahs-exclude '(( ruby-mode . "\\_<\\(end\\)\\_>" )
                       ( prog-mode . "\\_<\\(t\\|nil\\)\\_>" )))
   (autoload 'global-ahs-mode "auto-highlight-symbol")
   (global-ahs-mode 1))



;;; Lsp-Mode

(x lsp-mode
   :config
   (setq lsp-auto-guess-root t)
   (setq lsp-session-file (concat _CACHE_ ".lsp-session-v1")))

(x dap-mode/x)

(x lsp-ui
   :init
   (setq lsp-ui-sideline-show-symbol nil)
   (setq lsp-ui-sideline-show-hover nil)
   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
   (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

(x company-lsp
   :config
   (setq company-lsp-async t)
   (setq company-lsp-cache-candidates nil)
   (setq company-lsp-enable-snippet t)
   (setq company-transformers nil)
   (push 'company-lsp company-backends))



;;; Front-End
;;
;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;  - 20181120, Servers as Elnode is more powerful but too old. Simpled-Httpd is simple and enough.
;;  - 20181120, Use Livereload replace Impatient! Websocket has a better experience than iframe.

(x livereload
   :commands (liveview liveload))

(x web-mode/w
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|tpl\\|css\\|vue\\)\\'"

   :config
   (setq web-mode-markup-indent-offset    2
         web-mode-css-indent-offset       2
         web-mode-code-indent-offset      2
         web-mode-enable-css-colorization t
         web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'")))

   (defun my-web-mode-hook ()
     (hs-minor-mode -1)
     ;; (flycheck-mode +1)
     (append-local 'company-backends
                   'company-tide 'company-css 'company-web-html)
     (pcase (file-name-extension (or (buffer-file-name) ""))
       ("js"  (im/tide-enable))
       ("jsx"
        (im/tide-enable)
        (rjsx-minor-mode 1) (set (make-local-variable 'web-mode-enable-auto-quoting) nil)
        (electric-pair-local-mode 1))))

   (add-hook 'web-mode-hook 'my-web-mode-hook)

   (defun my-yas-expand-extra (&rest _)
     "Yasnippet in in SCRIPT block."
     (when (equal major-mode 'web-mode)
       (my-yas--clear-extra-mode)
       (yas-activate-extra-mode
        (pcase (web-mode-language-at-pos)
          ("html"           'html-mode)
          ("css"            'css-mode)
          ("javascript"     'js2-mode)
          ("jsx"            'js2-mode)))))
   (advice-add 'yas--maybe-expand-key-filter :before 'my-yas-expand-extra))

(x emmet-mode/d
   :hook (web-mode rjsx-mode mhtml-mode)
   :init (setq emmet-move-cursor-between-quotes t))

(x js2-mode
   :mode "\\.js\\'"
   :config
   (setq js2-basic-offset 2
         js2-highlight-level 3
         js2-strict-missing-semi-warning nil
         js2-strict-inconsistent-return-warning nil)

   (defun my-js2-hook ()
     (setq mode-name "JS2")
     (electric-pair-local-mode 1)
     (flycheck-mode 1)
     (im/tide-enable))

   (add-hook 'js2-mode-hook 'my-js2-hook))

(x rjsx-mode
   :config
   (mapc (lambda (e) (define-key rjsx-mode-map e nil)) `(">" "<" ,(kbd "C-d")))
   (add-hook-lambda 'rjsx-mode-hook
     (electric-pair-local-mode 1)
     (append-local 'company-backends 'company-tide 'company-files)
     (set (make-local-variable 'emmet-expand-jsx-className?) t)))

(x json-mode
   :config
   (add-hook-lambda 'json-mode-hook
     (make-local-variable 'js-indent-level)
     (setq js-indent-level 2)))

(x web-beautify
   :if (executable-find "js-beautify"))

(x tide
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
       (set (make-local-variable 'company-tooltip-align-annotations) t) t))

   (defun im/tide-generate-config ()
     "Generate jsconfig file for tide server."
     (interactive)
     (let ((dest (propertize (concat default-directory "jsconfig.json") 'face '(:foreground "ForestGreen"))))
       (if (file-exists-p dest)
           (message "File %s already exists." dest)
         (with-temp-file dest
           (insert (json-encode
                    '((include . ["./**/*"])
                      (exclude . ["node_modules" ".git"])
                      (compilerOptions
                       (target . "es2017")
                       (allowSyntheticDefaultImports . t)
                       (noEmit . t)
                       (checkJs . t)
                       (jsx . "react")
                       (lib . ["dom" "es2017"])))))
           (json-pretty-print-buffer))
         (message "File %s Generated!" dest))))

   :config
   (defun tide-project-root ()
     "Project root folder determined based on the presence of tsconfig.json."
     (or tide-project-root
         (let ((root (or (locate-dominating-file default-directory "tsconfig.json")
                         (locate-dominating-file default-directory "jsconfig.json"))))
           (unless root
             (message "Using current %s as project root." (propertize default-directory 'face '(:foreground "ForestGreen")))
             (setq root default-directory))
           (let ((full-path (expand-file-name root)))
             (setq tide-project-root full-path) full-path))))

   (defun my-company-with-tide (f &rest args)
     "Complete with tide in SCRIPT block."
     (let ((tide-mode (or (derived-mode-p 'js2-mode)
                          (and (equal major-mode 'web-mode)
                               (or (string= (web-mode-language-at-pos) "javascript")
                                   (string= (web-mode-language-at-pos) "jsx"))))))
       (apply f args)))
   (advice-add 'company-tide :around 'my-company-with-tide))



;;; C Family

(x prog-mode
   :bind
   (:map prog-mode-map
         ("C-c C-u" . backward-up-list))
   :init
   (add-hook-lambda 'prog-mode-hook
     (setq show-trailing-whitespace t)
     (abbrev-mode 1)
     (rainbow-delimiters-mode 1)
     (which-function-mode 1))
   :config
   (x eldoc/d)
   (x flycheck/w))

(x compile :bind (([f9] . compile)))

(x cc-mode/w
   "For C family languages, such as Java/C/Cpp..."
   :config
   (setq-default
    c-basic-offset 4
    gdb-many-windows t gdb-show-main t)

   (defun im/lsp-cquery-enable-prob ()
     (if (executable-find "cquery")
         (progn
           (lsp-ui-mode 1)
           (lsp-cquery-enable)
           (when (>= emacs-major-version 26)
             (lsp-ui-doc-mode 1)))
       nil))

   (defun my-ccc-hook ()
     (c-turn-on-eldoc-mode)
     (flycheck-mode 1)
     (unless (im/lsp-cquery-enable-prob)
       (semantic-mode)
       (cscope-minor-mode))
     (set (make-local-variable 'compile-command)
          (if (file-exists-p "Makefile") "make"
            (format "cc %s -g %s -o %s"
                    (buffer-name)
                    (or (getenv "CFLAGS") "-std=c99 -Wall")
                    (file-name-sans-extension (buffer-name))))))

   (defun my-cpp-hook ()
     (setq compile-command (if (file-exists-p "Makefile")
                               "make" "clang++ -Wall -Wextra -std=c++14")))

   (add-hook 'c-mode-hook 'my-ccc-hook)
   (add-hook 'c++-mode-hook 'my-ccc-hook)
   (add-hook 'c++-mode-hook 'my-cpp-hook)

   (define-key c-mode-map (kbd "C-c C-c") 'compile)
   (define-key c-mode-map (kbd "C-c C-k") 'kill-compilation)
   (define-key c++-mode-map (kbd "C-c C-c") 'compile)
   (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation))

(x cquery
   "If use lsp, need cquery support
   "
   "   $ yaourt -S cquery-git
   "
   "CEDET + ECB is a choice, and Irony for C++ is another choice.
   "
   :after (cc-mode)
   :commands lsp-cquery-enable
   :config
   (setq cquery-cache-dir ".cqindex")
   (setq cquery-extra-args (list (format "--log-file=%scquery.log" temporary-file-directory)))
   (setq cquery-extra-init-params '(:cacheFormat "msgpack" :completion (:detailedLabel t) :xref (:container t)
                                                 :diagnostics (:frequencyMs 5000)))
   (require 'company-lsp)
   (add-to-list 'projectile-globally-ignored-directories cquery-cache-dir))

(x semantic
   :after (cc-mode)
   :config
   (setq semanticdb-default-save-directory (concat _CACHE_ "semanticdb"))
   (global-semanticdb-minor-mode 1)
   (global-semantic-idle-scheduler-mode 1)
   (global-semantic-stickyfunc-mode 1))



;; Java/Scala

;; use Ensime to support both Java and Scala Dev.
;; choose one build tool, ie. maven/gradle/sbt.

;; use Meghanada to support Java Dev, maybe a better choice.
;; or use Lsp-Java?

(x scala-mode
   "Can use `ensime' to support Scala.
   "
   "M-x `ensime' to connect current project.
   "
   "You should init your build tool, and generate .ensime file anyway:
   "
   "   // Download the build tool, eg, sbt:  "
   "   eww https://www.scala-sbt.org/download.html
   "
   "   // Create project via sbt/gradle...  "
   "   sbt new scala/scala-seed.g8   \n   cd hello
   "
   "   // Generate .ensime with ensimePlugin/gradlePlugin "
   "   echo 'addSbtPlugin(\"org.ensime\" % \"sbt-ensime\" % \"2.5.1\")' > ~/.sbt/1.0/plugins/plugins.sbt   \n   sbt ensimeConfig
   "
   )

(x ensime
   :config
   (setq ensime-company-minimum-prefix-length 0)
   (setq ensime-graphical-tooltips t)
   (setq ensime-eldoc-hints 'all)
   (setq ensime-startup-notification nil))

(x lsp-java
   :init
   (setq lsp-java-server-install-dir (concat _CACHE_ "lsp-server-java"))

   (add-hook-lambda 'java-mode-hook
     (require 'lsp-java)
     ;; arglist indent
     (c-set-offset 'arglist-intro '+)))



;;; Language World

;; Slime/Sly

(x lisp-mode
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
       (loop with init-file = (expand-file-name ".roswell/helper.el" (if (env-windows) (getenv "USERPROFILE") "~/"))
             for cmd in '(sly sly-connect)
             do (autoload cmd init-file nil t))
     ;; When no roswell support, use inner sly and local lisp-bin
     (when (setq inferior-lisp-program (seq-find #'executable-find '("sbcl" "ccl" "clisp" "ecl")))
       (unless (package-installed-p 'sly) (package-install 'sly))
       (x sly :config (add-hook 'sly-mode-hook '%lisp/ensure-quicklisp))))

   (setq org-babel-lisp-eval-fn 'sly-eval)
   (add-hook 'sly-mrepl-mode-hook '%lisp/init-repl-buffer)
   (add-hook 'sly-connected-hook '%lisp/after-connected)

   (defun %lisp/init-repl-buffer ()
     (company-mode-on)
     (local-set-key (kbd "C-r") 'isearch-backward)
     (local-set-key (kbd "C-c M-o") 'sly-mrepl-clear-repl))

   (defun %lisp/after-connected ()
     (when (file-exists-p lisp/init-file)
       (sly-eval-async
           `(cl:unless (cl:member :im-loaded cl:*features*)
                       (slynk:load-file (cl:merge-pathnames ,lisp/init-file))))))

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
           (sly-eval-buffer))))))

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

   (add-hook-lambda 'haskell-mode-hook
     (interactive-haskell-mode)
     (flycheck-mode -1)
     (if (executable-find "cabal") (dante-mode)))

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
   (add-hook-lambda 'dante-mode-hook
     (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))
   :config
   (patch/haskell))

;; Erlang
;; - Distel is said a good IDE. try some day.
;; - other setups later too.

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
     (env-windows
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
   :config
   (append-local 'company-backends 'company-robe)

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
   :config
   (defun my-php-lookup ()
     (interactive)
     (let ((symbol (symbol-at-point)))
       (if (not symbol)
           (message "No symbol at point.")
         (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol))))))

   (defun my-php-stuff()
     (flycheck-mode 1)
     (electric-pair-local-mode 1)
     (local-set-key (kbd "<f1>") 'my-php-lookup)

     (when (executable-find "php")
       (x company-php :config
          (ac-php-core-eldoc-setup) ;; enable eldoc
          (append-local 'company-backends 'company-ac-php-backend))))

   (add-hook 'php-mode-hook 'my-php-stuff))


(provide 'imcoding)

;;; imcoding.el ends here