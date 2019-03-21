;;;; immor.el --- Modules Configuration
;;; Commentary:

;; Download Emacs Mirror:
;; http://mirrors.ustc.edu.cn/gnu/emacs/windows/

;;; Code:

;;; Auto-Mode

(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.\\(xml\\|xsl\\)\\'" . sgml-mode)
        ("\\.class\\'"           . class-mode)
        ("\\.scm\\'"             . scheme-mode)
        ("\\.\\(ba\\)?sh\\'"     . sh-mode)))

;;; Common-Hooks

((lambda ()

   (defun -my/before-open ()
     ;; large file
     (when (> (buffer-size) (* 2 1024 1024))
       (setq buffer-read-only t)
       (buffer-disable-undo)
       (fundamental-mode))
     ;; system file
     (and (not (string-match-p "/usr/home\\|.cache/\\|temp/\\|tmp/\\|vvv/" buffer-file-name))
          (not (string-match-p "\\(autoloads\\|loaddefs\\).el$" buffer-file-name))
          (let ((case-fold-search nil))
            (string-match-p "^/usr/\\|.emacs.d/packages\\|/emacs/\\|.roswell/lisp/quicklisp" buffer-file-name))
          (view-mode 1)))

   (defun -my/before-save ()
     (unless (seq-contains '(org-mode) major-mode)
       (delete-trailing-whitespace)))

   (defun -my/el-compile (&optional dir)
     (save-window-excursion
       (when (and (eq major-mode 'emacs-lisp-mode)
                  (string-match-p "^[a-z]" (buffer-name))
                  (string-match-p "\\/\\.emacs\\.d\\/\\(core\\|extra\\)\\/" (buffer-file-name)))
         (byte-compile-file (buffer-file-name)))))

   (defun -my/idle-once ()
     (let ((then (current-time)))
       (message "∵ [%s] Idle loading..." (time then 2))
       (mapc 'require im/need-idle-loads)
       (princ im/need-idle-loads)
       (message "∴ [%s] Idle loaded, %.2fs elapsed."
                (time nil 2) (time-subtract-seconds (current-time) then)))
     (remove-hook 'auto-save-hook '-my/idle-once))

   (add-hook 'find-file-hook    '-my/before-open)
   (add-hook 'before-save-hook  '-my/before-save)
   (add-hook 'after-save-hook   '-my/el-compile)
   (add-hook 'auto-save-hook    '-my/idle-once)))


;;;; Basic-Modes

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(setq desktop-path `(,_CACHE_ "."))

(setq auto-revert-mode-text "")
(global-auto-revert-mode)

(setq syntax-subword-skip-spaces t)

(setq recentf-max-saved-items 40)
(setq recentf-save-file (concat _CACHE_ "_recentf"))
(recentf-mode 1)

(winner-mode 1)

(show-paren-mode 1)

(x beacon/d :init (beacon-mode 1))

;;; Ediff

(x ediff :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))

;;; Auto-Highlight-Symbol

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

;;; Page Line Break

(x page-break-lines/e
   :init
   (setq page-break-lines-lighter "")
   (nconc page-break-lines-modes '(web-mode css-mode))
   (global-page-break-lines-mode))

;;; Isearch/Anzu/Occur

(x anzu/d :init (global-anzu-mode))

(with-eval-after-load "isearch"
  (bind-key "\C-e" ; Superword-Mode when search
            (lambda ()
              (interactive)
              (superword-mode 1)
              (isearch-yank-word-or-char)
              (superword-mode -1)) isearch-mode-map)
  (bind-key "M-s o" ; Word under cursor first
            (lambda (str)
              (interactive (list (aif (symbol-at-point) (symbol-name it))))
              (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))
  (advice-add 'occur :after ; After Ocur, jump to the result buffer.
              (lambda (&rest _) (aif (get-buffer-window "*Occur*") (select-window it)))))

;;; Ibuffer

(x ibuffer/e :config
   (setq ibuffer-show-empty-filter-groups nil
         ibuffer-saved-filter-groups
         `(("default"
            ("apple"
             (or (name . "\\*.+\\*")
                 (name . "\\*magit")))
            ("banana"
             (or (mode . dired-mode)))
            ("orange"
             (or (mode . org-mode)))
            ("melon"
             (or (mode . erc-mode)
                 (mode . rcirc-mode)))
            ("grape"
             (or (predicate . (eq (get major-mode 'derived-mode-parent) 'prog-mode))
                 (predicate . (member major-mode '(nxml-mode sgml-mode))))))))

   (add-hook-lambda 'ibuffer-mode-hook
     (ibuffer-switch-to-saved-filter-groups "default")))

;;; Dired/Neotree/Ivy/Swiper/Projectile

(x wdired/e
   :bind
   (:map dired-mode-map
         ( "6"  . dired-up-directory )
         ( "z"  . dired-du-mode )
         ( "r"  . wdired-change-to-wdired-mode )
         ( "."  . imdra-dired/body ))
   :config
   (setq dired-dwim-target t)
   (setq dired-du-size-format 'comma)
   ;; sort style
   (if (env-linux)
       (setq dired-listing-switches "-alh --group-directories-first")
     (require 'ls-lisp)
     (setq ls-lisp-dirs-first t)
     (setq ls-lisp-UCA-like-collation nil)))

(x wgrep/e :init
   (setq wgrep-enable-key "r"))

(x neotree/x :init
   (setq neo-theme 'arrow
         neotree-smart-optn t
         neo-window-fixed-size nil))

(x ivy/d
   :bind
   (:map ivy-minibuffer-map
         ("C-j" . ivy-done)
         ("C-m" . ivy-alt-done))
   :config
   (setq ivy-use-virtual-buffers t)
   (setq smex-save-file (concat _CACHE_ ".smex-items")) ;; frequently used cmds
   (ivy-mode 1))

(x swiper/w)

(x counsel-projectile/w
   :init
   (setq projectile-mode-line-prefix ""
         projectile-completion-system 'ivy
         projectile-cache-file (concat _CACHE_ "__projectile.cache")
         projectile-known-projects-file (concat _CACHE_ "__projectile-bookmark.eld")
         projectile-mode-line '(:eval (propertize (format " [%s]" (projectile-project-name)) 'face '(:foreground "darkred"))))

   (projectile-mode 1)
   (counsel-projectile-mode 1)

   :config
   (setq counsel-find-file-ignore-regexp "Volume\\|RECYCLE.BIN\\|\\.class$\\|\\.jar$\\|out/.*/classes/\\|build/libs")
   (setq projectile-globally-ignored-file-suffixes '(".class" ".jar" ".exe" ".dll" ".so"))
   (add-to-list 'projectile-globally-ignored-directories ".gradle")
   (add-to-list 'projectile-project-root-files ".pro")
   (add-to-list 'projectile-project-root-files "package.json"))

;;; HS-Minor-Mode/Outline-Minor-Mode

(x hideshow
   :delight hs-minor-mode
   :hook ((prog-mode) . hs-minor-mode)
   :bind* (:map hs-minor-mode-map
                ([(M-down-mouse-1)] . nil)
                ([(M-mouse-1)] . hs-mouse-toggle-hiding)
                ("C-c f"   . hs-toggle-hiding)
                ("C-c F"   . -my/hs-toggle-all)
                ("C-c C-f" . imdra-hs/body))
   :config

   (defun -my/hs-toggle-all ()
     (interactive)
     (if (seq-find
          (lambda (ov) (and (overlayp ov) (overlay-get ov 'hs)))
          (overlays-in (point-min) (point-max)))
         (hs-show-all)
       (hs-hide-all)))

   (defun -my/display-code-line-counts (ov)
     (when (eq 'code (overlay-get ov 'hs))
       (overlay-put ov 'face font-lock-warning-face)
       (overlay-put ov 'display
                    (format " ...%d..."
                            (count-lines (overlay-start ov)
                                         (overlay-end ov))))))

   (setq hs-set-up-overlay '-my/display-code-line-counts)

   (add-to-list
    'hs-special-modes-alist
    '(ruby-mode
      "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
      (lambda (arg) (ruby-end-of-block)) nil)))

;;; Hydra/Ace-Window

(x hydra :init
   (defvar hydra-stack nil)

   (defun hydra-push (expr)
     (push `(lambda () ,expr) hydra-stack))

   (defun hydra-pop ()
     (interactive)
     (let ((x (pop hydra-stack)))
       (when x (funcall x)))))

(x ace-window
   :bind ("C-x w" . ace-window)
   :config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; Shell

(x eshell/w
   :init

   (setq eshell-visual-subcommands
         '(("git" "log" "diff" "show")
           ("sudo" "vi" "visudo")))

   (setq comint-scroll-show-maximum-output nil
         eshell-scroll-show-maximum-output nil
         eshell-history-size    500
         eshell-hist-ignoredups t
         eshell-destroy-buffer-when-process-dies t
         eshell-directory-name (concat _CACHE_ "eshell")
         eshell-aliases-file "~/.emacs.d/resource/eshell.alias")

   :config

   ;; 256-color
   (setenv "TERM" "xterm-256color")
   (add-hook-lambda 'eshell-before-prompt-hook
     (setq xterm-color-preserve-properties t)
     (setq eshell-preoutput-filter-functions '(xterm-color-filter))
     (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

   ;;; mode hook
   (add-hook-lambda 'eshell-mode-hook
     (define-key eshell-mode-map (kbd "C-r") 'eshell/h))

   (defun eshell/h ()
     (interactive)
     (require 'em-hist)
     (let* ((start-pos (save-excursion (eshell-bol) (point)))
            (end-pos (point))
            (input (buffer-substring-no-properties start-pos end-pos))
            (command (ivy-read "Command: "
                               (delete-dups
                                (when (> (ring-size eshell-history-ring) 0)
                                  (ring-elements eshell-history-ring)))
                               :initial-input input)))
       (setf (buffer-substring start-pos end-pos) command)
       (end-of-line)))

   (defun eshell/ssh (&rest args)
     "Secure shell"
     (let ((cmd (eshell-flatten-and-stringify (cons "ssh" args)))
           (display-type (framep (selected-frame))))
       (cond
        ((and (eq display-type 't) (getenv "STY"))
         (send-string-to-terminal (format "\033]83;screen %s\007" cmd)))
        (t (apply 'eshell-exec-visual (cons "ssh" args)))))))

;;; Tramp

(x tramp/w
   "Common ssh CONNECTIONS:
   "
   " - M-x ssh-edit
   "
   :init
   (setq tramp-verbose 1)
   (setq tramp-default-user "root" tramp-default-method "scp")
   (setq remote-file-name-inhibit-cache nil)
   (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

   (setq
    tramp-my-hosts
    '(("vps.45" . "/sshx:vip@45.63.55.2:~/")
      ("ygmall" . "/sshx:Administrator@120.24.78.141:/c/soft/phpstudy/WWW/ygmall")))

   ;; shortcuts
   (defun ssh-edit ()
     "Shortcut for remote site."
     (interactive)
     (let* ((item (completing-read "Connect to: " (mapcar 'car tramp-my-hosts)))
            (host (or (cdr (assoc-string item tramp-my-hosts)) item)))
       (find-file host))))

;;; Edebug

(x edebug :init
   (add-hook-lambda 'edebug-mode-hook
     (view-mode -1)))

;;; View

(x view :bind
   (:map view-mode-map
         ( "h"     .  backward-char )
         ( "l"     .  forward-char  )
         ( "j"     .  next-line     )
         ( "k"     .  previous-line )
         ( "%"     .  his-match-paren )
         ( "<DEL>" .  nil )))

;;; Magit

(when (executable-find "git")
  (x git-auto-commit/e)

  (x magit/w
     :bind ("C-c m" . magit-status)
     :init (magit-auto-revert-mode -1)))

;;; Translate

(x youdao-dictionary/w
   "pacman -S mpg123
   "
   "youdao-dictionary-use-chinese-word-segmentation t
   "
   :init
   (setq url-automatic-caching t youdao-dictionary-search-history-file (concat _CACHE_ ".youdao")))

;;; Engine Search

(x engine-mode
   :init (engine-mode 1)
   :config

   (defengine google
     "https://google.com/search?q=%s")

   (defengine github
     "https://github.com/search?ref=simplesearch&q=%s")

   (defengine stackoverflow
     "http://stackoverflow.com/search?q=%s")

   (defengine wikipedia
     "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")

   (defengine arch-wiki
     "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go"
     :browser 'eww-browse-url)

   (defengine wolfram-alpha
     "http://www.wolframalpha.com/input/?i=%s")

   (defengine youtube
     "http://www.youtube.com/results?aq=f&oq=&search_query=%s"))


;;;; Programer - Common

(x eldoc/d)

(x prog-mode
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list))
   :init (add-hook-lambda 'prog-mode-hook
           (setq show-trailing-whitespace t)
           (abbrev-mode 1)
           (rainbow-delimiters-mode 1)
           (which-function-mode 1)))

;;; Flycheck/Flyspell

(x flycheck/w)

(x flyspell/x
   :preface (setq ispell-program-name "ispell") ;; apt install ispell
   :if (executable-find ispell-program-name)
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (rcirc-mode . flyspell-mode))
   :config (ispell-change-dictionary "american" t))

;;; Abbrev/Hippie-Expand/Company-Mode/Yasnippet

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-file-name-partially try-complete-file-name
        try-expand-all-abbrevs try-expand-list try-expand-line
        try-complete-lisp-symbol-partially try-complete-lisp-symbol))

(x abbrev/d :config
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file)))

(x company/w
   :hook ((prog-mode   . company-mode))
   :bind (:map company-active-map
               ("C-c"   . company-abort)
               ("<SPC>" . -my/insert-blank))
   :config
   (setq company-minimum-prefix-length 1)
   (setq company-idle-delay 0.1)
   (setq company-lighter-base "")
   (defun -my/insert-blank () (interactive) (company-abort) (insert " ")))

(x yasnippet/ed
   :bind
   (:map yas-keymap ("C-m" . 'yas-next-field-or-maybe-expand))
   :init
   (setq yas-verbosity 2
         yas-alias-to-yas/prefix-p nil
         yas--basic-extras '(fundamental-mode))

   (defun -my/yas--clear-extra-mode ()
     (dolist (mode yas--extra-modes)
       (unless (member mode yas--basic-extras)
         (yas-deactivate-extra-mode mode))))

   (defun -my/yasnippet-hook ()
     (delight '((yas-minor-mode "" yasnippet)))
     (mapc 'yas-activate-extra-mode yas--basic-extras))

   (add-hook 'yas-minor-mode-hook '-my/yasnippet-hook)

   (yas-global-mode 1))

;;; Compile Buffer

(x compile
   :bind (([f9] . compile)
          :map compilation-mode-map
          ("e" . (lambda () (interactive) (bury-buffer)
                   (awhen (get-buffer-window "*eshell*")
                     (let ((wstat (window-dedicated-p it)))
                       (set-window-dedicated-p it nil)
                       (aw-switch-to-window it)
                       (set-window-dedicated-p it wstat)))))))


;;;; Programer - Languages

;;; SQL-Client

(x sql/w
   "M-x: sql-connect/sql-postgres"

   :init
   (setq sql-connection-alist
         '((postgres/45
            (sql-product 'postgres)
            (sql-server "45.63.55.2")
            (sql-database "imdev")
            (sql-user "vip"))
           (mysql/45
            (sql-product 'mysql)
            (sql-server "45.63.55.2")
            (sql-port 3306)
            (sql-database "test")
            (sql-user "root"))))

   :config
   (add-hook 'sql-interactive-mode-hook 'im/set-buffer-mono-font)
   (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) *\\[[^ ]*\\]> *")

   (env-windows
    (setq sql-mysql-options '("-C" "-f" "-n" "-t"))
    (add-hook 'sql-interactive-mode-hook 'im/cp936-encoding)))

(x sqlplus
   "M-x: sqlplus
   "
   "SQLi for Oracle is not enough, for example, bad output format.
   "
   "Use this instead for oracle, util some days, merge its features to SQLi.
   "
   :commands sqlplus
   :init
   (if (env-windows) (setq sqlplus-process-encoding 'gbk) (setenv "NLS_LANG" "AMERICAN_AMERICA.UTF8"))
   (add-to-list 'auto-mode-alist '("\\.spl\\'" . sqlplus-mode))
   (setq sqlplus-session-cache-dir (concat _CACHE_ "sqlplus/"))
   :config
   (im/make-face-mono 'sqlplus-table-head-face 'sqlplus-table-odd-rows-face 'sqlplus-table-even-rows-face))

;;; CC-Mode

(x cc-mode/w
   "If use lsp, need cquery support
   "
   "   # pacman -S cquery-git
   "
   "CEDET + ECB is a choice, and Irony for C++ is another choice.
   "
   :config
   (setq-default c-basic-offset     4
                 gdb-many-windows   t
                 gdb-show-main      t)

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
     (setq compile-command (if (file-exists-p "Makefile") "make" "clang++ -Wall -Wextra -std=c++14")))

   (add-hook 'c-mode-hook 'my-ccc-hook)
   (add-hook 'c++-mode-hook 'my-ccc-hook)
   (add-hook 'c++-mode-hook 'my-cpp-hook)

   (define-key c-mode-map (kbd "C-c C-c") 'compile)
   (define-key c-mode-map (kbd "C-c C-k") 'kill-compilation)
   (define-key c++-mode-map (kbd "C-c C-c") 'compile)
   (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation))

(x cquery
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

;;; Slime/Sly

(x lisp-mode
   "SLIME (The Superior Lisp Interaction Mode for Emacs) maybe the best IDE for lisp.
   "
   "Sly is a fork of SLIME, improved a lot. Switched to it in 20190122.
   "
   "And Roswell is just like pip/gem, i.e: make it easy to use lisp script and so on.
   "
   :config
   (defvar lisp/init-file "~/.notes/x.code.lsp/config/bootstrap.lisp")
   (add-hook-lambda 'lisp-mode-hook (electric-pair-local-mode 1))

   (cond
    ((executable-find "ros")    ;; When roswell installed: ros install sly
     (load (expand-file-name "~/.roswell/helper.el")))
    (t (x sly :ensure t :config ;; When no roswell support
          (setq inferior-lisp-program (seq-find #'executable-find '("sbcl" "ccl")))
          (add-hook 'sly-connected-hook '%lisp/init-quicklisp))))

   (add-hook 'sly-connected-hook '%lisp/load-init-file)
   (add-hook 'sly-transcript-stop-hook '%lisp/after-init)
   (add-hook 'sly-mrepl-mode-hook 'company-mode)

   (defun %lisp/load-init-file ()
     "Load lisp init file."
     (when (file-exists-p lisp/init-file)
       (with-temp-buffer
         (insert-file-contents lisp/init-file)
         (sly-eval-buffer))))

   (defun %lisp/after-init ()
     "Things to do after init."
     (switch-to-buffer (seq-find (lambda (b) (string-match-p "sly-mrepl for.*" (buffer-name b))) (buffer-list)))
     (sly-mrepl--eval-for-repl `(slynk-mrepl:guess-and-set-package 'IMFINE))
     (remove-hook 'sly-transcript-stop-hook '%lisp/after-init))

   (defun %lisp/init-quicklisp ()
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

;;; Haskell

(x haskell-mode
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
     (if (executable-find "cabal") (dante-mode))))

(x dante
   :commands dante-mode
   :bind (:map hs-minor-mode-map ("C-c '" . dante-eval-block))
   :init (add-hook-lambda 'dante-mode-hook
           (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))
   :config (im/patch))

;;; Erlang
;; Distel is said a good IDE. try some day.
;; other setups later too.

;;; Python-Elpy

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
      (add-hook 'inferior-python-mode-hook 'im/cp936-encoding))))

;;; Ruby-Mode

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

;;; Golang

(x go-mode
   :hook ((go-mode . smartparens-mode))
   :config
   (x company-go
      :bind (:map go-mode-map ("M-." . godef-jump))))

;;; PHP

(x php-mode :config
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

;;; Scala

(x scala-mode
   "M-x `ensime' to connect for this mode.
   "
   "To use ensime with sbt:
   "
   " echo 'addSbtPlugin(\"org.ensime\" % \"ensime-sbt\" % \"2.5.1\")' > ~/.sbt/1.0/plugins/plugins.sbt"
   " sbt new xxx/tpl"
   " sbt ensimeConfig
   "
   :init
   (setq scala-mode-hook '-my/scala-mode-hook)

   :config
   (x ensime :config
      (setq ensime-startup-notification nil)))

;;;; Programer - Front-End
;;
;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;  - 20181120, Servers as Elnode is more powerful but too old. Simpled-Httpd is simple and enough.
;;  - 20181120, Use Livereload replace Impatient! Websocket has a better experience than iframe.

(x simple-httpd
   "Start local server with port 5555:
   "
   " - M-x im/http-here
   "
   "Define your own servlet:
   "
   "  (defservlet time text/html () (insert (format \"%s\" (time))))
   "
   "Then you can visit with 'http://host:5555/time'
   "
   :init
   (defun im/httpd-here ()
     (interactive)
     (httpd-start :port 5555 :root default-directory))

   :config (im/patch)
   (defservlet time text/html ()
     (insert (format "<h1>%s</h1>" (time)))))

(x livereload
   :commands (liveview liveload))

(x web-mode
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|tpl\\|css\\|vue\\)\\'"

   :config
   (setq web-mode-markup-indent-offset    2
         web-mode-css-indent-offset       2
         web-mode-code-indent-offset      2
         web-mode-enable-css-colorization t
         web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'")))

   (defun -my/web-mode-hook ()
     (hs-minor-mode -1)
     ;; (flycheck-mode +1)
     (append-local 'company-backends
                   'company-tide 'company-css 'company-web-html)
     (pcase (file-name-extension (buffer-file-name))
       ("js"  (im/tide-enable))
       ("jsx"
        (im/tide-enable)
        (rjsx-minor-mode 1) (set (make-local-variable 'web-mode-enable-auto-quoting) nil)
        (electric-pair-local-mode 1))))

   (add-hook 'web-mode-hook '-my/web-mode-hook)

   (defun -my/yas-expand-extra (&rest args)
     "Yasnippet in in SCRIPT block."
     (when (equal major-mode 'web-mode)
       (-my/yas--clear-extra-mode)
       (yas-activate-extra-mode
        (pcase (web-mode-language-at-pos)
          ("html"           'html-mode)
          ("css"            'css-mode)
          ("javascript"     'js2-mode)
          ("jsx"            'js2-mode)))))
   (advice-add 'yas--maybe-expand-key-filter :before '-my/yas-expand-extra))

(x emmet-mode/d
   :hook (web-mode rjsx-mode)
   :init (setq emmet-move-cursor-between-quotes t))

(x js2-mode
   :mode "\\.js\\'"
   :config
   (setq js2-basic-offset 2
         js2-highlight-level 3
         js2-strict-missing-semi-warning nil
         js2-strict-inconsistent-return-warning nil)

   (defun -my/js2-hook ()
     (setq mode-name "JS2")
     (electric-pair-local-mode 1)
     (flycheck-mode 1)
     (im/tide-enable))

   (add-hook 'js2-mode-hook '-my/js2-hook))

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

   (defun im/tide-enable (&optional mode)
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

   (defun -my/company-with-tide (f &rest args)
     "Complete with tide in SCRIPT block."
     (let ((tide-mode (or (derived-mode-p 'js2-mode)
                          (and (equal major-mode 'web-mode)
                               (or (string= (web-mode-language-at-pos) "javascript")
                                   (string= (web-mode-language-at-pos) "jsx"))))))
       (apply f args)))
   (advice-add 'company-tide :around '-my/company-with-tide))


;;;; Programer - LSP

(x lsp-mode)

;; (x dap-mode)

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


(provide 'immor)

;;; immor.el ends here

;;; Tooltips

;; remove `Origami', I don't like it. 20180111
;; remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; remove `iedit', as I found `auto-highlight-symbol' is so powerful, and the code is amazing! I will hack it for fun. 2018-07-03
