;;; immor.el --- Modules Configuration
;;; Commentary:

;; Download Mirror:
;; http://mirrors.ustc.edu.cn/gnu/emacs/windows/

;;; Code:


;;; Auto-mode-alist

(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.\\(xml\\|xsl\\)\\'"      . sgml-mode)
        ("\\.class\\'"                . class-mode)
        ("\\.scm\\'"                  . scheme-mode)
        ("\\.\\(ba\\)?sh\\'"          . sh-mode)))


;;; Display-Buffer-Alist

(setq display-buffer-alist
      `(("\\*shell\\*" display-buffer-same-window)
        ("\\*eshell\\*" display-buffer-same-window)))


;;; Loads

(autoload 'grep-apply-setting "grep")


;;; Hooks

((lambda ()
   (add-hook 'find-file-hook    'my/before-open)
   (add-hook 'before-save-hook  'my/before-save)
   (add-hook 'after-save-hook   'my/el-compile)
   (add-hook 'auto-save-hook    'my/idle-once)
   (add-hook 'auto-save-hook    'my/idle-tasks)

   (defun my/before-open ()
     ;; large file
     (when (> (buffer-size) (* 2 1024 1024))
       (setq buffer-read-only t)
       (buffer-disable-undo)
       (fundamental-mode))
     ;; system file
     (and (not (string-match-p "\\(autoloads\\|loaddefs\\).el$" buffer-file-name))
          (let ((case-fold-search nil)) (string-match-p "^/usr/\\|.emacs.d/packages\\|/emacs/" buffer-file-name))
          (view-mode 1)))

   (defun my/before-save ()
     (unless (seq-contains '(org-mode) major-mode)
       (delete-trailing-whitespace)))

   (defun my/el-compile (&optional dir)
     (save-window-excursion
       (when (and (eq major-mode 'emacs-lisp-mode)
                  (string-match-p "^[a-z]" (buffer-name))
                  (file-exists-p (concat (or dir "~/.emacs.d/core/") (buffer-name))))
         (byte-compile-file (buffer-file-name)))))

   (defun my/idle-once ()
      (let ((then (current-time)))
       (message "∵ [%s] Idle loading..." (time then 2))
       (mapc 'require im/need-idle-loads)
       (princ im/need-idle-loads)
       (message "∴ [%s] Idle loaded, %.2fs elapsed."
                (time nil 2) (time-subtract-seconds (current-time) then)))
     (remove-hook 'auto-save-hook 'my/idle-tasks))

   (defun my/idle-tasks ()
     (recentf-save-list))))



;;; Basic-Modes

(x paren/e :init (show-paren-mode))
(x beacon/ve :init (beacon-mode))
(x autorevert/e :init (setq auto-revert-mode-text "") (global-auto-revert-mode))
(x page-break-lines/e
   :init
   (setq page-break-lines-lighter "")
   (nconc page-break-lines-modes '(web-mode css-mode))
   (global-page-break-lines-mode))


;;; Ediff

(x ediff :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))


;;; Multiple-Cursor

(x multiple-cursors
   :bind (([S-f6]          . mc/mark-all-dwim)
          ("C-S-<mouse-1>" . mc/add-cursor-on-click)))


;;; Auto-Highlight-Symbol

(x auto-highlight-symbol
   :bind (:map auto-highlight-symbol-mode-map
               ("M-p" . ahs-backward)
               ("M-n" . ahs-forward))
   :init
   (setq ahs-idle-interval 0.3
         ahs-case-fold-search nil
         ahs-exclude '(( ruby-mode . "\\_<\\(end\\)\\_>"))))


;;; Isearch/Anzu/Occur

(x anzu/v :init (global-anzu-mode))

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


;;; Recentf/Dired/Neotree/Ivy/Projectile

(x recentf)

(x wdired/e
   :bind
   (:map dired-mode-map
         ( "6"  . dired-up-directory )
         ( "r"  . wdired-change-to-wdired-mode )
         ( "z"  . dired-du-mode ))
   :config
   (setq dired-dwim-target t)
   (setq dired-du-size-format 'comma)
   ;; sort style
   (if (env-linux)
       (setq dired-listing-switches "-alh --group-directories-first")
     (require 'ls-lisp)
     (setq ls-lisp-dirs-first t)
     (setq ls-lisp-UCA-like-collation nil)))

(x neotree/x :init
   (setq neo-theme 'arrow
         neotree-smart-optn t
         neo-window-fixed-size nil))

(x ivy/v
   :bind
   (:map ivy-minibuffer-map
         ("C-j" . ivy-done)
         ("C-m" . ivy-alt-done))

   :config
   (setq ivy-use-virtual-buffers t)
   (setq smex-save-file (concat _CACHE_ "smex-items")) ;; frequent used cmds
   (ivy-mode 1)
   (recentf-mode 1))

(x counsel-projectile/w
   :bind (( "M-x"      . counsel-M-x          )
          ( "C-x b"    . ivy-switch-buffer    )
          ( "C-x C-b"  . ibuffer              )
          ( "C-x d"    . counsel-projectile-find-dir )
          ( "C-x C-d"  . dired                )
          ( "C-x f"    . counsel-projectile-find-file )
          ( "C-x C-f"  . counsel-find-file    )
          ( "C-x i"    . counsel-imenu        )
          ( "C-x p"    . ivy-pages            )
          ( "C-h b"    . counsel-descbinds    )
          ( "C-h v"    . counsel-describe-variable )
          ( "C-h f"    . counsel-describe-function )
          ( "C-h S"    . counsel-info-lookup-symbol))

   :init
   (setq projectile-cache-file          (concat _CACHE_ "__projectile.cache")
         projectile-known-projects-file (concat _CACHE_ "__projectile-bookmark.eld")
         counsel-find-file-ignore-regexp "Volume\\|RECYCLE.BIN"
         projectile-completion-system   'ivy
         projectile-mode-line '(:eval (if (string= "-" (projectile-project-name)) "" (format " [%s]" (projectile-project-name)))))

   :config
   (projectile-mode 1)
   (if (fboundp 'counsel-projectile-on)
       (counsel-projectile-on)
     (counsel-projectile-mode)))


;;; HS-Minor-Mode/Outline-Minor-Mode
;; 20180111, Origami is removed

(x hideshow
   :diminish hs-minor-mode
   :hook ((web-mode prog-mode) . hs-minor-mode)
   :bind* (:map hs-minor-mode-map
                ([(M-down-mouse-1)] . nil)
                ([(M-mouse-1)] . hs-mouse-toggle-hiding)
                ("C-c C-f" . hs-toggle-hiding)
                ("C-c f"   . hs-toggle-hiding)
                ("C-c F"   . (lambda () (interactive)
                               (defvar im/hs-state nil)
                               (make-local-variable 'im/hs-state)
                               (setq im/hs-state (not im/hs-state))
                               (if im/hs-state (hs-hide-all) (hs-show-all)))))
   :config (add-to-list
            'hs-special-modes-alist
            '(ruby-mode
              "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
              (lambda (arg) (ruby-end-of-block)) nil)))


;;; Shell

(x eshell/w
   :init
   (setq comint-scroll-show-maximum-output nil
         eshell-scroll-show-maximum-output nil
         eshell-history-size    500
         eshell-hist-ignoredups t
         eshell-destroy-buffer-when-process-dies t
         eshell-aliases-file "~/.emacs.d/resource/eshell.alias"
         eshell-visual-subcommands
         '(("git" "log" "diff" "show")
           ("sudo" "vi" "visudo")))

   :config

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
        (t (apply 'eshell-exec-visual (cons "ssh" args))))))

   ;;; Hooks
   (add-hook-lambda 'eshell-mode-hook
     (define-key eshell-mode-map (kbd "C-r") 'eshell/h))
   (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))


;;; Tramp

(x tramp/w :init
   (setq tramp-default-user "root"
         tramp-default-method "sshx")

   ;; optimize
   (setq remote-file-name-inhibit-cache nil)
   (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
   (setq tramp-verbose 1)

   ;; shortcuts
   (defun ssh-edit ()
     "Shortcut for remote site."
     (interactive)
     (let* ((hosts
             '(("ygmall" . "/sshx:Administrator@120.24.78.141:/c/soft/phpstudy/WWW/ygmall")
               ("vps.45" . "/sshx:root@45.63.55.2:~/")))
            (item (completing-read "Connect to: " (mapcar 'car hosts)))
            (host (or (cdr (assoc-string item hosts)) item)))
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
         ( "<DEL>" .  nil)))


;;; Magit

(when (executable-find "git")
  (x magit/w
     :bind ("C-c m" . magit-status)
     :init (magit-auto-revert-mode -1)))


;;; SQL

(x sql
   :init
   (defalias 'mysql 'sql-mysql)
   :config
   (setq sql-user "root")
   (setq sql-product 'mysql)
   (setq sql-connection-alist
         '((45.63.55.2
            (sql-product 'mysql)
            (sql-server "45.63.55.2")
            (sql-port 3306)
            (sql-database "ego")
            (sql-user "root"))))
   (sql-set-product-feature 'mysql
                            :prompt-regexp "^\\(MariaDB\\|MySQL\\) *\\[[^ ]*\\]> *")
   (env-windows
    (setq sql-mysql-options '("-C" "-t" "-f" "-n"))
    (add-hook 'sql-interactive-mode-hook 'im/cp936-encoding)))


;;; Engine Mode

(x engine-mode
   :init
   (setq engine/keybinding-prefix "C-h h")
   (engine-mode 1)

   :config
   (defengine arch-wiki
     "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go"
     :keybinding "l"
     :browser 'eww-browse-url)

   (defengine google
     "https://google.com/search?q=%s"
     :keybinding "h")

   (defengine github
     "https://github.com/search?ref=simplesearch&q=%s"
     :keybinding "g")

   (defengine stackoverflow
     "http://stackoverflow.com/search?q=%s"
     :keybinding "s")

   (defengine wikipedia
     "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
     :keybinding "w")

   (defengine wolfram-alpha
     "http://www.wolframalpha.com/input/?i=%s")

   (defengine youtube
     "http://www.youtube.com/results?aq=f&oq=&search_query=%s"))



;;; Programer - Common

(x prog-mode
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list))
   :init (add-hook-lambda 'prog-mode-hook
           (abbrev-mode 1)
           (auto-highlight-symbol-mode 1)
           (rainbow-delimiters-mode 1)
           (which-function-mode 1)))


;;; Which-Func

(x which-func/x
   :config
   (setq which-func-format
         `("["
           (:propertize (:eval (my-which-func-current))
                        local-map ,which-func-keymap
                        face which-func
                        mouse-face mode-line-highlight
                        help-echo "Singing...")
           "]"))
   (defun my-which-func-current ()
     (let ((current (gethash (selected-window) which-func-table)))
       (if current
           (truncate-string-to-width current 20 nil nil "…")
         which-func-unknown))))


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

(x abbrev/v :config
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file)))

(x company/vw
   :init (defun add-company-backend (backend)
           (add-to-list 'company-backends backend))
   :hook ((prog-mode   . company-mode))
   :init (setq company-idle-delay 0.2))

(x yasnippet/e
   :diminish (yas-minor-mode . " Y")
   :bind (:map yas-keymap ("C-m" . 'yas-next-field-or-maybe-expand))
   :init (setq yas-verbosity 2 yas-alias-to-yas/prefix-p nil)
   :config
   (add-hook 'yas-minor-mode-hook
             (lambda ()
               (yas-activate-extra-mode 'fundamental-mode)))
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
                       (set-window-dedicated-p it wstat))))))
   :init (add-hook 'compilation-mode-hook
                   (lambda () (aif (get-buffer "*compilation*") (switch-to-buffer it)))))



;;; Programer - Languages

;;; lsp-mode
;;
(x lsp-mode)

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


;;; CC-Mode
;;
;; If use lsp, need cquery support
;;
;;    # pacman -S cquery-git
;;
;; CEDET + ECB is a choice, and Irony for C++ for another choice.
;;
(x cc-mode/w :config
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
   (require 'projectile)
   (add-to-list 'projectile-globally-ignored-directories cquery-cache-dir))

(x semantic
   :after (cc-mode)
   :config
   (setq semanticdb-default-save-directory (concat _CACHE_ "semanticdb"))
   (global-semanticdb-minor-mode 1)
   (global-semantic-idle-scheduler-mode 1)
   (global-semantic-stickyfunc-mode 1))


;;; Web-Mode/JS-Mode/CSS-Mode
;;
;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;
(x web-mode
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|jsx\\|tpl\\|css\\)\\'"
   :config
   (setq web-mode-markup-indent-offset    4
         web-mode-css-indent-offset       4
         web-mode-code-indent-offset      4
         web-mode-enable-css-colorization t
         web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'")))

   (add-hook-lambda 'web-mode-hook
     ;; (flycheck-mode +1)
     (hs-minor-mode -1)
     (set (make-local-variable 'company-backends)
          '(company-css company-tide company-dabbrev-code company-keywords company-files)))

   (add-hook-lambda 'yas-before-expand-snippet-hook
     (if (eq major-mode 'web-mode)
         (set (make-local-variable 'yas--extra-modes)
              (pcase (web-mode-language-at-pos)
                ("html"           '(html-mode))
                ("css"            '(css-mode))
                ("javascript"     '(js2-mode))))))

   ;; Indent
   (add-to-list 'web-mode-indentless-elements "html")
   (advice-add 'web-mode-markup-indentation :around
               (lambda (name &rest args)
                 (if (member (get-text-property (car args) 'tag-name) '("head"))
                     0 (apply name args)))))

(x emmet-mode/v
   :hook web-mode
   :init (setq emmet-move-cursor-between-quotes t))

(x impatient-mode
   :commands imp-visit-buffer
   :init
   (defalias 'liveload 'impatient-mode)
   (defalias 'liveview 'imp-visit-buffer)
   (advice-add 'impatient-mode :before
               (lambda (&rest args)
                 (unless (process-status "httpd")
                   (setq httpd-port 5555)
                   (httpd-start)))))

(x js2-mode
   :mode "\\.js\\'"
   :config
   (setq js2-highlight-level 3
         js2-strict-missing-semi-warning nil
         js2-strict-inconsistent-return-warning nil)

   (add-hook-lambda 'js2-mode-hook
     (setq mode-name "ES")
     (flycheck-mode +1)
     (js2-imenu-extras-mode +1))

   (x web-beautify :if (executable-find "js-beautify")))

(when (executable-find "node")
  (x tide/w
     :diminish " τ"
     :hook ((js2-mode) . tide)
     :init
     (setq tide-default-mode "JS")
     (defun tide ()
       (interactive)
       (require 'tide)
       (tide-setup)
       (eldoc-mode +1)
       (tide-hl-identifier-mode +1)
       (set (make-local-variable 'company-tooltip-align-annotations) t))))


;;; SLIME (The Superior Lisp Interaction Mode for Emacs)

(x slime
   :config
   (setq inferior-lisp-program (seq-find #'executable-find '("sbcl" "ccl" "clisp")))
   (add-to-list 'slime-contribs 'slime-fancy)

   ;; hooks
   (add-hook-lambda 'slime-connected-hook
     ;; [quicklisp] load or initial
     (let* ((ql-home (file-name-as-directory "~/.quicklisp"))
            (ql-url "http://beta.quicklisp.org/quicklisp.lisp")
            (ql-dist (concat ql-home "quicklisp.lisp"))
            (slime-init "~/.cases/lang-lisp/misc/slime-init.lisp"))
       (if (file-exists-p ql-dist)  ;; load slime-init.lisp
           (when (file-exists-p slime-init)
             (with-temp-buffer
               (insert-file-contents slime-init)
               (slime-eval-buffer))
             (sit-for 2)
             (ignore-errors (slime-repl-set-package "IMFINE")))
         (make-directory ql-home)   ;; begin to initial
         (url-copy-file ql-url ql-dist)
         (with-temp-buffer
           (insert-file-contents ql-dist)
           (goto-char (point-max))
           (insert (format "\n(quicklisp-quickstart:install :path \"%s\")" ql-home))
           (insert "(let ((*do-not-prompt* t)) (ql:add-to-init-file))")
           (slime-eval-buffer))))
     ;; auto switch to repl buffer
     (switch-to-buffer (format "*slime-repl %s*" inferior-lisp-program)))

   (add-hook-lambda 'slime-mode-hook
     (global-set-key "\C-cs" 'slime-selector))

   (add-hook-lambda 'slime-repl-mode-hook
     (define-key slime-repl-mode-map (kbd "TAB") 'hippie-expand))

   (add-hook-lambda 'slime-autodoc-mode-hook
     (setq-local hippie-expand-try-functions-list
                 (append (butlast hippie-expand-try-functions-list 2)
                         (list 'try-expand-slime)))))


;;; Haskell
;;
;;  Basic usage, with interactive-mode
;;
;;  Advanced usage, with Intero, IDE-like, based on Stack:
;;
;;    pacman -S stack
;;
;;  Intero will take a long time to initialize for the first time, download a lot!
;;
;;  Get out, intero! Toooo much disk space used! Change to Dante 20180513, saved 3G space...
;;
(x haskell-mode
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
   :commands 'dante-mode
   :bind (:map hs-minor-mode-map ("C-c '" . dante-eval-block))
   :init
   (add-hook-lambda 'dante-mode-hook
     (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

   :config
   ;; mess fix
   (defun haskell-process-type ()
     (let ((cabal-sandbox (locate-dominating-file default-directory "cabal.sandbox.config"))
           (stack         (locate-dominating-file default-directory "stack.yaml"))
           (cabal-new     (locate-dominating-file default-directory "cabal.project"))
           (cabal         (locate-dominating-file default-directory (lambda (d) (cl-find-if (lambda (f) (string-match-p ".\\.cabal\\'" f)) (directory-files d))))))
       (if (eq 'auto haskell-process-type)
           (cond
            ((and cabal-sandbox (executable-find "cabal")) (setq inferior-haskell-root-dir cabal-sandbox) 'cabal-repl)
            ((and stack (executable-find "stack")) (setq inferior-haskell-root-dir stack) 'stack-ghci)
            ((and cabal-new (executable-find "cabal")) (setq inferior-haskell-root-dir cabal-new) 'cabal-new-repl)
            ((and cabal (executable-find "cabal")) (setq inferior-haskell-root-dir cabal) 'cabal-repl)
            ((executable-find "ghc") (setq inferior-haskell-root-dir default-directory) 'ghci)
            (t (error "Could not find any installation of GHC.")))
         haskell-process-type)))
   (defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
     (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
       (with-current-buffer (haskell-interactive-mode-splices-buffer session)
         (erase-buffer)))
     (let* ((ok (cond
                 ((haskell-process-consume process "Ok,\\(?:.+\\) modules? loaded\\.$") t)
                 ((haskell-process-consume process "Failed,\\(?:.+\\) modules? loaded\\.$") nil)
                 ((haskell-process-consume process "Ok, modules loaded: \\(.+\\)\\.$") t)
                 ((haskell-process-consume process "Failed, modules loaded: \\(.+\\)\\.$") nil)
                 (t (error (message "Unexpected response from haskell process.")))))
            (modules (haskell-process-extract-modules buffer))
            (cursor (haskell-process-response-cursor process))
            (warning-count 0))
       (haskell-process-set-response-cursor process 0)
       (haskell-check-remove-overlays module-buffer)
       (while
           (haskell-process-errors-warnings module-buffer session process buffer)
         (setq warning-count (1+ warning-count)))
       (haskell-process-set-response-cursor process cursor)
       (if (and (not reload) haskell-process-reload-with-fbytecode)
           (haskell-process-reload-with-fbytecode process module-buffer)
         (haskell-process-import-modules process (car modules)))
       (if ok (haskell-mode-message-line (if reload "Reloaded OK." "OK."))
         (haskell-interactive-mode-compile-error session "Compilation failed."))
       (when cont
         (condition-case-unless-debug e
             (funcall cont ok)
           (error (message "%S" e))
           (quit nil)))))
   (defun haskell-mode-find-def (ident)
     (when (stringp ident)
       (let ((reply (haskell-process-queue-sync-request
                     (haskell-interactive-process)
                     (format (if (string-match "^[a-zA-Z_]" ident) ":info %s" ":info (%s)") ident))))
         (let ((match (string-match "-- Defined \\(at\\|in\\) \\(.+\\)$" reply)))
           (when match
             (let ((defined (match-string 2 reply)))
               (let ((match (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)$" defined)))
                 (if match (list 'file
                                 (expand-file-name (match-string 1 defined) (haskell-session-current-dir (haskell-interactive-session)))
                                 (string-to-number (match-string 2 defined))
                                 (string-to-number (match-string 3 defined)))
                   (let ((match (string-match "‘\\(.+?\\):\\(.+?\\)’$" defined)))
                     (if match (list 'library
                                     (match-string 1 defined)
                                     (match-string 2 defined))
                       (let ((match (string-match "‘\\(.+?\\)’$" defined)))
                         (if match (list 'module (match-string 1 defined)))))))))))))))


;;; Erlang
;; Distel is said a good IDE. try some day.
;; other setups later too.


;;; Python-Elpy
;;
;;  Install these modules for more features
;;  - pip install jedi importmagic
;;  - pip install flake8 autopep8
;;
(x python
   :interpreter ("python" . python-mode)
   :config
   (when (executable-find "python")
     (elpy-enable)
     (env-windows
      (setq python-shell-completion-native-enable nil)
      (add-hook 'inferior-python-mode-hook 'im/cp936-encoding))))


;;; Ruby-Mode
;;
;;  Rinari is a collection to develop RoR
;;
;;  Inf-ruby provide a REPL
;;
;;  Robe-mode provide company backend.
;;    - gem install pry.
;;
(x ruby-mode
   :hook ((ruby-mode . inf-ruby-minor-mode)
          (ruby-mode . robe-mode))
   :config
   (add-company-backend 'company-robe)

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
      :bind (:map go-mode-map
                  ("M-." . godef-jump))))


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
          (make-local-variable 'company-backends)
          (add-to-list 'company-backends 'company-ac-php-backend))))

   (add-hook 'php-mode-hook 'my-php-stuff))


(provide 'immor)

;;; immor.el ends here
