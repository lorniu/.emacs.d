;;;==============================
;;;   Plugins and Development
;;;==============================


;;; Loads
(autoload 'grep-apply-setting "grep")
(autoload 'comint-mode "ls-lisp")
(autoload 'beacon-mode "beacon")

(beacon-mode)
(recentf-mode)
(show-paren-mode)
(global-auto-revert-mode)


;;; Hooks
(defun im/task-after-open-file ()
  (if (string-match-p "^/usr/\\|.emacs.d/packages\\|/emacs-lisp/" buffer-file-name)
      (read-only-mode 1)))

(defun im/task-when-idle ()
  (let ((then (current-time)))
    (message "∵ [%s] Idle loading..." (time then 2))
    (global-anzu-mode)
    (mapc #'require im/need-idle-loads)
    (princ im/need-idle-loads)
    (message "∴ [%s] Idle loaded, %.2fs elapsed."
             (time nil 2) (time-subtract-seconds (current-time) then)))
  (remove-hook 'auto-save-hook 'im/task-when-idle))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'im/el-autocompile)
(add-hook 'auto-save-hook 'im/task-when-idle)
(add-hook 'find-file-hook 'im/task-after-open-file)


;;; Auto-mode-alist
(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.\\(xml\\|xsl\\)\\'"      . sgml-mode)
        ("\\.class\\'"                . class-mode)
        ("\\.scm\\'"                  . scheme-mode)
        ("\\.sqp\\'"                  . sqlplus-mode)
        ("\\.\\(ba\\)?sh\\'"          . sh-mode)))



;;; Exec-Path
(x exec-path-from-shell/x
   :if (with-linux)
   :init (setq exec-path-from-shell-check-startup-files nil)
   :config (exec-path-from-shell-initialize))



;;; Ediff
(x ediff :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))



;;; Isearch/Anzu/Occur
(with-eval-after-load "isearch"  ;; superword
  (bind-key "\C-e"
            (lambda () (interactive)
              (superword-mode 1)
              (isearch-yank-word-or-char)
              (superword-mode -1))
            isearch-mode-map)
  (bind-key "M-s o"
            (lambda (str) ;; when occur, word under cursor first
              (interactive (list (aif (symbol-at-point) (symbol-name it))))
              (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))

  (defun after-occur-follow (&rest args)
    (aif (get-buffer-window "*Occur*") (select-window it)))
  (advice-add 'occur :after #'after-occur-follow))



;;; Dired/Neotree/Ivy/Projectile
(x wdired/e
   :bind
   (:map dired-mode-map
         ( "6"  . dired-up-directory )
         ( "r"  . wdired-change-to-wdired-mode )
         ( "z"  . dired-get-size ))
   :config
   ;; Keep only one dired buffer
   (defun dired-ff-advice(f &rest args)
     (let ((orig (current-buffer))
           (filename (dired-get-file-for-visit)))
       (apply f args)
       (if (and (file-directory-p filename)
                (not (eq (current-buffer) orig)))
           (kill-buffer orig))))
   (defun dired-ud-advice(f &rest args)
     (let ((orig (current-buffer)))
       (apply f args) (kill-buffer orig)))
   (advice-add 'dired-find-file :around #'dired-ff-advice)
   (advice-add 'dired-up-directory :around #'dired-ud-advice)
   ;; Z to get the size, used du
   (defun dired-get-size ()
     (interactive)
     (let ((files (dired-get-marked-files)))
       (with-temp-buffer
         (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
         (message "Size of all marked files: %s"
                  (progn (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*\\(量\\|total\\)$")
                         (match-string 1)))))))
(x neotree/x :init
   (setq neo-theme 'arrow
         neotree-smart-optn t
         neo-window-fixed-size nil))
(x ivy/ve
   :bind
   (:map ivy-minibuffer-map
         ("C-j" . ivy-done)
         ("C-m" . ivy-alt-done))
   :config
   (setq ivy-use-virtual-buffers  t)
   (ivy-mode 1))
(x counsel-projectile/w
   :bind (( "C-x C-b"  . ibuffer              )
          ( "M-x"      . counsel-M-x          )
          ( "C-x d"    . counsel-projectile-find-dir )
          ( "C-x C-d"  . dired                )
          ( "C-x f"    . counsel-projectile-find-file )
          ( "C-x C-f"  . counsel-find-file    )
          ( "C-x i"    . counsel-imenu        )
          ( "C-h b"    . counsel-descbinds    )
          ( "C-h v"    . counsel-describe-variable )
          ( "C-h f"    . counsel-describe-function )
          ( "C-h S"    . counsel-info-lookup-symbol))
   :init
   (setq projectile-cache-file          "~/.emacs.d/.cache/__projectile.cache"
         projectile-known-projects-file "~/.emacs.d/.cache/__projectile-bookmark.eld"
         projectile-mode-line '(:eval (if (string= "-" (projectile-project-name)) "" (format " [%s]" (projectile-project-name)))))
   :config
   (projectile-mode 1)
   (if (fboundp 'counsel-projectile-on)
       (counsel-projectile-on)
     (counsel-projectile-mode)))



;;; Ibuffer
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      `(("default"
         ("apple"
          (or (name . "\\*.+\\*")))
         ("pear"
          (or (name . "\\*magit")))
         ("mongo"
          (or (mode . org-mode)
              (mode . dired-mode)))
         ("orange"
          (or (mode . erc-mode)
              (mode . rcirc-mode)))
         ("banana"
          (or (predicate . (eq (get major-mode 'derived-mode-parent) 'prog-mode))
              (predicate . (member major-mode '(nxml-mode sgml-mode))))))))
(add-hook-lambda 'ibuffer-mode-hook
  (ibuffer-switch-to-saved-filter-groups "default"))



;;; HS-Minor-Mode/Outline-Minor-Mode/Origami
(x hideshow
   :diminish hs-minor-mode
   :bind (([S-f5] . im/hs-toggle-all)
          ([f5]   . hs-toggle-hiding))
   :init (defun im/hs-toggle-all ()
           (interactive)
           (defvar im/hs-state)
           (if (and (boundp 'im/hs-state) im/hs-state)
               (progn (setf im/hs-state nil) (hs-show-all))
             (setf im/hs-state t) (hs-hide-all))))
(x origami/w
   :bind (("M-o"   . origami-recursively-toggle-node )
          ("C-M-o" . origami-toggle-all-nodes ))
   :config (global-origami-mode))



;;; Tramp/Eshell
(x tramp/w :init
   (setq tramp-default-user "root"
         tramp-default-method "sshx")
   (defun im/ssh ()
     "a shortcut for my remote site"
     (interactive) (ivy-mode -1)
     (find-file (completing-read "remote: " '("/sshx:root@45.63.55.2:~/"))) (ivy-mode 1)))
(x eshell/w
   :config
   (setq eshell-input-filter
         (lambda (input)
           ;; filter dup hist
           (and (not (string-match-p "\\`\\s-*\\'" input))
                (> (length input) 2)
                (dotimes (i (ring-length eshell-history-ring) t)
                  (when (equal input (ring-ref eshell-history-ring i))
                    (ring-remove eshell-history-ring i))))))
   (defun eshell/ccc ()
     (interactive)
     (eshell/clear-scrollback)
     (eshell-send-input))
   (add-hook-lambda 'eshell-mode-hook
     (define-key eshell-command-map [(control ?l)] 'eshell/ccc)))



;;; Flyspell
;;  apt install aspell aspell-en
(x flyspell
   :config
   (setq ispell-program-name "aspell"
         ispell-extra-args '("--sug-mode=ultra")
         flyspell-mode-line-string nil)
   (when (executable-find ispell-program-name)
     (ispell-change-dictionary "american" t)
     (add-hook 'text-mode-hook 'flyspell-mode)
     (add-hook 'prog-mode-hook 'flyspell-prog-mode)))



(x view
   :bind (:map view-mode-map
               ( "h"   .  backward-char )
               ( "l"   .  forward-char  )
               ( "j"   .  next-line     )
               ( "k"   .  previous-line )
               ( "C-f" .  forward-sexp  )
               ( "C-b" .  backward-sexp )))



;;; Irc and Gnus
(advice-add 'irc  :before (lambda (&rest args) (require 'imnet)))
(advice-add 'gnus :before (lambda (&rest args) (require 'imnet)))



;;; Magit
(x magit/w :bind ("C-c m" . magit-status))



;;; SQLPlus Mode
(x sqlplus
   :config
   (setq sqlplus-pagesize 2000)
   (add-hook-lambda 'sqlplus-mode-hook
     (with-windows
      (dolist (f '(sqlplus-table-head-face sqlplus-table-even-rows-face sqlplus-table-odd-rows-face))
        (set-face-attribute f nil :inherit 'org-table)))
     (let ((encoding 'gbk))
       (set-process-coding-system (get-process (sqlplus-get-process-name sqlplus-connect-string)) encoding encoding))))



;;;==========================
;;;   program languages
;;;==========================

(x prog-mode
   :init
   (add-hook-lambda 'prog-mode-hook
     (abbrev-mode 1)
     (hs-minor-mode 1)
     (rainbow-delimiters-mode 1)
     (which-function-mode 1))
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list)))



;;; Abbrev/Hippie-Expand/Company-Mode/Yasnippet
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-file-name-partially try-complete-file-name
        try-expand-all-abbrevs try-expand-list try-expand-line
        try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(x abbrev/v
   :config (if (file-exists-p abbrev-file-name) (quietly-read-abbrev-file)))
(x company/vw
   :init
   (defun add-company-backend (backend)
     (add-to-list 'company-backends backend))
   :hook (prog-mode . company-mode) (progn-mode . company-statistics-mode)
   :init (setq company-idle-delay 0.1))
(x yasnippet/e
   :diminish (yas-minor-mode . " Y")
   :bind (:map yas-keymap ("C-m" . 'yas-next-field-or-maybe-expand))
   :init (setq yas-verbosity 2 yas-alias-to-yas/prefix-p nil)
   :config (yas-global-mode 1))



;;; Web-Mode/JS-Mode/CSS-Mode
(x emmet-mode/v
   :hook web-mode
   :init (setq emmet-move-cursor-between-quotes t))
(x web-mode
   :mode "\\.\\([xp]?html?\\(.erb\\)?\\|[aj]sp\\|css\\|jsx\\|tpl\\)\\'"
   :config
   ;; variable
   (setq web-mode-markup-indent-offset    4
         web-mode-enable-css-colorization t
         liveload-port                 5555
         web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
         web-mode-engines-alist '(("blade"  . "\\.blade\\.")
                                  ("ruby"   . "\\.html\\.erb\\'")))
   ;; function
   (defun ake-clean-indent(fun &rest args)
     (if (member (get-text-property (car args) 'tag-name) '("head"))
         0 (apply fun args)))
   (defun open-httpd-server (&rest args)
     (unless (process-status "httpd")
       (setq httpd-port liveload-port)
       (httpd-start)))
   (defun yas-judge-mode ()
     (when (eq major-mode 'web-mode)
       (set (make-local-variable 'yas--extra-modes)
            (pcase (web-mode-language-at-pos)
              ("html"       '(html-mode))
              ("css"        '(css-mode))
              ("javascript" '(js2-mode))))))
   (defun my-web-mode-hook ()
     (set (make-local-variable 'company-backends)
          '(company-web-html  company-files)))
   ;; company
   (add-hook 'web-mode-hook #'my-web-mode-hook)
   ;; snippet
   (add-hook 'yas-before-expand-snippet-hook #'yas-judge-mode)
   ;; head/body without indent
   (add-to-list 'web-mode-indentless-elements "html")
   (advice-add 'web-mode-markup-indentation :around #'ake-clean-indent)
   ;; liveload
   (x impatient-mode :commands imp-visit-buffer)
   (advice-add 'impatient-mode :before #'open-httpd-server)
   (autoload 'comint-mode "ls-lisp")
   (defalias 'liveload 'impatient-mode)
   (defalias 'liveview 'imp-visit-buffer))
;;; JS2-Mode/Tern/Web-Beautify/JS2-Refactor
;; Tern is a stand-alone code-analysis engine for JavaScript, for code completion.
;; Web-Beautify is a code beautify tool.
;; JS2-Refactor is used to code refactor.
;; - pacman -S npm
;; - npm install -g tern
;; - npm install -g js-beautify
(x js2-mode
   :mode "\\.js\\'"
   :hook (js2-mode . js2-imenu-extras-mode)
   :config
   (setq js2-highlight-level 3
         js2-strict-missing-semi-warning nil
         js2-strict-inconsistent-return-warning nil)

   (add-hook-lambda 'js2-mode-hook (setq mode-name "ES"))

   (x js2-refactor/v
      :config
      (add-hook-lambda 'js2-mode-hook
        (js2-refactor-mode 1)
        (js2r-add-keybindings-with-prefix "C-c j r")))

   (x tern/v
      :if (executable-find "tern")
      :hook (js2-mode . tern-mode)
      :config (add-company-backend 'company-tern))

   (x web-beautify
      :if (executable-find "js-beautify")))



;;; CC-Mode
(x cc-mode/w
   :config
   (setq c-default-style   "linux"
         gdb-many-windows   t
         gdb-show-main      t )

   (x xcscope
      :hook (cscope-list-entry-hook)
      :config
      (setq cscope-name-line-width            -15
            cscope-close-window-after-select  nil
            cscope-edit-single-match          nil)
      (add-to-list 'cscope-indexer-ignored-directories "__*"))

   (x semantic/e
      :config
      (global-semanticdb-minor-mode 1)
      (global-semantic-idle-scheduler-mode 1)
      (global-semantic-stickyfunc-mode 1))

   (defun ccc-common-hook ()
     (semantic-mode)
     (cscope-minor-mode     1)
     (c-turn-on-eldoc-mode)
     (unless (file-exists-p "Makefile")
       (set (make-local-variable 'compile-command)
            (format "cc %s -g %s -o %s" (buffer-name)
                    (or (getenv "CFLAGS") "-std=c99 -Wall")
                    (file-name-sans-extension (buffer-name))))))

   (add-hook 'c-mode-hook 'ccc-common-hook)
   (add-hook 'c++-mode-hook 'ccc-common-hook))



;;; Compile Buffer
(x compile
   :bind ([f9] . compile)
   :config
   (defkey compilation-mode-map  ;; jump to *eshell* buffer
     ("e" (ilambda
           (bury-buffer)
           (awhen (get-buffer-window "*eshell*")
             (let ((wstat (window-dedicated-p it)))
               (set-window-dedicated-p it nil)
               (aw-switch-to-window it)
               (set-window-dedicated-p it wstat))))))
   (add-hook-lambda 'compilation-mode-hook
     (aif (get-buffer "*compilation*") (switch-to-buffer it))))



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
            (slime-init "~/.showcase/lang-lisp/misc/slime-init.lisp"))
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
;;  some Cabal packages needed.
;;  for advanced usage, please try intero, IDE-like & base on Stack.
(x haskell-mode
   :mode "\\.hs\\'"
   :hook (haskell-mode . interactive-haskell-mode)
   :config
   (setq haskell-tags-on-save t
         haskell-process-show-debug-tips nil
         haskell-process-suggest-remove-import-lines t)
   (add-company-backend '(company-ghc :with company-dabbrev-code)))



;;; Erlang
;; distel is said a good IDE. try some day.
;; other setups later too.



;;; Python-Elpy
;;  install these modules for more features
;;    pip install jedi importmagic
;;    pip install flake8 autopep8
(x python :config (elpy-enable))



;;; Ruby-Mode
;;  rinari is a collection to develop ror
;;  inf-ruby provide a REPL
;;  robe-mode provide company backend.
;; need pry: gem install pry.
;; need invoke: robe-start
(x ruby-mode
   :hook ((ruby-mode . inf-ruby-minor-mode)
          (ruby-mode . robe-mode))
   :config
   (add-company-backend 'company-robe)

   (x inf-ruby
      :config
      (define-key inf-ruby-minor-mode-map (kbd "C-c C-s")
        (ilambda  ;; open rails console, fails then irb
         (or (ignore-errors (inf-ruby-console-auto))
             (ignore-errors (inf-ruby)))
         (robe-start)))))



;;; GOLANG
(x go-mode
   :hook ((go-mode . smartparens-mode))
   :config
   (x company-go
      :bind (:map go-mode-map
                  ("M-." . godef-jump))))



;;; PHP
(x php-mode
   :config
   (add-hook-lambda 'php-mode-hook
     (ac-php-core-eldoc-setup)
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends 'company-ac-php-backend)))






(provide 'immor)

;;; immor.el ends here
