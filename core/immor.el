;;; immor.el --- Modules Configuration
;;; Commentary:

;;; Code:


;;; Auto-mode-alist

(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.\\(xml\\|xsl\\)\\'"      . sgml-mode)
        ("\\.class\\'"                . class-mode)
        ("\\.scm\\'"                  . scheme-mode)
        ("\\.\\(ba\\)?sh\\'"          . sh-mode)))


;;; Loads

(autoload 'grep-apply-setting "grep")
(autoload 'comint-mode "ls-lisp")


;;; Hooks

(defun im/task-after-open-file ()
  (if (and (string-match-p "^/usr/\\|.emacs.d/packages\\|/emacs-lisp/" buffer-file-name)
           (not (string-match-p "autoloads.el$" buffer-file-name)))
      (view-mode 1)))

(defun im/task-when-idle ()
  (let ((then (current-time)))
    (message "∵ [%s] Idle loading..." (time then 2))
    (mapc 'require im/need-idle-loads)
    (princ im/need-idle-loads)
    (message "∴ [%s] Idle loaded, %.2fs elapsed."
             (time nil 2) (time-subtract-seconds (current-time) then)))
  (remove-hook 'auto-save-hook 'im/task-when-idle))

(defun im/before-save ()
  (unless (seq-contains '(org-mode) major-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'im/before-save)
(add-hook 'after-save-hook 'im/el-autocompile)
(add-hook 'auto-save-hook 'im/task-when-idle)
(add-hook 'find-file-hook 'im/task-after-open-file)


;;; Basic

(x recentf/e :init (recentf-mode))
(x paren/e :init (show-paren-mode))
(x beacon/ve :init (beacon-mode))
(x autorevert/e :init (setq auto-revert-mode-text "") (global-auto-revert-mode))


;;; Ediff

(x ediff :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))


;;; Multiple-Cursor
(x multiple-cursors
   :bind (([S-f6]          . mc/mark-all-dwim)
          ("C-S-<mouse-1>" . mc/add-cursor-on-click)))


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
     (ibuffer-switch-to-saved-filter-groups "default")))


;;; Dired/Neotree/Ivy/Projectile

(x wdired/e
   :bind
   (:map dired-mode-map
         ( "6"  . dired-up-directory )
         ( "r"  . wdired-change-to-wdired-mode )
         ( "z"  . dired-get-size ))

   :config
   ;; directory first
   (setq dired-listing-switches "-aBhl  --group-directories-first")
   ;; Get the size of file or dir(du), 'z' for short.
   (defun dired-get-size ()
     (interactive)
     (let ((files (dired-get-marked-files)))
       (with-temp-buffer
         (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
         (message "Size of all marked files: %s"
                  (progn (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*\\(量\\|total\\)$")
                         (match-string 1))))))
   ;; Keep only one dired buffer
   (advice-add 'dired-find-file :around
               (lambda (f &rest args)
                 (let ((orig (current-buffer))
                       (filename (dired-get-file-for-visit)))
                   (apply f args)
                   (if (and (file-directory-p filename)
                            (not (eq (current-buffer) orig)))
                       (kill-buffer orig)))))
   (advice-add 'dired-up-directory :around
               (lambda (f &rest args)
                 (let ((orig (current-buffer)))
                   (apply f args) (kill-buffer orig)))))

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
   (setq ivy-use-virtual-buffers  t)
   (ivy-mode 1))

(x counsel-projectile/w
   :bind (( "M-x"      . counsel-M-x          )
          ( "C-x b"  . ivy-switch-buffer    )
          ( "C-x C-b"  . ibuffer              )
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
   (setq projectile-cache-file          (concat _CACHE_ "__projectile.cache")
         projectile-known-projects-file (concat _CACHE_ "__projectile-bookmark.eld")
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


;;; Tramp/Eshell

(x tramp/w :init
   (setq tramp-default-user "root"
         tramp-default-method "sshx")

   (defun im/ssh ()
     "Shortcut for my remote site."
     (interactive)
     (im/with-mode-silent ivy-mode
       (find-file (completing-read "remote: " '("/sshx:root@45.63.55.2:~/"))))))

(x eshell/w
   :config
   (setq eshell-input-filter
         (lambda (input) ; Filter dup hist
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


;;; Exec-Path

(x exec-path-from-shell/x
   :if (env-linux)
   :init (setq exec-path-from-shell-check-startup-files nil)
   :config (exec-path-from-shell-initialize))


;;; View

(x view :bind
   (:map view-mode-map
         ( "h"   .  backward-char )
         ( "l"   .  forward-char  )
         ( "j"   .  next-line     )
         ( "k"   .  previous-line )))


;;; Magit

(x magit/w :bind ("C-c m" . magit-status))


;;; SQLPlus

(x sqlplus
   :mode "\\.sqp\\'"
   :config
   (setq sqlplus-pagesize 2000)
   (add-hook-lambda 'sqlplus-mode-hook
     (env-windows
      (dolist (f '(sqlplus-table-head-face sqlplus-table-even-rows-face sqlplus-table-odd-rows-face))
        (set-face-attribute f nil :inherit 'org-table)))
     (let ((encoding 'gbk))
       (set-process-coding-system (get-process (sqlplus-get-process-name sqlplus-connect-string)) encoding encoding))))


;;; Flycheck/Flyspell

(x flycheck/w)

(x flyspell/x
   :preface (setq ispell-program-name "ispell") ;; apt install ispell
   :if (executable-find ispell-program-name)
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (rcirc-mode . flyspell-mode))
   :config (ispell-change-dictionary "american" t))


;;; Programer

(x prog-mode
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list))
   :init (add-hook-lambda 'prog-mode-hook
           (abbrev-mode 1)
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


;;; CC-Mode

(x cc-mode/w
   :config
   (setq-default c-default-style   "linux"
                 c-basic-offset     4
                 gdb-many-windows   t
                 gdb-show-main      t )

   (x xcscope
      :hook (cscope-list-entry-hook)
      :config
      (setq cscope-name-line-width            -15
            cscope-close-window-after-select  nil
            cscope-edit-single-match          nil)
      (add-to-list 'cscope-indexer-ignored-directories "__*"))

   (x semantic/w :config
      (global-semanticdb-minor-mode 1)
      (global-semantic-idle-scheduler-mode 1)
      (global-semantic-stickyfunc-mode 1))

   ;; for C++, use Rtags or Irony-Mode
   ;; CEDET + ECB is another choice
   (defun ccc-common-hook ()
     (semantic-mode)
     (cscope-minor-mode)
     (c-turn-on-eldoc-mode)

     (unless (file-exists-p "Makefile")
       (set (make-local-variable 'compile-command)
            (format "cc %s -g %s -o %s"
                    (buffer-name)
                    (or (getenv "CFLAGS") "-std=c99 -Wall")
                    (file-name-sans-extension (buffer-name))))))

   (add-hook 'c-mode-hook 'ccc-common-hook)
   (add-hook 'c++-mode-hook 'ccc-common-hook))


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
   :hook ((prog-mode   . company-mode)
          (progn-mode  . company-statistics-mode))
   :init (setq company-idle-delay 0.2))

(x yasnippet/e
   :diminish (yas-minor-mode . " Y")
   :bind (:map yas-keymap ("C-m" . 'yas-next-field-or-maybe-expand))
   :init (setq yas-verbosity 2 yas-alias-to-yas/prefix-p nil)
   :config (yas-global-mode 1))


;;; Web-Mode/JS-Mode/CSS-Mode
;; 20180111, Use Tide-Mode to Autocomplete instead of TERN.

(x web-mode
   :mode "\\.\\([xp]?html?\\(.erb\\|.blade\\)?\\|[aj]sp\\|jsx\\|tpl\\|css\\)\\'"
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

(x emmet-mode/v
   :hook web-mode
   :init (setq emmet-move-cursor-between-quotes t))


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
;;  for advanced usage, please try intero, IDE-like, it's based on Stack.

(x haskell-mode
   :mode "\\.hs\\'"
   :hook (haskell-mode . interactive-haskell-mode)
   :config
   (setq haskell-tags-on-save t
         haskell-process-show-debug-tips nil
         haskell-process-suggest-remove-import-lines t)
   (add-company-backend '(company-ghc :with company-dabbrev-code)))


;;; Erlang
;; Distel is said a good IDE. try some day.
;; other setups later too.


;;; Python-Elpy
;;  Install these modules for more features
;;  - pip install jedi importmagic
;;  - pip install flake8 autopep8

(x python :config (elpy-enable))


;;; Ruby-Mode
;; Rinari is a collection to develop ror
;; Inf-ruby provide a REPL
;; Robe-mode provide company backend.
;; - gem install pry.

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
   (add-hook 'php-mode-hook
             '(lambda ()
                (require 'company-php)
                (company-mode t)
                (ac-php-core-eldoc-setup) ;; enable eldoc
                (make-local-variable 'company-backends)
                (add-to-list 'company-backends 'company-ac-php-backend))))


(provide 'immor)

;;; immor.el ends here
