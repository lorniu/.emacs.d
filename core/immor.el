;;;==============================
;;;   Plugins and Development
;;;==============================


;;; Autoloads
(autoload 'grep-apply-setting "grep")
(autoload 'comint-mode "ls-lisp")
(autoload 'beacon-mode "beacon")



;;; Auto-mode-alist
(mapc (lambda (s) (pushnew s auto-mode-alist))
      '(("\\.\\(xml\\|xsl\\)\\'"      . sgml-mode)
        ("\\.class\\'"                . class-mode)
        ("\\.scm\\'"                  . scheme-mode)
        ("\\.hs\\'"                   . haskell-mode)
        ("\\.sqp\\'"                  . sqlplus-mode)
        ("\\.\\(ba\\)?sh\\'"          . sh-mode)
        ("\\.org\\'"                  . org-mode)
        ("\\.\\(js\\|json\\)\\'"      . js2-mode)
        ("\\.\\([xp]?html?\\(.erb\\)?\\|[aj]sp\\|css\\)\\'" . web-mode)))



;;; Hippie-Expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))



;;; miscellaneous
(setq diff-switches "-u")
(add-hook 'after-save-hook 'im/el-autocompile)



;;; isearch
;;  C-e become superword forward in isearch
(with-eval-after-load "isearch"
  (define-key isearch-mode-map "\C-e"
    (ilambda
     (superword-mode 1)
     (isearch-yank-word-or-char)
     (superword-mode -1))))



;;; Ivy and Projectile
(with-eval-after-load 'ivy
  (counsel-projectile-on)
  (setq ivy-use-virtual-buffers  t)
  (setq projectile-cache-file          "~/.emacs.d/.cache/__projectile.cache"
        projectile-known-projects-file "~/.emacs.d/.cache/__projectile-bookmark.eld")
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))



;;; HS-Minor-Mode/Hideshowvis/Outline-Minor-Mode
(add-hook-lambda prog-mode-hook
  (hs-minor-mode 1)
  ;; (hideshowvis-minor-mode 1)
  (set (make-local-variable 'im/hs-state) nil))
(with-graphic (with-eval-after-load 'hideshowvis (hideshowvis-symbols)))



;;; Occur
(defun after-occur-follow (&rest args)
  (aif (get-buffer-window "*Occur*") (select-window it)))
(advice-add 'occur :after #'after-occur-follow)



;;; Tramp
(setq tramp-default-user "root"
      tramp-default-method "sshx")
(defun im/ssh ()
  "a shortcut for my remote site"
  (interactive) (ivy-mode -1)
  (find-file (completing-read "remote: " '("/sshx:root@45.63.55.2:~/"))) (ivy-mode 1))



;;; Flyspell
;;  apt install aspell aspell-en
(when (and nil (executable-find ispell-program-name))
  (require 'flyspell)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))
(with-eval-after-load 'flyspell
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-mode-line-string nil)
  (ispell-change-dictionary "american" t))



;;; Eshell
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
(add-hook-lambda eshell-mode-hook
  (define-key eshell-command-map [(control ?l)] 'eshell/ccc))



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
(add-hook-lambda ibuffer-mode-hook
  (ibuffer-switch-to-saved-filter-groups "default"))



;;; Dired
(require 'wdired)
(defkey dired-mode-map
  ( "6"   dired-up-directory )
  ( "r"   wdired-change-to-wdired-mode )
  ( "z"   dired-get-size ))
(defun dired-get-size ()
  "z to get the size, used du"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*\\(量\\|total\\)$")
                      (match-string 1))))))
;; keep only one dired buffer
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



;;; Irc and Gnus
(advice-add 'irc  :before (lambda (&rest args) (require 'imnet)))
(advice-add 'gnus :before (lambda (&rest args) (require 'imnet)))



;;; SQLPlus Mode
(add-hook-lambda sqlplus-mode-hook
  (setq sqlplus-pagesize 2000)
  (with-windows
   (dolist (f '(sqlplus-table-head-face sqlplus-table-even-rows-face sqlplus-table-odd-rows-face))
     (set-face-attribute f nil :inherit 'org-table)))
  (let ((encoding 'gbk))
    (set-process-coding-system (get-process (sqlplus-get-process-name sqlplus-connect-string)) encoding encoding)))



;;; Miscellaneous
(setq diff-switches "-u")
(add-hook 'after-save-hook 'im/el-autocompile)




;;;==========================
;;;   program languages
;;;==========================

;;; Common
(add-hook-lambda prog-mode-hook
  (abbrev-mode 1)
  (rainbow-delimiters-mode 1)
  (which-function-mode 1))
(defkey prog-mode-map ( "C-c C-u" backward-up-list ))



;;; Company-Mode
(defun add-company-backend(backend)
  (with-eval-after-load 'company
    (add-to-list 'company-backends backend)))
(setq-local company-minimum-prefix-length 1)
(add-hook-lambda prog-mode-hook (company-mode 1))



;;; Yas-Snippet
(add-hook-lambda yas-before-expand-snippet-hook
  (when (eq major-mode 'web-mode)
    (make-local-variable 'yas-extra-modes)
    (setq yas-extra-modes (pcase (web-mode-language-at-pos)
                            ("html"       '(html-mode))
                            ("css"        '(css-mode))
                            ("javascript" '(js2-mode))))))
(add-hook-lambda prog-mode-hook (yas-minor-mode 1))



;;; Web-Mode
(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-engines-alist '(("blade"  . "\\.blade\\.")
                                 ("ruby"   . "\\.html\\.erb\\'")))
  ;; head/body without indent
  (advice-add 'web-mode-markup-indentation :around
              (lambda (fun &rest args)
                (if (member (get-text-property (car args) 'tag-name) '("head"))
                    0 (apply fun args))))
  (add-to-list 'web-mode-indentless-elements "html")
  ;; hook
  (add-hook-lambda web-mode-hook (emmet-mode 1)))
;;; JS2-Mode/Tern/Web-Beautify/js2-Refactor
;; Tern is a stand-alone code-analysis engine for JavaScript.
;; Web-Beautify is a code beautify tool.
;; - npm install -g tern
;; - npm install -g js-beautify
(with-eval-after-load 'js2-mode
  (setq js2-highlight-level 3)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (when (executable-find "tern")
    (add-hook-lambda js2-mode-hook (tern-mode 1))
    (add-company-backend 'company-tern)))



;;; C-Mode
(defun ccc-common-hook ()
  ;; cscope
  (cscope-minor-mode     1)
  (setq cscope-name-line-width           -15
        cscope-close-window-after-select nil
        cscope-edit-single-match         nil )
  (add-to-list 'cscope-indexer-ignored-directories "__*")
  ;; eldoc
  (c-turn-on-eldoc-mode)
  ;; compile
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (format "cc %s -g %s -o %s"
                 (buffer-name)
                 (or (getenv "CFLAGS") "-std=c99 -Wall")
                 (file-name-sans-extension (buffer-name))))))
(add-hook-lambda find-file-hook
  (when (string-match-p "^/usr/.*" buffer-file-name)
    (read-only-mode 1)))
(add-hook 'c-mode-hook 'ccc-common-hook)
(add-hook 'c++-mode-hook 'ccc-common-hook)
(add-hook 'cscope-list-entry-hook 'cscope-minor-mode)



;;; Compile Buffer
(with-eval-after-load 'compile
  (defkey compilation-mode-map  ;; jump to *eshell* buffer
    ("e" (ilambda
          (bury-buffer)
          (awhen (get-buffer-window "*eshell*")
            (let ((wstat (window-dedicated-p it)))
              (set-window-dedicated-p it nil)
              (aw-switch-to-window it)
              (set-window-dedicated-p it wstat))))))
  (add-hook-lambda compilation-mode-hook
    (aif (get-buffer "*compilation*") (switch-to-buffer it))))



;;; SLIME (The Superior Lisp Interaction Mode for Emacs)
(with-eval-after-load 'slime
  (setq inferior-lisp-program (find-if #'executable-find '("sbcl" "ccl" "clisp")))
  (add-to-list 'slime-contribs 'slime-fancy)
  ;; hooks  
  (add-hook-lambda slime-connected-hook
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
  (add-hook-lambda slime-mode-hook
    (global-set-key "\C-cs" 'slime-selector))
  (add-hook-lambda slime-repl-mode-hook
    (define-key slime-repl-mode-map (kbd "TAB") 'hippie-expand))
  (add-hook-lambda slime-autodoc-mode-hook
    (setq-local hippie-expand-try-functions-list
                (append (butlast hippie-expand-try-functions-list 2)
                        (list 'try-expand-slime)))))



;;; Haskell
;;  some Cabal packages needed
(with-eval-after-load 'haskell-mode
  (setq haskell-tags-on-save t
        haskell-process-show-debug-tips nil
        haskell-process-suggest-remove-import-lines t)
  (add-company-backend '(company-ghc :with company-dabbrev-code))
  (add-hook-lambda haskell-mode-hook (interactive-haskell-mode 1)))



;;; Erlang
;; distel is said a good IDE. try some day.
;; other setups later too.



;;; Python-Elpy
;;  install these modules for more features
;;    pip install jedi importmagic
;;    pip install flake8 autopep8
(with-eval-after-load 'python
  (elpy-enable))



;;; Ruby-Mode
;;  rinari is a collection to develop ror
;;  inf-ruby provide a REPL
;;  robe-mode provide company backend.
;; need pry: gem install pry.
;; need invoke: robe-start
(with-eval-after-load 'ruby-mode
  (add-company-backend 'company-robe)
  (add-hook-lambda ruby-mode-hook
    (inf-ruby-minor-mode)
    (robe-mode)))
(with-eval-after-load 'inf-ruby
  (define-key inf-ruby-minor-mode-map (kbd "C-c C-s")
    (ilambda  ;; open rails console, fails then irb
     (or (ignore-errors (inf-ruby-console-auto))
         (ignore-errors (inf-ruby)))
     (robe-start))))



;;; PHP
(with-eval-after-load 'php
  ;; temprary deal the error when org-publish: 'php-variable-name-face not found'
  (defface php-variable-name-face '((t (:inherit font-lock-variable-name-face)))
    "PHP Mode face used to highlight variable names."
    :group 'php-faces))









(provide 'immor)

;;; immor.el ends here
