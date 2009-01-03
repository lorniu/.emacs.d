;;;==============================
;;;   plugins and development
;;;==============================

;;; autoloads
(autoload 'grep-apply-setting "grep")
(autoload 'comint-mode "ls-lisp")
(autoload 'beacon-mode "beacon")



;;; auto-mode-alist
(mapc (lambda (s) (pushnew s auto-mode-alist))
      '(("\\.\\(xml\\|xsl\\)\\'"      . sgml-mode)
        ("\\.\\([xp]?html?\\(.erb\\)?\\|[aj]sp\\|css\\)\\'" . web-mode)
        ("\\.class\\'"                . class-mode)
        ("\\.\\(js\\|htc\\)\\'"       . js2-mode)
        ("\\.\\(jl\\|sawfishrc\\)\\'" . sawfish-mode)
        ("\\.scm\\'"                  . scheme-mode)
        ("\\.hs\\'"                   . haskell-mode)
        ("\\.sqp\\'"                  . sqlplus-mode)
        ("\\.\\(ba\\)?sh\\'"          . sh-mode)
        ("\\.org\\'"                  . org-mode)))



;;; hippie-expand
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



;;; misc
(setq diff-switches "-u")



;;; isearch
;;  C-e become superword forward in isearch
(with-eval-after-load "isearch"
  (define-key isearch-mode-map "\C-e"
    (ilambda
     (superword-mode 1)
     (isearch-yank-word-or-char)
     (superword-mode -1))))



;;; ivy and projectile
(with-eval-after-load 'ivy
  (counsel-projectile-on)
  (setq ivy-use-virtual-buffers  t)
  (setq projectile-cache-file          "~/.emacs.d/.cache/__projectile.cache"
        projectile-known-projects-file "~/.emacs.d/.cache/__projectile-bookmark.eld")
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))



;;; occur
(defun after-occur-follow (&rest args)
  (aif (get-buffer-window "*Occur*") (select-window it)))
(advice-add 'occur :after #'after-occur-follow)



;;; tramp
(setq tramp-default-user "root")
(setq tramp-default-method "sshx")



;;; flyspell
;;  apt install aspell aspell-en
(with-eval-after-load 'flyspell
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-mode-line-string nil)
  (ispell-change-dictionary "american" t))
(when (and nil (executable-find ispell-program-name))
  (require 'flyspell)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))



;;; eshell
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

(add-hook-lambda eshell-mode-hook (define-key eshell-command-map [(control ?l)] 'eshell/ccc))



;;; ibuffer
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



;;; dired
(require 'wdired)

(defun dired-get-size ()
  "z to get the size, used du"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*\\(量\\|total\\)$")
         (match-string 1))))))

(defkey dired-mode-map
  ( "6"   dired-up-directory         )
  ( "r" wdired-change-to-wdired-mode )
  ( "z"   dired-get-size             ))

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



;;; irc and ignus
(advice-add 'irc  :before (lambda (&rest args) (require 'imnet)))
(advice-add 'gnus :before (lambda (&rest args) (require 'imnet)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; program languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook-lambda prog-mode-hook
  (rainbow-delimiters-mode 1)
  (company-mode 1)
  (abbrev-mode 1)
  (hs-minor-mode 1)
  (which-function-mode 1))
(defkey prog-mode-map ( "C-c C-u" backward-up-list ))



;;; web-mode and snippet
(add-hook-lambda web-mode-hook
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-engines-alist
        '(("ruby"   . "\\.html\\.erb\\'")
          ("blade"  . "\\.blade\\.")))
  (emmet-mode 1)
  ;; head/body never indent
  (add-to-list 'web-mode-indentless-elements "html")
  (advice-add 'web-mode-markup-indentation :around
              (lambda (name &rest args)
                (if (member (get-text-property (car args) 'tag-name) '("head"))
                    0 (apply name args)))))
(add-hook-lambda yas-before-expand-snippet-hook
  (make-local-variable 'yas-extra-modes)
  (setq yas-extra-modes
        (let ((web-lang (web-mode-language-at-pos)))
          (cond
           ((equal web-lang "html")       '(html-mode))
           ((equal web-lang "css")        '(css-mode))
           ((equal web-lang "javascript") '(js2-mode))))))



;;
;; Slime (The Superior Lisp Interaction Mode for Emacs)
;;
;; var
(setq quicklisp-home "~/.quicklisp"
      slime-initial  "~/.showcase/lang-lisp/imodule/slime-init.lisp"
      inferior-lisp-program (find-if #'executable-find '("sbcl" "ccl" "clisp")))

;; setup
(add-to-list 'slime-contribs 'slime-fancy)

;; hooks
(add-hook-lambda slime-connected-hook
  ;; initialize quicklisp
  ;; install or load config file
  (let* ((home (file-name-as-directory quicklisp-home))
         (url "http://beta.quicklisp.org/quicklisp.lisp")
         (dist (concat home "quicklisp.lisp")))
    (if (file-exists-p dist)
        (when (file-exists-p slime-initial) ;; custom file
          (with-temp-buffer
            (insert-file-contents slime-initial)
            (slime-eval-buffer))
          (sit-for 1)
          (ignore-errors (slime-repl-set-package "IMFINE")))
      (make-directory home)
      (url-copy-file url dist)
      (with-temp-buffer
        (insert-file-contents dist)
        (goto-char (point-max))
        (insert (format "\n(quicklisp-quickstart:install :path \"%s\")" home))
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
                      (list 'try-expand-slime))))




;;; c-mode
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



;;; compile buffer
(add-hook-lambda compilation-mode-hook

  (aif (get-buffer "*compilation*")
    (switch-to-buffer it))

  ;; jump to *eshell* buffer
  (defkey compilation-mode-map
    ("e" (ilambda
          (bury-buffer)
          (awhen (get-buffer-window "*eshell*")
            (let ((wstat (window-dedicated-p it)))
              (set-window-dedicated-p it nil)
              (aw-switch-to-window it)
              (set-window-dedicated-p it wstat)))))))




;;; Haskell
;;  some cabal packages needed
(add-hook-lambda haskell-mode-hook
  (setq haskell-tags-on-save t)
  (setq haskell-process-show-debug-tips nil)
  (setq haskell-process-suggest-remove-import-lines t)

  (interactive-haskell-mode 1)
  (set (make-local-variable 'company-backends)
       (append '((company-capf company-dabbrev-code)) company-backends)))




;;; erlang
;; distel is said a good ide. try later.
;; other setups later too.



;;; python-elpy
;;  install these modules for more features
;;    pip install jedi importmagic
;;    pip install flake8 autopep8
(with-eval-after-load 'python
  (elpy-enable))



;;; ruby-mode
;;  rinari is a collection to develop ror
;;  inf-ruby provide a REPL
;;  robe-mode provide company backend.
;; need pry: gem install pry.
;; need invoke: robe-start
(add-hook-lambda ruby-mode-hook
  (inf-ruby-minor-mode)
  (robe-mode)
  (pushnew 'company-robe company-backends)
  (setq-local company-minimum-prefix-length 1)

  (define-key inf-ruby-minor-mode-map (kbd "C-c C-s")
    (ilambda  ;; open rails console, fails then irb
     (or (ignore-errors (inf-ruby-console-auto))
         (ignore-errors (inf-ruby)))
     (robe-start))))



;;; php temp
;; org-publish 的时候报错 php-variable-name-face 找不到
;; 这是临时策略，后面找找原因
(defface php-variable-name-face '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable names."
  :group 'php-faces)





(provide 'immor)
;;; immor.el ends here
