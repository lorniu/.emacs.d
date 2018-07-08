;;;; immor.el --- Modules Configuration
;;; Commentary:

;; Download Mirror:
;; http://mirrors.ustc.edu.cn/gnu/emacs/windows/

;;; Code:


;;; Auto-Mode

(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.\\(xml\\|xsl\\)\\'"      . sgml-mode)
        ("\\.class\\'"                . class-mode)
        ("\\.scm\\'"                  . scheme-mode)
        ("\\.\\(ba\\)?sh\\'"          . sh-mode)))


;;; Display-Buffer

(setq display-buffer-alist
      `(("\\*Youdao Dictionary\\*\\|\\*Help\\*\\|\\*Messages\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3))
        ("\\*[cC]ompilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom))
        ("\\*e?shell\\*"
         display-buffer-same-window)))


;;; Hooks

((lambda ()

   (defun -my/before-open ()
     ;; large file
     (when (> (buffer-size) (* 2 1024 1024))
       (setq buffer-read-only t)
       (buffer-disable-undo)
       (fundamental-mode))
     ;; system file
     (and (not (string-match-p "\\(autoloads\\|loaddefs\\).el$" buffer-file-name))
          (let ((case-fold-search nil)) (string-match-p "^/usr/\\|.emacs.d/packages\\|/emacs/" buffer-file-name))
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

   (defun -my/when-close ()
     (recentf-save-list))

   (add-hook 'find-file-hook    '-my/before-open)
   (add-hook 'before-save-hook  '-my/before-save)
   (add-hook 'after-save-hook   '-my/el-compile)
   (add-hook 'auto-save-hook    '-my/idle-once)
   (add-hook 'kill-emacs-hook   '-my/when-close)))



;;;; Basic-Modes

(x winner :init (winner-mode 1))
(x recentf :init (recentf-mode 1))
(x paren :init (show-paren-mode 1))
(x beacon/v :init (beacon-mode 1))

(x desktop :init
   (setq desktop-path `(,_CACHE_ ".")))

(x autorevert :init
   (setq auto-revert-mode-text "")
   (global-auto-revert-mode))

(x key-chord :init
   (setq key-chord-one-key-delay 0.3)
   (setq key-chord-two-keys-delay 0.2)
   (key-chord-mode 1))

(x syntax-subword :init ;; vi-like word move
   (setq syntax-subword-skip-spaces t))


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


;;; Dired/Neotree/Ivy/Swiper/Projectile

(x wdired/e
   :bind
   (:map dired-mode-map
         ( "6"  . dired-up-directory )
         ( "r"  . wdired-change-to-wdired-mode )
         ( "z"  . dired-du-mode )
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
   (setq smex-save-file (concat _CACHE_ "smex-items")) ;; frequently used cmds
   (ivy-mode 1))

(x swiper/w)

(x counsel-projectile/w
   :init
   (setq projectile-completion-system 'ivy
         projectile-cache-file (concat _CACHE_ "__projectile.cache")
         projectile-known-projects-file (concat _CACHE_ "__projectile-bookmark.eld")
         counsel-find-file-ignore-regexp "Volume\\|RECYCLE.BIN"
         projectile-mode-line '(:eval (if (string= "-" (projectile-project-name)) nil
                                        (list " ["
                                              `(:propertize ("" "ᴘ")
                                                            face (:weight bold)
                                                            help-echo ,(format "[%s]:\n\n%s%s/\n"
                                                                               (projectile-project-type)
                                                                               (projectile-project-root)
                                                                               (projectile-project-name)))
                                              "]"))))
   (projectile-mode 1)
   (counsel-projectile-mode 1))


;;; HS-Minor-Mode/Outline-Minor-Mode

(x hideshow
   :delight hs-minor-mode
   :hook ((web-mode prog-mode) . hs-minor-mode)
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
         ( "<DEL>" .  nil )))


;;; Magit

(when (executable-find "git")
  (x magit/w
     :bind ("C-c m" . magit-status)
     :init (magit-auto-revert-mode -1)))


;;; Translate

(x youdao-dictionary/w :init ;; pacman -S mpg123
   (setq url-automatic-caching t
         ;; youdao-dictionary-use-chinese-word-segmentation t
         youdao-dictionary-search-history-file (concat _CACHE_ ".youdao")))


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

(x eldoc/v)

(x prog-mode
   :bind (:map prog-mode-map ("C-c C-u" . backward-up-list))
   :init (add-hook-lambda 'prog-mode-hook
           (setq show-trailing-whitespace t)
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

(x company/w
   :hook ((prog-mode   . company-mode))
   :bind (:map company-active-map
               ("C-c"   . company-abort)
               ("<SPC>" . -my/insert-blank))
   :init
   (setq company-idle-delay 0.2
         company-lighter-base "")
   (defun add-company-backend (backend)
     (add-to-list 'company-backends backend))
   (defun -my/insert-blank () (interactive) (company-abort) (insert " ")))

(x yasnippet/ev
   :bind
   (:map yas-keymap ("C-m" . 'yas-next-field-or-maybe-expand))
   :init
   (setq yas-verbosity 2
         yas-alias-to-yas/prefix-p nil)
   :config
   (defun -my/yasnipet-hook ()
     (delight '((yas-minor-mode "" yasnippet)))
     (yas-activate-extra-mode 'fundamental-mode))
   (add-hook 'yas-minor-mode-hook '-my/yasnipet-hook)
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

;;; SQL
;;
;; M-x: sql-connect
;;
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
   :mode "\\.\\([xp]?html\\(.erb\\|.blade\\)?\\|[aj]sp\\|jsx\\|tpl\\|css\\|vue\\)\\'"
   :config
   (setq web-mode-markup-indent-offset    4
         web-mode-css-indent-offset       4
         web-mode-code-indent-offset      4
         web-mode-enable-css-colorization t
         web-mode-content-types-alist '(("jsx"    . "\\.js[x]?\\'"))
         web-mode-engines-alist       '(("blade"  . "\\.blade\\.")
                                        ("ruby"   . "\\.html\\.erb\\'")))

   (defun -my/web-mode-hook ()
     ;; (flycheck-mode +1)
     (hs-minor-mode -1)
     (set (make-local-variable 'company-backends)
          '(company-css company-web-html company-yasnippet company-keywords company-files)))

   (defun -my/web-mode-bore-complete ()
     (let ((curr-lang (web-mode-language-at-pos)))
       (cond ((string= curr-lang "css")
              (setq emmet-use-css-transform t))
             (t (setq emmet-use-css-transform nil)))))

   (defun -my/yas-before-expand ()
     (if (eq major-mode 'web-mode)
         (set (make-local-variable 'yas--extra-modes)
              (pcase (web-mode-language-at-pos)
                ("html"           '(html-mode))
                ("css"            '(css-mode))
                ("javascript"     '(js-mode))))))

   (add-hook 'web-mode-hook '-my/web-mode-hook)
   (add-hook 'web-mode-before-auto-complete-hooks '-my/web-mode-bore-complete)
   (add-hook 'yas-before-expand-snippet-hook '-my/yas-before-expand)

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
     (setq mode-name "J2")
     (flycheck-mode +1)
     (js2-imenu-extras-mode +1))

   (x web-beautify :if (executable-find "js-beautify")))

(when (executable-find "node")
  (x tide/w
     :delight " ť"
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


;;; Lisp/SLIME (The Superior Lisp Interaction Mode for Emacs)

(x slime :config
   (setq inferior-lisp-program (seq-find #'executable-find '("sbcl" "ccl" "clisp")))
   (add-to-list 'slime-contribs 'slime-fancy)

   ;; function

   (defun try-expand-slime (old)
     "Hippie Expand OLD word for slime."
     (when (not old)
       (he-init-string (slime-symbol-start-pos) (slime-symbol-end-pos))
       (setq he-expand-list
             (or (equal he-search-string "")
                 (sort (slime-simple-completions he-search-string) #'string-lessp))))
     (if (null he-expand-list)
         (progn (if old (he-reset-string)) ())
       (he-substitute-string (car he-expand-list))
       (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
       (setq he-expand-list (cdr he-expand-list))
       (message "Slime Expand") t))

   ;; hooks

   (add-hook-lambda 'slime-connected-hook
     ;; [quicklisp] load or initial
     (let* ((ql-home (file-name-as-directory "~/.quicklisp"))
            (ql-url "http://beta.quicklisp.org/quicklisp.lisp")
            (ql-dist (concat ql-home "quicklisp.lisp"))
            (slime-init "~/.notes/x.code.lsp/misc/slime-init.lisp"))
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

(add-hook-lambda 'lisp-mode-hook (electric-pair-mode 1))


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
(x haskell-mode :init
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
   :init (add-hook-lambda 'dante-mode-hook
           (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))
   :config (load "patches" nil nil))


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

;;; Tooltips

;; remove `Origami', I don't like it. 20180111
;; remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; remove `iedit', as I found `auto-highlight-symbol' is so powerful, and the code is amazing! I will hack it for fun. 2018-07-03
