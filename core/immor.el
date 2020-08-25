;;; immor.el --- Modules Configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'transient-command)



;; Auto-Mode Rules

(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.class\\'"           . class-mode)
        ("\\.scm\\'"             . scheme-mode)
        ("\\.\\(ini\\|inf\\)\\'" . conf-mode)
        ("\\.\\(ba\\)?sh\\'"     . sh-mode)))

;; Auto-Compression Rules
;; - for .rar, you should install `unarchiver'.

(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])
(jka-compr-update)

;; Buffer-Display Rules

(setq even-window-heights nil)

(setq display-buffer-alist
      `(("\\*Compile-Log\\*"            ; regexp to filter buffer-name
         (display-buffer-reuse-window)  ; functions to deal with display
         (window-height . 0.3)          ; parameters to pass to functions above
         (window-width . 0.3))

        ("\\*[cC]ompilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom))

        ("\\*Messages\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (reusable-frames . t))

        ("\\*Youdao Dictionary\\*\\|\\*Help\\*\\|\\*Shell Command Output\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (reusable-frames . t))

        ("\\*\\(e?shell[-+]?\\|PowerShell\\|Python\\)\\*\\|[-+]\\(shell\\|vterm\\)[-+]"
         (display-buffer-same-window)
         (reusable-frames . t))

        ("\\*Async Shell Command\\*"
         (%display-buffer-at-bottom-follows-with-quit)
         (window-height . 0.3))

        ("." nil (reusable-frames . t))))

(defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
  (display-buffer-at-bottom buffer alist)
  (select-window (get-buffer-window buffer))
  (with-current-buffer buffer (view-mode 1)))

;; Open/Save/Sudo

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "Edit as ROOT"
  :group 'imfine)

(defun su ()
  "Edit file as Super User."
  (interactive)
  (let ((pt (point)) (old-buf (current-buffer))
        (buf-name (expand-file-name (or buffer-file-name default-directory))))
    (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
    (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
      (find-file buf-name))
    (kill-buffer-if-not-modified old-buf)
    (goto-char pt)))

(defun my-find-file-root-header-warning ()
  (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
    (setq header-line-format (propertize " Edit as ROOT! " 'face 'find-file-root-header-face))))

(add-hook 'find-file-hook 'my-find-file-root-header-warning)
(add-hook 'dired-mode-hook 'my-find-file-root-header-warning)

(defvar my-readonly-dir nil)
(defun im/dont-touch-my-childen ()
  (interactive)
  (let ((dir (read-directory-name "Directory: " nil " ")))
    (if (and dir (file-directory-p dir))
        (progn (setq my-readonly-dir (expand-file-name dir))
               (message "Set `ic/temp-read-only-dir' to %s" dir))
      (setq my-readonly-dir nil)
      (user-error "Error directory, set to nil."))))
(add-hook-fun find-file-hook/readonly ()
  "Files that should be readonly."
  (let* ((case-fold-search nil)
         (match-p (lambda (&rest items)
                    (string-match-p (apply #'join-as-regor-group items)
                                    buffer-file-name))))
    (when (or (and my-readonly-dir
                   (string-prefix-p my-readonly-dir buffer-file-name))
              (and (not (funcall match-p ; exclude
                                 "/usr/home"
                                 "\\.cache/" "tmp/" "vvv/" "notes"
                                 "autoloads.el$" "loaddefs.el$"))
                   (funcall match-p ; include
                            "^/usr/"
                            ".emacs.d/packages"
                            ".roswell/lisp/quicklisp")))
      (view-mode 1)
      (make-local-variable 'view-mode-map)
      (define-key view-mode-map "q" 'View-kill-and-leave))))

(add-hook-fun before-save-hook ()
  ;; auto remove trailing whitespace
  (unless (cl-find major-mode '(org-mode))
    (delete-trailing-whitespace)))

(add-hook-fun after-save-hook ()
  (save-window-excursion
    (when (and (eq major-mode 'emacs-lisp-mode)
               (string-match-p "^[a-z1-9]" (buffer-name))
               (string-match-p "\\/\\.emacs\\.d\\/\\(core\\|extra\\)\\/" (buffer-file-name)))
      (byte-compile-file (buffer-file-name)))))


;;; Common

;; - remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; - remove `iedit', as `auto-highlight-symbol' is amazing. Hack it and enjoy it! 2018-07-03

(setq desktop-path `(,(locc) "."))
(setq syntax-subword-skip-spaces t)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(savehist-mode 1)
(show-paren-mode +1)
(setq auto-revert-mode-text "") (global-auto-revert-mode 1)
(add-hook-fun edebug-mode-hook () (view-mode -1))
(when (fboundp 'global-so-long-mode) (global-so-long-mode))

(x recentf
   :init
   (setq recentf-max-saved-items 200)
   (setq recentf-exclude '((expand-file-name package-user-dir)
                           ".cache"
                           ".cask"
                           ".elfeed"
                           "bookmarks"
                           "cache"
                           "ido.*"
                           "persp-confs"
                           "recentf"
                           "undo-tree-hist"
                           "url"
                           "COMMIT_EDITMSG\\'"))
   (defun recentf-clear ()
     (interactive)
     (setq recentf-list nil)
     (recentf-save-list))
   (recentf-mode 1))

(x isearch
   :config
   (setq isearch-lazy-count t) ; replace Anzu
   (bind-key "\C-e" ; Superword-Mode when select
             (lambda ()
               (interactive)
               (superword-mode 1)
               (unwind-protect
                   (call-interactively 'isearch-yank-word-or-char)
                 (superword-mode -1)))
             isearch-mode-map))

(x replace
   :config
   (bind-key "M-s o" ; Word under cursor first
             (lambda (str)
               (interactive (list (aif (symbol-at-point) (symbol-name it))))
               (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))
   (define-key occur-mode-map "r" 'occur-edit-mode)
   (add-function :after (symbol-function 'occur) ; After Occur, jump to the result buffer.
                 (lambda (&rest _) (aif (get-buffer-window "*Occur*") (select-window it)))))

(x ctrlf/-
   "Want to replace isearch and anzu, but then give up. Weird feeling.")

(x rg
   "pacman -S ripgrep"
   :ref ("dajva/rg.el"
         "ripgrep download page: BurntSushi/ripgrep/releases")
   :init
   (setq rg-group-result nil)
   (setq rg-ignore-case 'smart)
   (setq rg-align-column-number-field-length 4)

   :config
   (defun rg-rerun-toggle-group ()
     "Toggle show group in `rg-cur-search`."
     (interactive)
     (setq rg-group-result (not rg-group-result))
     (rg-rerun))

   (define-key rg-mode-map "G" 'rg-rerun-toggle-group))

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

(x page-break-lines
   :init
   (setq page-break-lines-lighter "")
   (global-page-break-lines-mode 1)
   :config
   (nconc page-break-lines-modes '(web-mode css-mode haskell-mode)))

(x ediff
   :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))

(x abbrev/d
   :config
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file)))

(x hippie-exp
   :init
   (setq hippie-expand-try-functions-list
         '(try-expand-dabbrev
           try-expand-dabbrev-visible
           try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
           try-complete-file-name-partially try-complete-file-name
           try-expand-all-abbrevs try-expand-list try-expand-line
           try-complete-lisp-symbol-partially try-complete-lisp-symbol)))

(x ibuffer/e
   :config
   (setq ibuffer-show-empty-filter-groups nil
         ibuffer-saved-filter-groups
         `(("default"
            ("apple"
             (or (predicate . (eq (get major-mode 'derived-mode-parent) 'prog-mode))
                 (predicate . (member major-mode '(nxml-mode sgml-mode)))))
            ("banana"
             (or (mode . org-mode)
                 (mode . tar-mode)))
            ("orange"
             (or (mode . erc-mode)
                 (mode . rcirc-mode)))
            ("melon"
             (or (name . "^\\*?magit")))
            ("peach"
             (or (name . "\\*.+\\*")))
            ("durin"
             (or (mode . dired-mode))))))

   (add-hook-fun ibuffer-mode-hook ()
     (ibuffer-switch-to-saved-filter-groups "default")))

(x dired
   "Maybe should add this to .bashrc:
   "
   "  export LC_COLLATE=C
   "
   :bind
   (:map dired-mode-map
         ( "6" . dired-up-directory )
         ( "r" . wdired-change-to-wdired-mode )
         ( "." . imdra-dired/body )
         ( "s" . hydra-dired-quick-sort/body )
         ( "z" . idp/dired-du-size )
         ( "Y" . idp/dired-rsync))
   :init
   (require 'wdired)
   (setq wgrep-enable-key "r")

   :config
   (require 'ls-lisp)
   (setq dired-listing-switches "-alh"
         ls-lisp-use-insert-directory-program nil
         ls-lisp-dirs-first t
         ls-lisp-use-string-collate nil
         ls-lisp-UCA-like-collation nil
         ls-lisp-use-localized-time-format t
         ls-lisp-format-time-list '("%Y/%m/%d %H:%M" "%Y/%m/%d %H:%M"))

   (defvar ls-lisp-xid-shorten-threshold (if IS-WIN 8)
     "Shorten display the Uid/Gid column, eg, Administrators is toooooo long.")

   (defun:before ls-lisp-sanitize$ (file-alist)
     (when ls-lisp-xid-shorten-threshold
       (cl-labels ((norm (file n &optional
                               (threshold ls-lisp-xid-shorten-threshold)
                               (len (- ls-lisp-xid-shorten-threshold 3)))
                         (let ((item (nth n file)))
                           (when (and (stringp item) (> (length item) threshold))
                             (setf (nth n file)
                                   (propertize (format "%s~" (upcase (cl-subseq item 0 len)))
                                               'help-echo item))))))
         (dolist (file file-alist)
           (norm file 3)
           (norm file 4)))))

   ;; others
   (setq dired-dwim-target t)
   (require 'im-dired-plus))

(x vlf
   "Open huge file, Part by Part."
   :ref "m00natic/vlfi"
   :init (require 'vlf-setup))

(x view
   :bind
   (:map view-mode-map
         ( "h"       .  backward-char )
         ( "l"       .  forward-char  )
         ( "j"       .  next-line     )
         ( "k"       .  previous-line )
         ( "%"       .  his-match-paren )
         ( "<DEL>"   .  nil )
         ( "C-x C-e" .  eval-last-sexp )))

(x flyspell/x
   :preface (setq ispell-program-name "ispell") ;; apt install ispell
   :if (executable-find ispell-program-name)
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (rcirc-mode . flyspell-mode))
   :config (ispell-change-dictionary "american" t))

(x image-mode
   :config
   (define-key image-mode-map "c" 'im/yank-current-buffer-name))

(x which-key/d
   :init
   (setq which-key-idle-delay 1.5)
   (which-key-mode))

(x transient
   :ref "magit/transient")

(x treemacs/i
   :ref "Alexander-Miller/treemacs"
   :config
   (setq treemacs-position 'left
         treemacs-persist-file (locc "treemacs-persist")
         treemacs-width 35
         treemacs-indentation 2
         treemacs-no-png-images nil)
   (treemacs-without-messages (treemacs-resize-icons 16))
   (treemacs-follow-mode t)
   (treemacs-filewatch-mode t)
   (treemacs-fringe-indicator-mode t))

(x projectile/di
   :ref "bbatsov/projectile"
   :init
   (defvar projectile-mode-line
     '(:eval (let ((name (projectile-project-name)))
               (if (and (plusp (length name))
                        (not (string-equal name "-")))
                   (propertize (format "<%s>  " name) 'face 'font-lock-comment-face)
                 " "))))

   (setq projectile-completion-system 'default
         projectile-cache-file (locc "__projectile.cache")
         projectile-known-projects-file (locc "__projectile-bookmark.eld")
         projectile-mode-line-prefix ""
         projectile-git-submodule-command nil)

   (projectile-mode +1)

   (setq projectile-globally-ignored-file-suffixes '(".class" ".jar" ".exe" ".dll" ".so"))
   (add-to-list 'projectile-globally-ignored-directories ".gradle")
   (add-to-list 'projectile-project-root-files ".pro")
   (add-to-list 'projectile-project-root-files "package.json")
   (add-to-list 'projectile-project-root-files-bottom-up ".idea"))


;;; Tramp/Shell

(when (and IS-WIN ; Cygwin/Msys2/Git-for-Win + fakecygpty for better TERMINAL
           (executable-find "bash"))
  ;; If `Spawning child process: Exec format error` occurred, recompile or change permissions.
  (add-to-list 'exec-path (expand-file-name (loce "extra/fakecygpty/")))
  (require 'fakecygpty)
  ;; (add-to-list 'fakecygpty-ignored-program-regexps "httpd")
  (fakecygpty-activate))

(x tramp/i
   :init
   (setq tramp-verbose 1)
   (setq tramp-persistency-file-name (locc "tramp"))
   (setq remote-file-name-inhibit-cache nil)
   (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

   :config
   ;; fix a bug on FreeBSD: echo `uname -r` does not return a valid Lisp expression...
   (when IS-BSD
     (require 'tramp-sh)
     (defun:before tramp-send-string$ (_ str)
       (when (string-match-p "`uname -sr`" str)
         (sit-for 1)))))

(x term
   :config
   (defun my-term-send-forward-word () (interactive) (term-send-raw-string "\ef"))
   (defun my-term-send-backward-word () (interactive) (term-send-raw-string "\eb"))
   (define-key term-raw-map [C-left] 'my-term-send-backward-word)
   (define-key term-raw-map [C-right] 'my-term-send-forward-word)

   (add-hook-fun term-mode-hook ()
     (goto-address-mode 1))

   (defun:after term-handle-exit$ (&rest _)
     (use-local-map (let ((map (make-sparse-keymap))) (define-key map "q" #'kill-current-buffer) map))))

(x shell
   :init
   (when (and IS-WIN (executable-find "bash"))
     ;; use bash.exe as default process if possible
     (setenv "PS1" "\\u@\\h \\w $ ")
     (setq explicit-shell-file-name (executable-find "bash"))
     (setq explicit-bash.exe-args '("--login" "-i")))

   (add-hook-fun shell-mode-hook ()
     (when (and IS-WIN ;; cmd/powershell on windows, should be gbk encoding
                (not (string-match-p "bash" (or explicit-shell-file-name ""))))
       (im/local-encoding 'cp936-dos))
     (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))

   :config
   (defun:around sh-set-shell$ (orig-fun &rest args)
     "Dont show message: Indentation setup for shell type bash"
     (cl-letf (((symbol-function 'message) #'ignore))
       (apply orig-fun args))))

(x eshell/i
   :init
   (setq eshell-history-size 200
         eshell-hist-ignoredups t
         eshell-aliases-file (loce "share/eshell.alias")
         comint-scroll-show-maximum-output nil
         eshell-scroll-show-maximum-output nil)

   ;; Visual Commands
   (setq eshell-visual-commands
         '("vim" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"))
   (setq eshell-visual-subcommands
         '(("git" "log" "diff" "show" "reflog")
           ("sudo" "vi" "visudo") ("sbt")))
   (setq eshell-visual-options
         '(("git" "--amend")))
   (setq eshell-destroy-buffer-when-process-dies t)

   (defun:override eshell-exec-visual$ (&rest args)
     "Add some options for specific visual command."
     (let* (eshell-interpreter-alist
            (interp (eshell-find-interpreter (car args) (cdr args)))
            (program (car interp))
            (args (flatten-tree (eshell-stringify-list (append (cdr interp) (cdr args)))))
            (term-buf (generate-new-buffer (concat "*" (file-name-nondirectory program) "*")))
            (eshell-buf (current-buffer))
            (proc-encoding nil))
       (save-current-buffer
         (when (and IS-WIN  ; git in Windows, page break
                    (string-match-p "git" program))
           (setq args (append (list "-c" "core.pager=less -+F") args)))
         ;;;;
         (switch-to-buffer term-buf 'norecord)
         (term-mode)
         (set (make-local-variable 'term-term-name) eshell-term-name)
         (make-local-variable 'eshell-parent-buffer)
         (setq eshell-parent-buffer eshell-buf)
         (term-exec term-buf program program nil args)
         (let ((proc (get-buffer-process term-buf)))
           (if (and proc (eq 'run (process-status proc)))
               (progn (if proc-encoding (set-process-coding-system proc proc-encoding proc-encoding))
                      (set-process-sentinel proc 'eshell-term-sentinel))
             (error "Failed to invoke visual command")))
         (term-char-mode)
         (if eshell-escape-control-x (term-set-escape-char ?\C-x))))
     nil)

   (defun:override eshell-term-sentinel$ (proc msg)
     "Q to quit visual-command mode."
     (term-sentinel proc msg)
     (let ((proc-buf (process-buffer proc)))
       (when (and eshell-destroy-buffer-when-process-dies
                  (plusp (- (line-number-at-pos (point-max)) (window-body-height) 1))) ; this, smarter
         (when (and proc-buf (buffer-live-p proc-buf)
                    (not (eq 'run (process-status proc)))
                    (= (process-exit-status proc) 0))
           (if (eq (current-buffer) proc-buf)
               (let ((buf (and (boundp 'eshell-parent-buffer)
                               eshell-parent-buffer
                               (buffer-live-p eshell-parent-buffer)
                               eshell-parent-buffer)))
                 (if buf (switch-to-buffer buf))))
           (kill-buffer proc-buf)))))

   ;; 256-color
   (setenv "TERM" "xterm-256color")
   (add-hook-fun eshell-before-prompt-hook ()
     (setq xterm-color-preserve-properties t)
     (setq eshell-preoutput-filter-functions '(xterm-color-filter))
     (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

   ;; mode hook
   (add-hook-fun eshell-mode-hook ()
     (setq-local company-idle-delay 1.2)
     (define-key eshell-mode-map (kbd "C-r") 'eshell/h)
     (define-key eshell-mode-map (kbd "C-c M-o") (lambda () (interactive) (recenter-top-bottom 0))))

   :config
   (defun eshell/! (&rest args)
     "Combination eshell and native-shell-command !"
     (let ((cmd (eshell-flatten-and-stringify args)))
       (shell-command-to-string cmd)))
   (defun eshell/h ()
     (interactive)
     (require 'em-hist)
     (let* ((start-pos (save-excursion (eshell-bol) (point)))
            (end-pos (point))
            (input (buffer-substring-no-properties start-pos end-pos))
            (hists (delete-dups (when (> (ring-size eshell-history-ring) 0)
                                  (ring-elements eshell-history-ring))))
            (command (completing-read "Command: " hists
                                      nil nil nil nil
                                      (if (plusp (length input)) input
                                        (car hists)))))
       (setf (buffer-substring start-pos end-pos) command)
       (end-of-line)))
   (defun eshell/ssh (&rest args)
     "Secure shell."
     (let ((cmd (eshell-flatten-and-stringify (cons "ssh" args)))
           (display-type (framep (selected-frame))))
       (cond
        ((and (eq display-type 't) (getenv "STY"))
         (send-string-to-terminal (format "\033]83;screen %s\007" cmd)))
        (t (apply 'eshell-exec-visual (cons "ssh" args)))))))

(x vterm
   "Add something like this to .bashrc:
   "
   "VtInit=~/.notes/x.share/emacs/vterm-boot.sh"
   "if [[ \"$INSIDE_EMACS\" = \"vterm\" ]] && [[ -f $VtInit ]]; then source $VtInit; fi
   "
   :ref "akermu/emacs-libvterm"
   :no-require t
   :if module-file-suffix
   :init
   (defvar vterm-dir-history (make-ring 10))
   :config
   (defun vterm (&optional buffer-name)
     (interactive)
     (let ((buf (or buffer-name "*vterm*")))
       (unless (get-buffer buf)
         (get-buffer-create buf)
         (with-current-buffer buf (vterm-mode)))
       (switch-to-buffer buf 'norecord)))
   (defun my/vterm-to-dir (dir)
     (interactive (list (completing-read "Dir History: " (ring-elements vterm-dir-history) nil t)))
     (if dir (vterm-send-string (format "cd %s\n" dir))))
   (define-key vterm-mode-map [f11] 'bury-buffer)
   (define-key vterm-mode-map (kbd "C-c b") 'my/vterm-to-dir)
   (define-key vterm-mode-map (kbd "C-c C-b") 'my/vterm-to-dir))

(defun im/popup-shell (&optional _)
  (interactive "P")
  (if (eq major-mode 'shell-mode)
      (bury-buffer)
    (let ((curr-dir default-directory))
      (shell "+shell+")
      (ring-insert comint-input-ring
                   (concat "cd " (shell-quote-argument curr-dir))))))

(defun im/popup-vterm (&optional _)
  (interactive "P")
  (if (eq major-mode 'vterm-mode)
      (bury-buffer)
    (let ((curr-dir default-directory)
          (buf-name "+vterm+")
          (ring-top (ignore-errors (ring-ref vterm-dir-history 0))))
      (unless (string= ring-top curr-dir)
        (ring-insert vterm-dir-history curr-dir))
      (vterm buf-name))))

(defun im/popup-shell-or-vterm ()
  (interactive)
  (cl-flet ((exist (ext) (file-exists-p (format "%s%s.%s" (file-name-directory (file-truename (locate-library "vterm"))) "vterm-module" ext))))
    (if (or (exist "dll") (exist "so"))
        (condition-case nil
            (call-interactively 'im/popup-vterm)
          (error (call-interactively 'im/popup-shell)))
      (call-interactively 'im/popup-shell))))

(defun im/popup-eshell (&optional arg)
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (bury-buffer)
    (let ((last-file (or dired-directory (buffer-file-name))))
      (eshell arg)
      (if last-file (eshell-add-to-dir-ring (file-name-directory last-file))))))

(defun im/popup-xshell ()
  (interactive)
  (let ((type (completing-read "Shell: " '(powershell system-default) nil t)))
    (if (string= type "powershell")
        (powershell)
      (let ((explicit-shell-file-name (getenv "SHELL")))
        (shell "+shell+")))))


;;; Code Folding

;; - *C-x $
;; - *hideshow
;; - *outline-minor-mode
;; - narrow/imenu
;; - Folding, ;; {{{ It makes code dirty, ;; }}}
;; - Origami, try and removed, I don't like it. 20180111

(x hideshow
   "Use `hs-special-modes-alist' to set rules:
  (MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC)
"
   :delight hs-minor-mode
   :hook ((prog-mode nxml-mode) . hs-minor-mode)
   :bind* (:map hs-minor-mode-map
                ([(M-down-mouse-1)] . nil)
                ([(M-mouse-1)] . hs-mouse-toggle-hiding)
                ("C-c f"   . hs-toggle-hiding)
                ("C-c F"   . im/hs-toggle-all)
                ("C-c C-f" . imdra-hideshow/body))
   :init
   (defun hs-add-rule (mode start end comment &optional forward-sexp-func adjust-beg-func)
     (add-to-list 'hs-special-modes-alist (list mode start end comment forward-sexp-func adjust-beg-func)))
   :config
   (defun im/hs-toggle-all ()
     (interactive)
     (save-mark-and-excursion
       (if (seq-find
            (lambda (ov) (and (overlayp ov) (overlay-get ov 'hs)))
            (overlays-in (point-min) (point-max)))
           (hs-show-all)
         (hs-hide-all))
       (recenter)))
   (defun im/hs-toggle-current ()
     (interactive)
     (save-mark-and-excursion
       (call-interactively 'hs-toggle-hiding)))
   (defun im/hs-toggle-level ()
     (interactive)
     (save-mark-and-excursion
       (hs-life-goes-on
        (if (hs-already-hidden-p)
            (hs-show-block)
          (call-interactively 'hs-hide-level)))))
   (defun my-display-code-line-counts (ov)
     (when (eq 'code (overlay-get ov 'hs))
       (overlay-put ov 'display
                    (concat " "
                            (propertize
                             (format "...%d..." (count-lines (overlay-start ov) (overlay-end ov)))
                             'face font-lock-warning-face)))))
   (setq hs-set-up-overlay 'my-display-code-line-counts))

(x outline
   "Use `outline-regexp' to set rule of buffer."
   :delight outline-minor-mode
   :hook ((prog-mode text-mode) . outline-minor-mode)
   :commands (im/outline-toggle-all)
   :bind* (:map outline-minor-mode-map
                ("C-c o"   . outline-toggle-children)
                ("C-c O"   . im/outline-toggle-all)
                ("C-c C-o" . my/open-outline-panel))
   :init
   (defun my/open-outline-panel ()
     (interactive)
     (cond ((eq major-mode 'org-mode) (call-interactively 'org-open-at-point))
           (t (call-interactively 'imdra-outline/body))))
   :config
   (defvar-local outline-fold-all-flag nil)
   (defvar-local outline-narrow-flag nil)
   (defun im/outline-toggle-all ()
     (interactive)
     (if outline-fold-all-flag
         (progn (outline-show-all) (setq outline-fold-all-flag nil))
       (outline-hide-sublevels 1)
       (setq outline-fold-all-flag t)))
   (defun im/outline-toggle-narrow ()
     (interactive)
     (if outline-narrow-flag
         (progn (outline-show-all) (setq outline-narrow-flag nil))
       (outline-hide-other)
       (setq outline-narrow-flag t))))

(x fold-this
   :ref "magnars/fold-this.el"
   :config
   (defun:filter-args fold-this$whole-line (args)
     (let ((beg (car args))
           (end (cadr args))
           (header (caddr args)))
       (list (save-excursion (goto-char beg)
                             (beginning-of-line)
                             (point))
             (save-excursion (goto-char end)
                             (end-of-line)
                             (point))
             header))))

(advice-add 'set-selective-display
            :filter-args (lambda (args)
                           (if (or (car args) selective-display)
                               args
                             (list (1+ (current-column)))))
            '((name . set-selective-display-from-cursor-column)))

(defhydra imdra-hideshow (:foreign-keys run :hint nil)
  "\nToggle: (F/f/l), Show: (S/s), Hide: (H/h/L)"
  ("S" hs-show-all)
  ("s" hs-show-block)
  ("H" hs-hide-all)
  ("h" hs-hide-block)
  ("L" hs-hide-level)
  ("F" im/hs-toggle-all)
  ("f" im/hs-toggle-current)
  ("l" im/hs-toggle-level)
  ("q" nil)
  ("SPC" nil))

(defhydra imdra-outline (:hint nil)
  "\nToggle: (O/o/N)  Subtree: (s/d)  Nav: (u/n/p/l)  ShowBranches: (k)  HideSubLevels: (q)"
  ;; Hide
  ("q" outline-hide-sublevels)
  ("d" outline-hide-subtree)
  ("N" im/outline-toggle-narrow)
  ;; Show
  ("k" outline-show-branches)
  ("s" outline-show-subtree)
  ;; Toggle
  ("o" outline-toggle-children)
  ("O" im/outline-toggle-all)
  ;; Move
  ("u" outline-up-heading)
  ("n" outline-next-visible-heading)
  ("p" outline-previous-visible-heading)
  ("l" recenter-top-bottom)
  ("SPC" nil))

(defhydra imdra-narrow (:hint nil :exit t :idle 0.8)
  "
 narrow  _d_ → defun   _b_ → org-block    _w_ → widen
         _n_ → region  _e_ → org-element
         _p_ → page    _s_ → org-subtree
"
  ("b" org-narrow-to-block)
  ("e" org-narrow-to-element)
  ("s" org-narrow-to-subtree)
  ("d" narrow-to-defun)
  ("n" narrow-to-region)
  ("p" narrow-to-page)
  ("w" widen))

(global-set-key (kbd "C-x n") 'imdra-narrow/body)


;;; Navigation

(x winner
   :init (winner-mode 1)
   :config
   (define-key winner-mode-map [C-left] 'winner-undo)
   (define-key winner-mode-map [C-right] 'winner-redo))

(x ivy/-
   "Mess of its implements. Replace with selectrum."
   :ref "abo-abo/swiper")

(x ace-window
   :bind ("C-x w" . ace-window)
   :config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(x hydra
   :ref "abo-abo/hydra"
   :init
   (defvar my-hydra-stack nil)

   (defun hydra-push (expr)
     (push `(lambda () ,expr) my-hydra-stack))

   (defun hydra-pop ()
     (interactive)
     (let ((x (pop my-hydra-stack)))
       (when x (funcall x)))))

(x ztree
   "Use `ztree-diff' to diff directories."
   :ref "fourier/ztree"
   :config
   (setq ztree-draw-unicode-lines t)
   (setq ztree-diff-additional-options nil) ; '("-w" "-i")
   (add-to-list 'ztree-diff-filter-list "^~"))


;;; Net

(x eww
   :init
   (setq eww-search-prefix "https://duckduckgo.com/html/?q=") ; https://www.google.com/search?q=
   (if IS-NG (setq browse-url-browser-function 'eww-browse-url)))

(x simple-httpd/+
   "Start local server with port 5555:
   "
   " - M-x im/http-here
   "
   "Define your own servlet:
   "
   "  (defservlet time text/html () (insert (format \"%s\" (time-str))))
   "
   "Then you can visit with 'http://host:5555/time'
   "
   :commands
   (httpd-start httpd-stop httpd-running-p httpd-serve-directory)

   :init
   (defun im/httpd-here (&optional arg)
     (interactive "P")
     (let ((root default-directory)
           (port (if arg (read-number "Port: " 5555) 5555)))
       (httpd-start :port port :root root)
       (message "http://localhost:%s | %s" port root)))

   :config
   (defservlet time text/html ()
     (insert (format "<h1>%s</h1>" (time-str)))))

(x engine-mode
   :init (engine-mode 1)
   :config
   (require 'format-spec)

   (defengine google         "https://google.com/search?q=%s")
   (defengine google-cn      "https://google.com/search?q=%s&lr=lang_zh-CN")
   (defengine dict-iciba     "http://www.iciba.com/%s")
   (defengine github         "https://github.com/search?ref=simplesearch&q=%s")
   (defengine stackoverflow  "http://stackoverflow.com/search?q=%s")
   (defengine iconify        "https://iconify.design/icon-sets/?query=%s")
   (defengine wikipedia      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
   (defengine arch-wiki      "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go" :browser 'eww-browse-url)
   (defengine wolfram-alpha  "http://www.wolframalpha.com/input/?i=%s")
   (defengine youtube        "http://www.youtube.com/results?aq=f&oq=&search_query=%s"))

(x youdao-dictionary
   "Text to speech support, when unix-like:
   "
   "  pacman -S mpg123
   "
   "youdao-dictionary-use-chinese-word-segmentation t
   "
   :init
   (setq url-automatic-caching t
         youdao-dictionary-search-history-file (locc "youdao"))
   (defun:override youdao-dictionary--play-voice$ (word)
     (let ((player (or (executable-find "mpv")
                       (executable-find "mplayer")
                       (executable-find "mpg123"))))
       (if player
           (start-process player nil player (youdao-dictionary--format-voice-url word))
         (if (executable-find "powershell")
             (powershell-execute (format "$w = New-Object -ComObject SAPI.SpVoice; $w.speak(\\\"%s\\\")" word))
           (user-error "mplayer/mpg123 or powershell is needed to play word voice")))))
   :config
   (define-key youdao-dictionary-mode-map "s" (lambda () (interactive) (message "%s" 'Add-To-List-TODO))))

(x go-translate/i
   "My rewrite of `google-translate' package."
   :ref ("lorniu/go-translate"
         "Google Translate: https://translate.google.cn/"
         "Vanilla: atykhonov/google-translate")
   :commands (go-translate
              go-translate-popup
              go-translate-popup-current
              go-translate-kill-ring-save
              go-translate-tts-play-current)
   :init
   (setq go-translate-base-url "http://translate.google.cn")
   (setq go-translate-local-language "zh-CN")
   (setq go-translate-target-language "en")
   (setq go-translate-extra-directions '(("zh-CN" . "ja") ("zh-CN" . "ko"))))

(x gnus
   :init
   (setq gnus-select-method  ; Mail
         `(nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port ,(if IS-WIN 993 "imaps"))
                  (nnimap-stream ssl))

         gnus-secondary-select-methods  ; Usenet
         '((nntp "nntp.aioe.org")
           ;; (nntp "news.eternal-september.org")
           ;; (nntp "freenews.netfront.net")
           ))

   (setq smtpmail-smtp-server "smtp.gmail.com"
         smtpmail-smtp-service 465
         smtpmail-stream-type  'ssl
         gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

   :config
   (setq gnus-novice-user nil)
   (setq gnus-expert-user t)
   (setq gnus-show-threads t)
   (setq gnus-interactive-exit t)
   (setq gnus-use-dribble-file nil)
   (setq gnus-always-read-dribble-file nil)
   (setq gnus-asynchronous t)
   (setq gnus-large-newsgroup 200)
   (setq mm-inline-large-images t)
   (setq send-mail-function 'smtpmail-send-it)
   (setq gnus-fetch-old-headers 'some)

   (setq gnus-thread-sort-functions
         '(gnus-thread-sort-by-most-recent-date
           (not gnus-thread-sort-by-number)))

   (setq gnus-summary-same-subject ""
         gnus-sum-thread-tree-indent "  "
         gnus-sum-thread-tree-single-indent "◎ "
         gnus-sum-thread-tree-root "● "
         gnus-sum-thread-tree-false-root "☆"
         gnus-sum-thread-tree-vertical "│"
         gnus-sum-thread-tree-leaf-with-other "├─► "
         gnus-sum-thread-tree-single-leaf "╰─► "
         gnus-summary-gather-subject-limit 'fuzzy)

   (setq gnus-topic-line-format "%4{[ %n -- %A ]%v%}\n"
         gnus-group-line-format "%{%M%S%p%} %0{%1y%} %P%4{%G%}\n"
         gnus-summary-line-format "%U%R%z%O %{%-5&user-date;%} %{%ua%} %B %(%I%-50,50s%)\n")

   (defun gnus-user-format-function-a (header)
     (let ((myself (concat "lorniu"))
           (references (mail-header-references header))
           (message-id (mail-header-id header)))
       (if (or (and (stringp references) (string-match-p myself references))
               (and (stringp message-id) (string-match-p myself message-id)))
           "X" "│")))

   (setq gnus-visible-headers
         (mapconcat 'regexp-quote
                    '("From:" "Newsgroups:" "Subject:" "Date:"
                      "Organization:" "To:" "Cc:"
                      "Followup-To" "Gnus-Warnings:"
                      "X-Sent:" "X-URL:" "User-Agent:"
                      "X-Newsreader:"
                      "X-Mailer:" "Reply-To:" "X-Spam:"
                      "X-Spam-Status:" "X-Now-Playing"
                      "X-Attachments" "X-Diagnostic")
                    "\\|"))

   (setq gnus-message-archive-group
         '((if (message-news-p) "miscellaneous-News" "miscellaneous-Gmail")))

   (setq gnus-posting-styles
         '((".*"
            (name "lorxiu")
            (address "loxnix@gmail.com")
            (signature "Life is busy.\n"))
           ((message-mail-p)
            (name "lorniu")
            (address "lorniu@gmail.com"))))

   (setq gnus-default-charset 'utf-8
         gnus-group-charset-alist
         '(("newsgroup\\.la" big5)
           ("\\(^\\|:\\)hk\\|\\(^\\|:\\)tw" big5)
           ("\\(^\\|:\\)cn" gbk))
         gnus-summary-show-article-charset-alist
         '((1 . gb2312)
           (2 . gb18030)
           (3 . gbk)
           (4 . big5)
           (5 . utf-8))
         gnus-group-name-charset-group-alist
         '(("newsgroup\\.la" . big5)
           ("\\.com\\.cn:" . gbk)
           ("news\\.newsfan\\.net" . gbk))
         gnus-newsgroup-ignored-charsets
         '(unknown-8bit
           x-unknown iso-8859-1
           unknown-8bit x-unknown gb18030))

   (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
   (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
   (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(x rcirc
   :bind (:map rcirc-mode-map ("C-c SPC" . rcirc-next-active-buffer))
   :init
   (defun rcirc-reconnect (host)
     (interactive (list (completing-read "Reconnect the Host: " rcirc-server-alist)))
     (let (cs rcirc-server-alist)
       (dolist (buf (buffer-list) (princ cs))
         (with-current-buffer buf
           (if (and (eq major-mode 'rcirc-mode)
                    (or (null (rcirc-buffer-process))
                        (string= host (process-contact (rcirc-buffer-process) :name))))
               (if (string-match-p "^#.+$" (buffer-name))
                   (push (buffer-name) cs)))))
       (ignore-errors (delete-process (get-process host)))
       (setq rcirc-server-alist `((,host :channels ,cs)))
       (rcirc nil)))

   :config

   (require 'rcirc-styles)

   (setq rcirc-default-user-name "loofee"
         rcirc-default-nick      "loofee"
         rcirc-server-alist      '(("irc.freenode.net" :channels ("#lisp" "#linuxba")))
         rcirc-authinfo          '(("freenode" nickserv "loofee" "ooo"))
         rcirc-align-width       10
         rcirc-prompt            " %t << "
         rcirc-always-use-server-buffer-flag t
         rcirc-fill-column       'frame-width
         rcirc-fill-prefix        (make-string (+ 7 rcirc-align-width) ? )
         rcirc-keywords          '("money" "lisp")
         rcirc-omit-responses    '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

   (add-hook-fun rcirc-mode-hook ()
     (rcirc-track-minor-mode 1)
     (unless rcirc-omit-mode
       (rcirc-omit-mode)
       (delight 'rcirc-omit-mode ".")))

   (defun rcirc-generate-new-buffer-name (process target)
     (substring-no-properties
      (or target (concat "[" (process-name process) "]"))))

   (defun rcirc-short-buffer-name (buffer)
     (let ((bn (buffer-name buffer)) sn)
       (cond ((string-match "^\\[irc\\.\\([^ .]+\\)" bn)
              (setq sn (match-string 1 bn)))
             (t (with-current-buffer buffer
                  (setq sn (or rcirc-short-buffer-name (buffer-name))))))
       (substring sn 0 (min (length sn) 4))))

   ;; Align message according to nick.
   (add-to-list 'rcirc-markup-text-functions
                (lambda (__ _)
                  (goto-char (point-min))
                  (string-match "^..:.. \\(<[^>]+>\\|\\[[a-z]+ \\)" (buffer-string))
                  (let* ((nick (match-string 1 (buffer-string)))
                         (nick* (format (format "%%%ds" rcirc-align-width) nick)))
                    (while (and nick (search-forward nick nil t))
                      (replace-match nick* nil t)))))

   ;; inhibit showing user-list when join
   (defvar rcirc-hide-userlist t)

   (defun my-advice-not-in-hidelist (old-fun &rest args)
     (if (not rcirc-hide-userlist)
         (apply old-fun args)))

   (advice-add 'rcirc-handler-353 :around 'my-advice-not-in-hidelist)
   (advice-add 'rcirc-handler-366 :around 'my-advice-not-in-hidelist)
   (defun:before rcirc-handler-JOIN$ (&rest _) (setq rcirc-hide-userlist t))
   (defun:before rcirc-cmd-names$ (&rest _) (setq rcirc-hide-userlist nil))

   (defun rcirc-handler-301 (&rest _) "/away message handler.")
   (defun rcirc-handler-372 (&rest _) "/welcome handler."))


;;; Utils

(x markdown-mode
   :ref "https://jblevins.org/projects/markdown-mode/"
   :init
   (add-hook-fun markdown-mode-hook ()
     (outline-hide-sublevels 1)))

(x all-the-icons
   "M-x all-the-icons-install-fonts."
   :ref "domtronn/all-the-icons.el")

(x git-auto-commit
   :commands (im/git-commit im/git-commit-and-push))

(x magit
   :ref "magit/magit"
   :if (executable-find "git")
   :bind ("C-c m" . magit-status)
   :init
   (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
   (modify-coding-system-alist 'process "git" 'utf-8)

   :config
   (transient-append-suffix 'magit-log "a" '("w" "Wip" magit-wip-log-current))
   (transient-append-suffix 'magit-log "-A" '("-m" "Omit merge commits" "--no-merges"))

   (magit-auto-revert-mode -1))

(x emms
   "Play music in emacs.
   "
   "First, install a player:
   "
   ": brew install mpg123/mplayer/vls...
   "
   :commands emms
   :init
   (setq emms-show-formatoo "Playing: %s")
   (setq emms-source-file-default-directory "~/music/")
   (setq emms-directory (locc "emms"))
   (make-directory emms-directory t)
   :config
   (require 'emms-setup)
   (emms-all)
   (emms-default-players))

(x elfeed-org
   "Read rss feeds."
   :init
   (setq elfeed-db-directory (locc "elfeed"))
   (setq elfeed-log-buffer-name " *elfeed-log*")
   (autoload 'elfeed "elfeed-org" nil t)

   (defun elfeed-proxy (&optional proxy)
     (interactive (let ((proxies '("no-proxy" "socks5://127.0.0.1:1080")))
                    (list (completing-read "Proxy:" proxies
                                           nil nil nil nil
                                           (if elfeed-curl-extra-arguments (car proxies) (cadr proxies))))))
     (if (or (zerop (length proxy)) (string-equal proxy "no-proxy"))
         (progn (setq elfeed-curl-extra-arguments nil)
                (message "Elfeed proxy disabled."))
       (setq elfeed-curl-extra-arguments (list "--proxy" proxy))
       (message "Elfeed proxy: %s" proxy)))

   :config
   (setq rmh-elfeed-org-files (list ic/feeds-org-file))
   (elfeed-org)

   (defun my-elfeed-show-mode-hook ()
     (pixel-scroll-mode t)
     (setq pixel-resolution-fine-flag t
           mouse-wheel-scroll-amount '(1)
           fast-but-imprecise-scrolling t
           jit-lock-defer-time 0
           mouse-wheel-progressive-speed nil)
     (setq line-spacing 3))

   (add-hook 'elfeed-search-update-hook 'f/face-font-buffer-local)
   (add-hook 'elfeed-show-mode-hook 'my-elfeed-show-mode-hook))

(x gimp
   :commands (connect-gimp gimp-mode))

(x alert
   :commands (alert)
   :config
   (setq alert-default-fade-time 8)
   (setq alert-default-style (cond ((executable-find "notify-send") 'libnotify)
                                   ((executable-find "powershell") 'powershell)
                                   ((executable-find "growlnotify") 'growl)
                                   ((executable-find "terminal-notifier") 'notifier))))

(x keypression/d-
   "Some problems exist, but it's interesting."
   :ref "chuntaro/emacs-keypression"
   :init
   (setq keypression-cast-command-name nil)
   (setq keypression-font-face-attribute '(:height 120 :width normal)))


;;; Multiple Modes

;; MMM-Mode
;; Poly-Mode
;; edit-indirect

(x edit-indirect/+
   :commands (edit-indirect-region)
   :ref "Fanael/edit-indirect")


(provide 'immor)

;;; immor.el ends here
