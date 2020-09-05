;;; imods.el --- Modules Configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;; Builtins

(x desktop
   :init
   (setq desktop-path `(,(locc) ".")))

(x winner
   :init (winner-mode 1)
   :config
   (define-key winner-mode-map [C-left] 'winner-undo)
   (define-key winner-mode-map [C-right] 'winner-redo))

(x electric
   :init
   (defun my-inhibit-electric-pair (c)
     (or
      (char-equal c ?\<) ; don't complete <
      (and (equal major-mode 'org-mode) ; only src-block in org
           (if (org-in-src-block-p)
               (not (member (car (org-babel-get-src-block-info)) '("csharp" "cs" "php")))
             t))
      (electric-pair-conservative-inhibit c)))
   (setq electric-pair-inhibit-predicate 'my-inhibit-electric-pair))

(x paren
   :init
   (show-paren-mode +1))

(x autorevert
   :init
   (setq auto-revert-mode-text "")
   (global-auto-revert-mode 1))

(x savehist
   :init
   (savehist-mode +1))

(x recentf
   :init
   (setq recentf-max-saved-items 200)
   (setq recentf-exclude '(package-user-dir
                           (expand-file-name package-user-dir)
                           "-autoloads.el$"
                           ".cache"
                           ".cask"
                           "bookmarks"
                           "cache"
                           "ido.*"
                           "persp-confs"
                           "recentf"
                           "undo-tree-hist"
                           "url"
                           "COMMIT_EDITMSG\\'"))

   (defun im/recentf-clear (&optional regexp)
     (interactive (list (read-string "Regexp for flushing from recentf: ")))
     (if (zerop (length regexp))
         (setq recentf-list nil)
       (cl-delete-if (lambda (f) (string-match-p regexp f)) recentf-list))
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
             (defun my-occur (str)
               (interactive (list (aif (symbol-at-point) (symbol-name it))))
               (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))
   (define-key occur-mode-map "r" 'occur-edit-mode)
   (add-function :after (symbol-function 'occur) ; After Occur, jump to the result buffer.
                 (lambda (&rest _) (aif (get-buffer-window "*Occur*") (select-window it)))))

(x ediff
   :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))

(x so-long
   :init
   (when (fboundp 'global-so-long-mode)
     (global-so-long-mode)))

(x abbrev/d
   :config
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file)))

(x dabbrev
   "What M-/ bound."
   :init
   (setq dabbrev-case-fold-search 'case-fold-search)
   (setq dabbrev-case-replace nil))

(x hippie-exp
   :init
   (setq hippie-expand-try-functions-list
         '(try-expand-dabbrev
           try-expand-dabbrev-visible
           try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
           try-complete-file-name-partially try-complete-file-name
           try-expand-all-abbrevs try-expand-list try-expand-line
           try-complete-lisp-symbol-partially try-complete-lisp-symbol))
   ;; (global-set-key (kbd "M-/") #'hippie-expand)
   )

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

   (defun-hook ibuffer-mode-hook ()
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
         ( "z" . idp/dired-du-size )
         ( "Y" . idp/dired-rsync )
         ( "," . dired-collapse-mode )
         ( "s" . imtt/transient-dired-sort )
         ( "C-c m" . imtt/transient-dired ))
   :init
   (require 'wdired)
   (setq wgrep-enable-key "r")

   (require 'dired-collapse)
   (defun-hook dired-mode-hook ()
     (dired-collapse-mode 1))

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

   (defun:before ls-lisp-format$shorten (_fn file-attr _fs _sw _ti)
     (when ls-lisp-xid-shorten-threshold
       (cl-labels ((norm (file n &optional
                               (threshold ls-lisp-xid-shorten-threshold)
                               (len (- ls-lisp-xid-shorten-threshold 3)))
                         (let ((item (nth n file)))
                           (when (and (stringp item) (> (length item) threshold))
                             (setf (nth n file)
                                   (propertize (format "%s~" (upcase (cl-subseq item 0 len)))
                                               'help-echo item))))))
         (norm file-attr 2)
         (norm file-attr 3))))

   ;; others
   (setq dired-dwim-target t)
   (require 'im-dired-plus))

(x vc
   :init
   (setq vc-handled-backends '(SVN Git Hg)))

(x view
   :bind
   (:map view-mode-map
         ( "h"       .  backward-char )
         ( "l"       .  forward-char  )
         ( "j"       .  next-line     )
         ( "k"       .  previous-line )
         ( "%"       .  his-match-paren )
         ( "<DEL>"   .  nil )
         ( "C-x C-e" .  eval-last-sexp ))
   :init
   (defun-hook edebug-mode-hook ()
     (view-mode -1)))

(x flyspell/x
   :preface (setq ispell-program-name "ispell") ;; apt install ispell
   :if (executable-find ispell-program-name)
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (rcirc-mode . flyspell-mode))
   :config (ispell-change-dictionary "american" t))

(x image-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))
   :config
   (define-key image-mode-map "c" 'im/yank-current-buffer-name))

(x hexl
   :bind (:map
          hexl-mode-map
          ("C-c m" . imtt/transient-hexl-mode)))

(x calc
   :init
   ;; Take region content as initial input of `quick-calc'.
   (defvar quick-calc-init-string nil)
   (defun:around calc-do-quick-calc$quick-calc-with-region (f &rest args)
     (let ((quick-calc-init-string (im/thing-at-region-or-point "")))
       (apply f args)))
   (defun:around calc-do-alg-entry$quick-calc-with-region (f &rest args)
     (when (and (string= (car args) "")
                (string= (cadr args) "Quick calc: "))
       (setf (car args) quick-calc-init-string))
     (apply f args))
   :ref "info: https://www.gnu.org/software/emacs/manual/html_node/calc/index.html")

(x which-func)


;;; Password and Auth

(x password-cache
   "Manage password in memory."
   :custom
   (password-cache-expiry nil))

(x auth-source-pass
   :custom
   (auth-source-pass-filename (or (getenv "PASSWORD_STORE_DIR") "~/.password-store"))
   :init
   (defun lookup-password (host user port)
     (require 'auth-source-pass)
     (let ((auth (auth-source-search :host host :user user :port port)))
       (if auth
           (let ((secretf (plist-get (car auth) :secret)))
             (if secretf
                 (funcall secretf)
               (error "Auth entry for %s@%s:%s has no secret!" user host port)))
         (error "No auth entry found for %s@%s:%s" user host port)))))


;;; Tramp/Shell

(progn
  (when (and IS-WIN ; Cygwin/Msys2/Git-for-Win + fakecygpty for better TERMINAL
             (executable-find "bash"))
    ;; If `Spawning child process: Exec format error` occurred, recompile or change permissions.
    (add-to-list 'exec-path (expand-file-name (loce "extra/fakecygpty/")))
    (require 'fakecygpty)
    ;; (add-to-list 'fakecygpty-ignored-program-regexps "httpd")
    (fakecygpty-activate))

  (defun im/popup-shell (&optional _)
    (interactive "P")
    (if (eq major-mode 'shell-mode)
        (bury-buffer)
      (let ((curr-dir default-directory))
        (shell "+shell+")
        (ring-insert comint-input-ring
                     (concat "cd " (shell-quote-argument curr-dir))))))

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

  (defun im/open-system-terminal-here ()
    (interactive)
    (if ic/system-terminal
        (let ((d (or default-directory "~/")))
          (start-process-shell-command (format "sys-terminal-%s" d) nil (format ic/system-terminal d))
          (message "OK."))
      (user-error "Please config `ic/system-terminal' first."))))

(x tramp/i
   "Try to use sshx in Windows if nessary.

    [BUG with FreeBSD, temp solution, it will be fixed after Emacs-27.2:]:

    ;; Disable echo expansion
+   (tramp-send-command vec \"set +E\" t)"
   :init
   (setq tramp-verbose 3)
   (setq tramp-persistency-file-name (locc "tramp"))
   (setq remote-file-name-inhibit-cache nil)
   (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

   (defun im/set-ssh-askpass ()
     "Maybe you should set path of askpass, for prompt password."
     (interactive)
     (unless IS-WIN (user-error "This is used for Windows System."))
     (let ((exe (read-file-name "askpass executable: "
                                (or (getenv "MSYS_HOME")
                                    (let ((bin (executable-find "bash")))
                                      (if bin
                                          (file-name-parent-directory
                                           (file-name-parent-directory
                                            (file-name-directory bin)))))
                                    "c:/")
                                nil t)))
       (if (and exe (file-exists-p exe))
           (progn (setenv "SSH_ASKPASS" exe)
                  (message "Set SSH_ASKPASS to %s." exe))
         (message "Error askpass file found, do nothing."))))

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

   (defun-hook term-mode-hook ()
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

   (defun-hook shell-mode-hook ()
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

   ;; Mode Hook
   (defun-hook eshell-mode-hook ()
     (setq-local company-idle-delay 1.2)
     ;; keys
     (with-eval-after-load 'em-hist
       (define-key ; disable local M-s
         (if (boundp 'eshell-hist-mode-map) eshell-hist-mode-map eshell-mode-map) ; v27 vs v28
         [(meta ?s)] nil))
     (define-key eshell-mode-map (kbd "C-r") 'eshell/h)
     (define-key eshell-mode-map (kbd "C-c M-o") (lambda () (interactive) (recenter-top-bottom 0))))

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
   (defun-hook eshell-before-prompt-hook ()
     (setq xterm-color-preserve-properties t)
     (setq eshell-preoutput-filter-functions '(xterm-color-filter))
     (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

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
                ("C-c f"   . im/hs-toggle-current)
                ("C-c l"   . im/hs-toggle-level)
                ("C-c F"   . im/hs-toggle-all))
   :init
   (defun hs-add-rule (mode start end comment &optional forward-sexp-func adjust-beg-func)
     (require 'hideshow)
     (setq hs-special-modes-alist
           (cl-remove-if (lambda (hm) (equal mode (car hm))) hs-special-modes-alist))
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


;;; Edit and Manage

(x project/e
   :config
   (cl-defmethod project-root ((project (eql nil))) nil)
   (cl-defmethod project-root ((project (head flg))) (cdr project))

   (defvar project-mode-line
     '(:eval (let* ((pname (project-root (project-current)))
                    (mname (if pname
                               (format (if (file-remote-p default-directory) "{%s}  " "<%s>  ")
                                       (file-name-nondirectory (if (string-suffix-p "/" pname)
                                                                   (subseq pname 0 (1- (length pname)))
                                                                 pname)))
                             " ")))
               (propertize mname 'face (if (facep 'project-mode-line-face)
                                           'project-mode-line-face
                                         'font-lock-comment-face)))))

   (defun my-project-try-flg-file (dir)
     (flet ((files-match-p (regexp d) (cl-find-if (lambda (f) (string-match-p regexp f)) (directory-files d))))
       (macrolet ((gor (&rest rest) `(let ((f (locate-dominating-file dir (lambda (d) (or ,@rest))))) (if f (cons 'flg f)))))
         (gor
          (file-exists-p (expand-file-name ".project" d))
          (file-exists-p (expand-file-name ".projectile" d))
          (files-match-p "\\.csproj$" d)))))
   (add-to-list 'project-find-functions #'my-project-try-flg-file t)

   (defun my-project-directories(project)
     (delete-dups
      (delq nil
            (mapcar (lambda (f)
                      (subseq (file-name-directory f)
                              (length (expand-file-name (project-root project)))))
                    (project-files project)))))
   (defun im/project-find-dir()
     (interactive)
     (let* ((project (or (project-current)
                         (user-error "No project detected.")))
            (root (project-root project))
            (dirs (my-project-directories project))
            (dir (completing-read (format "Directories (%s): " root) dirs)))
       (dired (expand-file-name dir root)))))

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

(x vlf
   "Open huge file, Part by Part."
   :ref "m00natic/vlfi"
   :init
   (require 'vlf-setup)

   (add-to-list 'vlf-forbidden-modes-list 'doc-view-mode-maybe)
   (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))

(x ztree
   "Use `ztree-diff' to diff directories."
   :ref "fourier/ztree"
   :config
   (setq ztree-draw-unicode-lines t)
   (setq ztree-diff-additional-options nil) ; '("-w" "-i")
   (add-to-list 'ztree-diff-filter-list "^~"))

(x ahs
   "- Remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01"
   "- remove `iedit', as `auto-highlight-symbol' is amazing. Hack it and enjoy it! 2018-07-03"
   :bind (:map ahs-mode-map
               ("M-p" . ahs-backward)
               ("M-n" . ahs-forward)
               ("M-r" . ahs-change-range))
   :init
   (setq ahs-idle-interval 0.3
         ahs-case-fold-search nil
         ahs-exclude '(( ruby-mode . "\\_<\\(end\\)\\_>" )
                       ( prog-mode . "\\_<\\(t\\|nil\\)\\_>" )))
   (require 'ahs)
   (global-ahs-mode 1))

(x narrow-indirect
   "Utils of indirect buffer (C-x 4 c)"
   :commands (ni-narrow-to-defun-indirect-other-window
              ni-narrow-to-region-indirect-other-window
              ni-narrow-to-page-indirect-other-window)
   :ref "https://www.emacswiki.org/emacs/NarrowIndirect")

(x expand-region/i
   :ref "magnars/expand-region.el"
   :init
   (setq expand-region-contract-fast-key ".")
   (setq expand-region-show-usage-message nil)
   :commands (er/expand-region))

(x iy-go-to-char
   :commands (iy-go-to-char
              iy-go-to-char-backward))


;;; Network

(x eww
   :init
   (setq eww-bookmarks-directory (locc "./"))
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
   :config
   (setq youdao-dictionary-search-history-file (locc "youdao"))
   (defun:override youdao-dictionary--play-voice$ (word)
     (let ((player (or (executable-find "mpv")
                       (executable-find "mplayer")
                       (executable-find "mpg123"))))
       (if player
           (start-process player nil player (youdao-dictionary--format-voice-url word))
         (if (executable-find "powershell")
             (powershell-execute (format "$w = New-Object -ComObject SAPI.SpVoice; $w.speak(\\\"%s\\\")" word))
           (user-error "mplayer/mpg123 or powershell is needed to play word voice")))))
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
   (defvar go-translate-base-url "https://translate.google.cn")
   (defvar go-translate-local-language "zh-CN")
   (defvar go-translate-target-language "en")
   (defvar go-translate-extra-directions '(("zh-CN" . "ja") ("zh-CN" . "ko")))
   (defvar go-translate-buffer-follow-p t)

   (defun go-translate-echo-area-and-kill-ring-save ()
     (interactive)
     (let ((go-translate-prepare-function #'ignore)
           (go-translate-render-function
            (lambda (_req resp)
              (deactivate-mark)
              (kill-new (go-translate-result--translation resp))
              (message "%s" (go-translate-result--translation resp)))))
       (call-interactively #'go-translate)))

   :config
   (require 'facemenu))


;;; Enhanced

(x page-break-lines
   :init
   (setq page-break-lines-lighter "")
   (global-page-break-lines-mode 1)
   :config
   (nconc page-break-lines-modes '(web-mode css-mode haskell-mode)))

(x which-key/d
   :init
   (setq which-key-show-early-on-C-h t
         which-key-idle-delay 1.5
         which-key-idle-secondary-delay 0.05)
   (which-key-mode 1)

   :config
   (setq which-key-compute-remaps t)
   (setq which-key-allow-multiple-replacements t)
   (setq which-key-sort-order #'my/which-key-local-then-prefix-then-key-order)

   (which-key-add-key-based-replacements
     "C-x 8"   "unicode"
     "C-x a"   "custom-act"
     "C-x i"   "custom-goto"
     "C-x p"   "project"
     "C-x r"   "rect/register/bookmark"
     "C-x v"   "version-control")

   (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
   (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("߅" . nil)))
   (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⚔" . nil)))
   (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . (" " . nil)))
   (add-to-list 'which-key-replacement-alist '(("ESC" . nil) . ("ℇ" . nil)))

   (defun my/which-key-local-then-prefix-then-key-order (acons bcons)
     (let ((aloc? (which-key--local-binding-p acons))
           (bloc? (which-key--local-binding-p bcons)))
       (if (not (eq aloc? bloc?))
           (and aloc? (not bloc?))
         (let ((apref? (which-key--group-p (cdr acons)))
               (bpref? (which-key--group-p (cdr bcons))))
           (if (not (eq apref? bpref?))
               (and (not apref?) bpref?)
             (which-key-key-order-alpha acons bcons)))))))

(x transient/e
   :ref "magit/transient"
   :init
   ;; Imenu Support
   (with-eval-after-load 'lisp-mode
     (add-to-list 'lisp-imenu-generic-expression
                  (list "Functions"
                        "^\\s-*(\\(transient-define-\\(?:\\(?:in\\|pre\\|suf\\)fix\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
                        2)))

   ;; Common Commands
   (defvar transient-show-common-commands-p t)
   (setq transient--common-command-prefixes (list (kbd "<f1>")))
   (defun:override transient--init-suffixes$ (name)
     (let ((levels (alist-get name transient-levels)))
       (cl-mapcan (lambda (c) (transient--init-child levels c))
                  (append (get name 'transient--layout)
                          (and (not transient--editp)
                               transient-show-common-commands-p
                               (get 'transient-common-commands
                                    'transient--layout))))))

   ;; Display Cases
   (defun:around transient--redisplay$short (f &rest args)
     (cond
      ((string-match-p "other-window" (format "%s" this-command))
       (message "C-x o-"))
      (t (apply f args))))

   ;;Helper
   (defmacro transient-my-bind-service (mode)
     "Bind transient to `C-c m' for MODE."
     (let ((map (intern (concat (symbol-name mode) "-map")))
           (trans (intern (concat "imtt/transient-" (symbol-name mode)))))
       `(define-key ,map (kbd "C-c m") ',trans)))

   (defun transient-my-description (key desc)
     "Generate string used by `transient-define-pre'."
     (concat (propertize key 'face 'transient-key) " " desc)))

(x ace-window
   :bind ("C-x w" . ace-window)
   :config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(x alert
   :commands (alert)
   :config
   (setq alert-default-fade-time 8)
   (setq alert-default-style (cond ((executable-find "dunstify") 'dunstify)
                                   ((executable-find "notify-send") 'libnotify)
                                   ((executable-find "powershell") 'powershell)
                                   ((executable-find "growlnotify") 'growl)
                                   ((executable-find "terminal-notifier") 'notifier))))


;;; Processes

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

(x pdf-tools
   :ref "vedang/pdf-tools"
   :init
   (defun-hook find-file-hook/pdf-tools ()
     (and (string-equal (file-name-extension buffer-file-name) "pdf")
          (not (featurep 'pdf-tools))
          (require 'pdf-tools)))
   :config
   ;; fire when epdfinfo exists
   (when (executable-find pdf-info-epdfinfo-program)
     (add-to-list 'magic-mode-alist (cons "%PDF" 'pdf-view-mode))
     (pdf-tools-install :no-query))
   ;; variable
   (setq-default pdf-view-display-size 'fit-page)
   (setq pdf-annot-activate-created-annotations t)
   (setq pdf-view-resize-factor 1.1)
   ;; keymap
   (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
   (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
   (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
   (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
   (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
   (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save))

(x git-auto-commit
   :commands (im/git-commit im/git-commit-and-push))

(x magit
   :ref "magit/magit"
   :if (executable-find "git")
   :init
   (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
   (modify-coding-system-alist 'process "git" 'utf-8)

   :config
   (transient-append-suffix 'magit-log "a" '("w" "Wip" magit-wip-log-current))
   (transient-append-suffix 'magit-log "-A" '("-m" "Omit merge commits" "--no-merges"))

   (magit-auto-revert-mode -1))

(x ssh-agency
   :if IS-WIN
   :after magit)

(x gimp
   :commands (connect-gimp gimp-mode))

(x mpv
   :commands (mpv-extra-options)
   :config
   (defun mpv-extra-options ()
     (interactive)
     (let ((s (read-from-minibuffer "MPV Options: "
                                    (mapconcat #'identity mpv-default-options " "))))
       (setq mpv-default-options
             (if (string-equal "" s) nil
               (split-string s " ")))
       (message "%S" mpv-default-options))))

(x emms
   "Play music in emacs.
   "
   "First, install a player:
   "
   ": brew install mpv/vls...
   "
   :commands emms
   :init
   (setq emms-show-formatoo "Playing: %s")
   (setq emms-source-file-default-directory "~/playing/")
   (setq emms-directory (locc "emms"))
   (make-directory emms-directory t)
   :config
   (require 'emms-setup)
   (emms-all)
   (emms-default-players)
   ;; keys
   (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
   (define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
   (define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
   (define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
   (define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
   (define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))
   (define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60))))


;;; Misc

(x pyim
   "default-input-method:
- chinese-py [builtin]
- pyim [pure elisp]
- rime [need librime support]"
   :ref "tumashu/pyim"
   :init
   (setq default-input-method "pyim")
   :config
   (when IS-G
     (setq pyim-page-tooltip 'posframe)
     (setq pyim-posframe-border-width 6))
   (pyim-basedict-enable))

(x markdown-mode
   :ref "https://jblevins.org/projects/markdown-mode/"
   :init
   (defun-hook markdown-mode-hook ()
     (outline-hide-sublevels 1)))

(x all-the-icons/i
   "M-x all-the-icons-install-fonts."
   :ref "domtronn/all-the-icons.el")

(x keypression/d-
   "Some problems exist, but it's interesting."
   :ref "chuntaro/emacs-keypression"
   :init
   (setq keypression-cast-command-name nil)
   (setq keypression-font-face-attribute '(:height 120 :width normal)))

(defun im/welcome-to-mode ()
  (interactive)
  (message
   "Current mode is %s, happy to service you."
   (propertize
    (format "%s" major-mode)
    'face 'font-lock-warning-face)))


(provide 'imods)

;;; imods.el ends here
