;;; -*- lexical-binding: t -*-

;;; Code:

(defgroup imfine nil
  "Private custom variables"
  :group 'emacs
  :prefix "im.")

(defcustom org-directory (im:the-file "~/.notes/" "~/org")
  "Main Directory for Agenda/Notes/Diary and others."
  :type 'directory)

(defcustom im.srcdir "~/source/"
  "Source directory."
  :type 'string)

(defcustom im.workdir "~/workdir/"
  "Personal work directory."
  :type 'string)

(defcustom im.downdir (im:the-file (expand-file-name "Downloads/" (getenv (if IS-WIN "USERPROFILE" "HOME"))))
  "Directory used to download."
  :type 'string)

(defcustom im.host "127.0.0.1"
  "My remote server. Ip or host."
  :type 'string)

(defcustom im.host-user "vip"
  "Default username of remote server."
  :type 'string)

(defcustom im.backup-dir (locc "backup-files")
  "Directory used to backup files."
  :type 'file)

(defcustom im.auto-save-dir (locc "auto-save")
  "Directory used to auto save files."
  :type 'file)

(defun im:host (&optional user host) (concat (or user im.host-user) "@" (or host im.host)))

(defcustom im.external-paths (list (loco "bin"))
  "Extend the PATH environment."
  :type '(repeat string))

(defcustom im.external-load-path-roots (list (loco "share/emacs/"))
  "Extra load-path root."
  :type '(repeat string))

(defcustom im.system-terminal (cond (IS-WIN "start cmd %s")
                                    ((executable-find "alacritty") "alacritty --working-directory %s -t floater"))
  "System terminal, %s for workding directory."
  :type 'string)

;; personal minor mode

(defvar imfine-mode-map (make-sparse-keymap))

(define-minor-mode imfine-mode "Personal mode" :init-value t :global t)


;;; Better default

(setq-default tab-width 4
              tab-always-indent 'complete
              indent-tabs-mode nil

              fill-column 80

              truncate-lines t
              fringes-outside-margins t

              lexical-binding t)

(setq default-directory "~/"

      track-eol t
      custom-unlispify-tag-names nil

      visible-bell nil
      ring-bell-function 'ignore

      enable-local-eval t
      enable-local-variables :all

      disabled-command-function nil
      ad-redefinition-action 'accept
      read-extended-command-predicate #'command-completion-default-include-p

      kill-ring-max 500
      mark-ring-max 50
      undo-limit 3200000
      message-log-max 100000

      resize-mini-windows t
      enable-recursive-minibuffers t
      suggest-key-bindings nil

      use-short-answers t
      use-dialog-box nil
      column-number-mode t
      woman-use-own-frame nil
      help-window-select t
      mouse-autoselect-window -0.05
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      man-notify-method 'pushy
      confirm-kill-processes nil

      auto-mode-case-fold nil
      word-wrap-by-category t
      redisplay-skip-fontification-on-input t

      kill-do-not-save-duplicates t
      select-enable-clipboard t
      select-enable-primary nil
      shared-game-score-directory (locc "shared-game-score/"))

;; files

(setq trusted-content :all
      create-lockfiles nil ; .#lock-file
      delete-by-moving-to-trash t)

(setq vc-make-backup-files t  ; backup versioned files
      backup-inhibited nil    ; version backup file~~
      backup-by-copying t
      kept-new-versions 6 kept-old-versions 2 delete-old-versions t version-control t
      backup-directory-alist `(("." . ,im.backup-dir)))

(setq auto-save-default t ; save #periodically# to avoid crashes
      auto-save-interval 300
      auto-save-timeout 30
      auto-save-list-file-prefix (expand-file-name ".saves-" im.auto-save-dir))

;; Sentence

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table))) '( ?， ?。 ?！ ?； ?？ ?： ?/ ))


;;; Load-Path

(cl-loop with ds = (list (loce "core") (loce "extra") (loce "repo") (loce "site-lisp"))
         for root in (append im.external-load-path-roots ds)
         for dirs = (ignore-errors (directory-files root t))
         do (cl-loop for dir in dirs
                     if (and (not (equal (file-name-extension dir) "")) (file-directory-p dir))
                     do (add-to-list 'load-path dir)))


;;; Exec-Path

(cond (IS-WIN   (add-to-list 'im.external-paths (loce "bin/win")))
      (IS-MAC   (add-to-list 'im.external-paths (loce "bin"))
                (add-to-list 'im.external-paths (loce "bin/mac"))
                (add-to-list 'im.external-paths "/opt/homebrew/bin")
                (add-to-list 'im.external-paths "~/.local/bin/"))
      (IS-LINUX (add-to-list 'im.external-paths (loce "bin"))
                (add-to-list 'im.external-paths (loce "bin/nix"))))

(dolist (d im.external-paths exec-path)
  (when (file-exists-p d)
    (setenv "PATH" (concat d (if IS-WIN ";" ":") (getenv "PATH")))
    (add-to-list 'exec-path d)))


;;; Environments

(setenv "TZ" "est-8")
(setenv "LC_COLLATE" "C")
;; (setenv "LC_ALL" "en_US.UTF-8")

(when IS-WIN
  (when-let* ((realhome (and (null (getenv-internal "HOME")) (getenv "USERPROFILE"))))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))


;;; Global Hooks

(defvar im.find-file-readonly-regexp nil)

(defun:hook find-file-hook/readonly ()
  "Files that should be readonly."
  (let* ((case-fold-search nil)
         (match? (lambda (&rest items) (string-match-p (apply #'im:join-as-regor-group items) buffer-file-name))))
    (when (and buffer-file-name
               (or (and im.find-file-readonly-regexp
                        (string-match-p im.find-file-readonly-regexp buffer-file-name))
                   (and (not
                         (funcall match? ; exclude
                                  "/usr/home"
                                  "\\.cache/" "tmp/" "vvv/" "notes"
                                  "autoloads.el$" "loaddefs.el$"))
                        (funcall match? ; include
                                 "^/usr/"
                                 ".emacs.d/packages"
                                 ".roswell/lisp/quicklisp"))))
      (view-mode 1)
      (make-local-variable 'view-mode-map)
      (define-key view-mode-map "q" 'View-kill-and-leave))))

(defun:hook before-save-hook/cleanup ()
  (unless (cl-find major-mode '(org-mode))
    (delete-trailing-whitespace)))

(defun:hook after-save-hook/bytecompile ()
  (when nil
    (save-window-excursion
      (when (and (eq major-mode 'emacs-lisp-mode)
                 (string-match-p "^[a-z1-9]" (buffer-name))
                 (string-match-p "\\/\\.emacs\\.d\\/\\(core\\|extra\\)\\/" (buffer-file-name)))
        (byte-compile-file (buffer-file-name))))))


;;; Assist actions for current mode (C-c m / <f12>)

(defun im/local-assist ()
  "Run the command define by `im:local-assist' method."
  (interactive)
  (im:local-assist major-mode))

(cl-defmethod im:local-assist (_default)
  "The general assist actions for current major mode."
  (let ((bn (intern (format "im/assist-%s" (buffer-name)))))
    (if (commandp bn)
        (call-interactively bn)
      (let ((tn (intern (format "im/assist-%s" major-mode))))
        (if (commandp tn)
            (call-interactively tn)
          (message "This is %s, nice to meet you."
                   (propertize (format "%s" major-mode) 'face 'warning)))))))


;;; Daemon server

(when IS-G
  (condition-case err
      (progn (require 'server)
             (setq server-auth-dir (locc "server/"))
             (delete-file (concat server-auth-dir "server"))
             (unless (server-running-p) (server-start)))
    (error (message "Server starting error: %s" err))))


;;; OS / Others

(when IS-MAC
  ;;(setq ns-command-modifier 'meta)
  ;;(setq ns-alternate-modifier 'super)
  (setq ns-auto-hide-menu-bar nil))

(when NATIVECOMP
  (add-to-list 'native-comp-eln-load-path (locc "eln/"))
  (setq native-comp-bootstrap-deny-list (list ".*/subr--trampoline.*")))


;;; Dynamic config file

(defcustom im.custom-file-directory (loco "share/emacs/inits/")
  "Directory of custom-file."
  :group 'imfine
  :type 'file)

(defcustom im.custom-file-file-name
  (format "init-%s@%s.el"
          (string-join (reverse (split-string (downcase (system-name)) "\\.")) ".")
          (user-real-login-name))
  "File name of custom-file."
  :group 'imfine
  :type 'string)

(unless (and (boundp 'custom-file) custom-file)
  (makunbound 'custom-file)) ; Remove it when not config before, then defvar below will work.

(defvar custom-file
  (if (file-exists-p im.custom-file-directory)
      (expand-file-name im.custom-file-file-name im.custom-file-directory)
    (loce im.custom-file-file-name))
  "Change this in \\='init-aux.el' if you want.")

(unless (file-exists-p custom-file)
  (with-temp-file custom-file (insert (format ";; %s" (system-name)))))

(message "[CUST] %s" custom-file)
(load custom-file nil t nil 'must-suffix)
