;;; cust.el --- Customization -*- lexical-binding: t -*-

;;; Code:

(setq user-full-name "imfine")
(setq user-mail-address "lorniu@gmail.com")


;;; User definitions

(defgroup imfine nil "Private custom variables"
  :group 'emacs :prefix "ic/")

(defcustom org-directory (let ((d "~/.notes")) (expand-file-name (if (file-exists-p d) d "~/org")))
  "Main Directory for GTD/Notes/Diary and others."
  :type 'string :group 'imfine)

(defcustom ic/up nil
  "Private Host"
  :type 'string)

(defcustom ic/external-paths (list (loce "bin/x") (loco "bin"))
  "Extend the PATH environment."
  :type '(repeat string))

(defcustom ic/external-load-path-tops (list (loco "share/emacs/"))
  "Extra load-path root."
  :type 'list)

(defcustom ic/package-repo (if (getenv "VPS") :origin)
  "Which elpa repo `package.el' will use."
  :type 'symbol)

(defcustom ic/find-file-readonly-regexp nil
  "Regexp to decide whether file should be readonly after opened."
  :type 'string)

(defcustom ic/system-terminal (cond (IS-WIN "start cmd %s")
                                    ((executable-find "alacritty") "alacritty --working-directory %s -t floater"))
  "System terminal, %s for workding directory."
  :type 'string)

(defcustom ic/shengciben (loco "000/words.org")
  "Shengciben"
  :type 'string)

(defvar im-keys-mode-map (make-sparse-keymap))
(define-minor-mode im-keys-mode "Global override map" :init-value t :global t)


;;; Better default

(setq inhibit-default-init t
      inhibit-startup-message t

      track-eol t
      custom-unlispify-tag-names nil

      visible-bell nil
      ring-bell-function 'ignore

      enable-local-eval t
      enable-local-variables :all

      disabled-command-function nil
      ad-redefinition-action 'accept

      undo-limit 10000000

      byte-compile-warnings '(cl-functions)
      comp-async-buffer-name " *Async-native-compile-log*")

(setq resize-mini-windows t
      enable-recursive-minibuffers t
      suggest-key-bindings nil)

(add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines nil)))

(setq column-number-mode 1
      fringes-outside-margins t)

(setq kill-do-not-save-duplicates t
      kill-ring-max 250
      mark-ring-max 10
      select-enable-clipboard t
      select-enable-primary nil)

(setq use-dialog-box nil
      woman-use-own-frame nil
      help-window-select t
      mouse-autoselect-window -0.05
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      man-notify-method 'pushy)

(setq confirm-kill-processes nil)

(setq-default tab-width 4
	          indent-tabs-mode nil
	          truncate-lines t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq request-storage-directory    (locc "request")
      pcache-directory             (locc "pacache/")
      shared-game-score-directory  (locc "shared-game-score/"))


;;; Data security

(setq create-lockfiles nil) ;; .#lock-file

(setq backup-inhibited nil  ;; version backup file~~
      backup-by-copying t
      version-control t kept-new-versions 4 kept-old-versions 2 delete-old-versions t
      backup-directory-alist `(("\\.cache\\|\\.git\\|\\.ssh")
                               ("." . ,(locc "backup-files/"))))

(setq auto-save-default t   ;; save #periodically# to avoid crashes
      auto-save-interval 300 auto-save-timeout 30
      auto-save-file-name-transforms `((".*" ,(locc "auto-save/") t))
      auto-save-list-file-prefix (locc "auto-save/"))

(setq delete-by-moving-to-trash t)


;;; Encoding

(set-locale-environment   "utf-8")
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)

(defun im/local-encoding (&optional encoding)
  "Reset local system encoding, default is CP936."
  (interactive)
  (let ((encoding (or encoding 'cp936-dos)))
    (when (called-interactively-p 'any)
      (setq encoding (read-coding-system "Choose charset: " 'utf-8)))
    (set-buffer-file-coding-system encoding)
    (ignore-errors
      (set-process-coding-system (get-buffer-process (current-buffer)) encoding encoding))
    (message "Changed local coding to %s." encoding)))

(when IS-WIN
  ;; generic encoding
  (set-language-environment "chinese-gbk")
  (prefer-coding-system 'utf-8)

  ;; process global encoding
  (setq process-coding-system-alist '(("what?" utf-8 . utf-8)))

  ;; specified encoding
  (setq file-name-coding-system 'cp936-dos)
  (set-terminal-coding-system 'cp936-dos))


;;; Syntax/Indent etc

(setq sentence-end-double-space nil)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table))) '( ?， ?。 ?！ ?； ?？ ?： ?/ ))


;;; Environments

(setenv "TZ" "PRC")
(setenv "LC_COLLATE" "C")
(setenv "LC_ALL" "en_US.UTF-8")

(when (and IS-WIN (null (getenv "HOME"))) (setenv "HOME" (getenv "USERPROFILE")))


;;; Exec-Path

(cond (IS-WIN
       (add-to-list 'ic/external-paths (loce "bin/w") t)))

(cl-loop for d in ic/external-paths
         if (file-exists-p d) do
         (setenv "PATH" (concat d (if IS-WIN ";" ":") (getenv "PATH")))
         (add-to-list 'exec-path d))


;;; Custom File

(setq custom-file
      (if (boundp 'custome-file-private) custome-file-private
        (let ((n (format "%s@%s.el"
                         (string-join (reverse (split-string (downcase (system-name)) "\\.")) ".")
                         (user-real-login-name)))
              (d (loco "share/emacs/inits/")))
          (if (file-exists-p d) (expand-file-name n d) (loce (concat "init-" n))))))

(unless (file-exists-p custom-file)
  (with-temp-file custom-file (insert (format ";; %s" (system-name)))))

(advice-add #'custom-set-faces :around
            (defun %make-custom-set-face-the-highest-period (f &rest args)
              (eval-after-load 'imover (apply f args))))

(condition-case _err
    (load custom-file t t)
  (error (warn "Error in: %s" custom-file)))

(provide 'cust)

;;; cust.el ends here
