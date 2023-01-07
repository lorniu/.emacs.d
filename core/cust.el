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

(setq default-directory "~/"

      inhibit-default-init t
      inhibit-startup-message t

      track-eol t
      custom-unlispify-tag-names nil

      visible-bell nil
      ring-bell-function 'ignore

      enable-local-eval t
      enable-local-variables :all

      disabled-command-function nil
      ad-redefinition-action 'accept
      read-extended-command-predicate #'command-completion-default-include-p

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


;;; Environments

(setenv "TZ" "est-8")
(setenv "LC_COLLATE" "C")
(setenv "LC_ALL" "en_US.UTF-8")

(when IS-WIN
  (when-let (realhome (and (null (getenv-internal "HOME"))
                           (getenv "USERPROFILE")))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))


;;; Encoding/Sentence

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

(setq sentence-end-double-space nil)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table))) '( ?， ?。 ?！ ?； ?？ ?： ?/ ))


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
  )


;;; Global List

(setq auto-mode-alist
      (append '(("\\.class\\'"           . class-mode)
                ("\\.scm\\'"             . scheme-mode)
                ("\\.\\(ba\\)?sh\\'"     . sh-mode)
                ("\\.xaml\\'"            . nxml-mode)
                ("\\.\\(ini\\|inf\\)\\'" . conf-mode))
              auto-mode-alist))

(setq jka-compr-compression-info-list
      (prog1 ;; for .rar, you should install `unarchiver'
          (append `(["\\.plist$"
                     "converting text XML to binary plist" "plutil" ("-convert" "binary1" "-o" "-" "-")
                     "converting binary plist to text XML" "plutil" ("-convert" "xml1" "-o" "-" "-")
                     nil nil "bplist"])
                  jka-compr-compression-info-list)
        (jka-compr-update)))

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

        ("\\*sly-macroexpansion\\*\\|\\*Pp Macroexpand Output\\*"
         (display-buffer-reuse-window %display-buffer-in-direction-or-below-selected)
         (direction . right))

        ("\\*Youdao Dictionary\\*\\|\\*Help\\*\\|\\*Shell Command Output\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (reusable-frames . t))

        ("\\*\\(e?shell[-+]?\\|PowerShell\\|Python\\)\\*\\|[-+]\\(shell\\)[-+]"
         (display-buffer-same-window)
         (reusable-frames . t))

        ("\\*Async Shell Command\\*"
         (%display-buffer-at-bottom-follows-with-quit)
         (window-height . 0.3))

        ("\\*org-roam\\*"
         (display-buffer-in-direction)
         (direction . right)
         (window-width . 0.33)
         (window-height . fit-window-to-buffer))

        ("." nil (reusable-frames . t))))

(defmacro im/make-fun--display-my-buffer-in-direction-or- (other)
  (let ((fname (intern (format "%%display-buffer-in-direction-or-%s" other)))
        (dname (intern (format "display-buffer-%s" other))))
    `(defun ,fname (buffer alist)
       (if (and (> (frame-width) 120)
                (null (window-in-direction 'right))
                (null (window-in-direction 'left)))
           (display-buffer-in-direction buffer alist)
         (,dname buffer alist)))))

(im/make-fun--display-my-buffer-in-direction-or- below-selected)
(im/make-fun--display-my-buffer-in-direction-or- at-bottom)

(defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
  (display-buffer-at-bottom buffer alist)
  (select-window (get-buffer-window buffer))
  (with-current-buffer buffer (view-mode 1)))


;;; Global Hooks

(defun:hook find-file-hook/readonly ()
  "Files that should be readonly."
  (let* ((case-fold-search nil)
         (match? (lambda (&rest items) (string-match-p (apply #'join-as-regor-group items) buffer-file-name))))
    (when (and buffer-file-name
               (or (and ic/find-file-readonly-regexp
                        (string-match-p ic/find-file-readonly-regexp buffer-file-name))
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


;;; Global Proxy

(defvar ic/proxy-type nil "nil, :http or :sock")
(defvar ic/proxy-http '("127.0.0.1:1081" nil nil))
(defvar ic/proxy-sock '("Default server" "127.0.0.1" 1080 5))

(setq url-gateway-local-host-regexp
      (concat "^" (regexp-opt '("localhost" "127.0.0.1" "192.168." "10."))))

(defun im/proxy (&optional type)
  (interactive (list (intern (completing-read "type: " '(disable :sock :http) nil t))))
  (pcase type
    (:http (pcase-let ((`(,url ,user ,password) ic/proxy-http))
             (setq url-gateway-method 'native
                   url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp) ("http" . ,url) ("https" . ,url))
                   url-http-proxy-basic-auth-storage (if user `((,url (,user . ,password))))
                   socks-server nil)
             (message "Http proxy %s enabled." url)))
    (:sock (setq url-gateway-method 'socks
                 url-proxy-services nil
                 socks-server ic/proxy-sock)
           (message "Sock proxy %s enabled." ic/proxy-sock))
    (_ (setq url-gateway-method 'native
             url-proxy-services nil
             socks-server nil)
       (message "Proxy canceled."))))

(if ic/proxy-type (im/proxy ic/proxy-type)) ; initial proxy


;;; Server Daemon

(when IS-G
  (condition-case err
      (progn (require 'server)
             (setq server-auth-dir (locc "server/"))
             (delete-file (concat server-auth-dir "server"))
             (unless (server-running-p) (server-start)))
    (error (message "Server starting error: %s" err))))

(provide 'cust)

;;; cust.el ends here
