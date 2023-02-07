;;; cust.el --- Customization -*- lexical-binding: t -*-

;;; Code:

(defgroup imfine nil
  "Private custom variables"
  :group 'emacs
  :prefix "ic/")

(defcustom org-directory (im:the-file "~/.notes/" "~/org")
  "Main Directory for Agenda/Notes/Diary and others."
  :type 'directory)

(defcustom ic/srcdir "~/source/"
  "Source directory."
  :type 'string)

(defcustom ic/workdir "~/workdir/"
  "Personal work directory."
  :type 'string)

(defcustom ic/downloaddir (im:the-file (expand-file-name "Downloads/" (getenv (if IS-WIN "USERPROFILE" "HOME"))))
  "Directory used to download."
  :type 'string)

(defcustom ic/host "127.0.0.1"
  "My remote server. Ip or host."
  :type 'string)

(defcustom ic/host-user "vip"
  "Default username of remote server."
  :type 'string)

(defcustom ic/proxy nil
  "http://auth@localhost:1081 or sock://localhost:1080 style."
  :type 'string)

(defun im:host (&optional user host) (concat (or user ic/host-user) "@" (or host ic/host)))

(defcustom ic/external-paths (list (loce "bin/x") (loco "bin"))
  "Extend the PATH environment."
  :type '(repeat string))

(defcustom ic/external-load-path-roots (list (loco "share/emacs/"))
  "Extra load-path root."
  :type '(repeat string))

(defcustom ic/system-terminal (cond (IS-WIN "start cmd %s")
                                    ((executable-find "alacritty") "alacritty --working-directory %s -t floater"))
  "System terminal, %s for workding directory."
  :type 'string)

;; personal minor mode

(defvar imfine-mode-map (make-sparse-keymap))

(define-minor-mode imfine-mode "Personal mode" :init-value t :global t)


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

      comp-async-buffer-name " *Async-native-compile-log*")

(setq resize-mini-windows t
      enable-recursive-minibuffers t
      suggest-key-bindings nil)

(add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines nil)))

(setq column-number-mode 1
      fringes-outside-margins t)

(setq kill-do-not-save-duplicates t
      kill-ring-max 500
      mark-ring-max 20
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

(defalias #'linum-mode #'display-line-numbers-mode)

(setq shared-game-score-directory (locc "shared-game-score/"))


;;; Environments

(setenv "TZ" "est-8")
(setenv "LC_COLLATE" "C")
(setenv "LC_ALL" "en_US.UTF-8")

(when IS-WIN
  (when-let (realhome (and (null (getenv-internal "HOME")) (getenv "USERPROFILE")))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))


;;; Encoding/Sentence

;; (prefer-coding-system 'gb2312)
;; (prefer-coding-system 'cp936)
;; (prefer-coding-system 'utf-16)

;; (if IS-WIN
;;     (progn
;;       ;; generic encoding
;;       (set-locale-environment "zh_CN.GBK")
;;       (prefer-coding-system 'utf-8)
;;       ;; process global encoding
;;       (setq process-coding-system-alist '(("what?" utf-8 . utf-8))))
;;   (set-locale-environment "utf-8")
;;   (prefer-coding-system 'utf-8-unix))

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

(unless (and (boundp 'custom-file) custom-file)
  (makunbound 'custom-file)) ; Remove it when not config before, then defvar below will work.

(defvar custom-file
  (let ((n (format "init-%s@%s.el"
                   (string-join (reverse (split-string (downcase (system-name)) "\\.")) ".")
                   (user-real-login-name)))
        (d (loco "share/emacs/inits/")))
    (if (file-exists-p d) (expand-file-name n d) (loce n)))
  "Change this in 'init-aux.el' if you want.")

(unless (file-exists-p custom-file)
  (with-temp-file custom-file (insert (format ";; %s" (system-name)))))

(condition-case _err (load custom-file t t))


;;; Global List

(setq auto-mode-alist
      (append '(("\\.class\\'"           . class-mode)
                ("\\.scm\\'"             . scheme-mode)
                ("\\.\\(ba\\)?sh\\'"     . sh-mode)
                ("\\.xaml\\'"            . nxml-mode)
                ("\\.\\(ini\\|inf\\|nmconnection\\)\\'" . conf-mode))
              auto-mode-alist))

(setq jka-compr-compression-info-list
      (prog1 ;; for .rar, you should install `unarchiver'
          (append `(["\\.plist$"
                     "converting text XML to binary plist" "plutil" ("-convert" "binary1" "-o" "-" "-")
                     "converting binary plist to text XML" "plutil" ("-convert" "xml1" "-o" "-" "-")
                     nil nil "bplist"])
                  jka-compr-compression-info-list)
        (jka-compr-update)))


;;; Global Hooks

(defvar ic/find-file-readonly-regexp nil)

(defun:hook find-file-hook/readonly ()
  "Files that should be readonly."
  (let* ((case-fold-search nil)
         (match? (lambda (&rest items) (string-match-p (apply #'im:join-as-regor-group items) buffer-file-name))))
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


;;; Proxy ⏚

(defun im/proxy (&optional url)
  (interactive (list (completing-read "Proxy to enable: "
                                      (delq nil (list
                                                 ic/proxy
                                                 "sock://127.0.0.1:1080"
                                                 "http://127.0.0.1:1081"
                                                 "sock://10.1.1.1:1080"
                                                 "http://10.1.1.1:1081"
                                                 (if ic/host (format "http://%s:11181" ic/host)))))))
  (cond ((= (length url) 0)
         (setq url-gateway-method 'native
               url-proxy-services nil
               url-http-proxy-basic-auth-storage nil
               socks-server nil
               ic/proxy nil)
         (message "Proxy canceled."))
        ((string-match "^\\(http\\|sock\\)s?://\\(?:\\(.*\\)@\\)?\\(.+\\):\\([0-9]+\\)$" url)
         (let* ((auth (match-string 2 url))
                (host (match-string 3 url))
                (port (match-string 4 url))
                (host+port (concat host ":" port)))
           (pcase (match-string 1 url)
             ("http"
              (setq url-gateway-method 'native
                    url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp)
                                         ("http" . ,host+port)
                                         ("https" . ,host+port))
                    url-http-proxy-basic-auth-storage (if auth (list (list host+port (cons "Proxy" (base64-encode-string auth)))))
                    socks-server nil
                    ic/proxy url)
              (message (concat "Proxy "
                               (propertize (concat "http://" (if auth (concat (car (split-string auth ":")) "@")) host+port) 'face 'font-lock-string-face)
                               " enabled.")))
             ("sock"
              (setq url-gateway-method 'socks
                    url-proxy-services nil
                    url-http-proxy-basic-auth-storage nil
                    socks-server (list "Default server" host (string-to-number port) 5)
                    ic/proxy url)
              (message (concat "Proxy " (propertize (concat "socks://" host+port) 'face 'font-lock-string-face) " enabled."))))))
        (t (user-error "Proxy URL should like 'http://auth@127.0.0.1:1081' or 'sock://127.0.0.1:1080'"))))

(defvar ic/proxy-lighter "  ℘  ")

(defvar url-gateway-local-host-regexp
  (concat "^" (regexp-opt `("localhost" "127.0." "192.168." "10." ,ic/host))))

(defvar im:proxy-format
  '(:propertize ic/proxy-lighter
                local-map (keymap (mode-line keymap (mouse-1 . (lambda () (interactive) (im/proxy) (force-mode-line-update)))))
                face font-lock-comment-face
                mouse-face warning
                help-echo ic/proxy))

(if ic/proxy (im/proxy ic/proxy))


;;; Server Daemon

(when IS-G
  (condition-case err
      (progn (require 'server)
             (setq server-auth-dir (locc "server/"))
             (delete-file (concat server-auth-dir "server"))
             (unless (server-running-p) (server-start)))
    (error (message "Server starting error: %s" err))))


;;; Miscellaneous

(when IS-MAC
  ;;(setq ns-command-modifier 'meta)
  ;;(setq ns-alternate-modifier 'super)
  (setq ns-auto-hide-menu-bar nil))

(provide 'cust)

;;; cust.el ends here
