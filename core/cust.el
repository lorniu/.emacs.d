;;; cust.el --- Basic Configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Customization

(defgroup imfine nil "Private custom variables"
  :group 'emacs :prefix "ic/")

(defcustom org-directory (let ((d "~/.notes")) (expand-file-name (if (file-exists-p d) d "~/org")))
  "Main Directory for GTD/Notes/Diary and others."
  :type 'string :group 'imfine)

(defcustom ic/up nil
  "Private Host"
  :type 'string)

(defcustom ic/cache-dir "~/.cache/emacs/"
  "Cache Directory"
  :type 'string)

(defcustom ic/external-paths (list (loce "bin/x") (loco "x.bin"))
  "Extend the PATH environment."
  :type '(repeat string))

(defcustom ic/external-load-path-tops (list (loco "x.share/emacs/"))
  "Extra load-path root."
  :type 'list)

(defcustom ic/elpa-repo-type (if (getenv "VPS") :origin)
  "Which elpa repo `package.el' will use."
  :type 'symbol)

(defcustom ic/package-load-alist nil
  "Packages to disable or redirect: (('xxx' . nil|version|directory))"
  :type 'list)

(defcustom ic/faces nil
  "(:title a :font (cons b 120) :theme c)"
  :type 'list)

(defcustom ic/find-file-readonly-regexp nil
  "Regexp to decide whether file should be readonly after opened."
  :type 'string)

;;
(defcustom ic/my-postgres (list "postgres" "root")
  "Postgres connection info, (db user [password] [host] [port]) format."
  :type 'list)

(defcustom ic/lisp-init-file (loce "share/lisp/sly.lisp")
  "Common Lisp init file."
  :type 'file)

;;
(defcustom ic/proxy-type nil
  "Which proxy to use, :http or :sock."
  :type 'keyword)

(defcustom ic/proxy-sock '("Default server" "127.0.0.1" 1080 5)
  "Socket proxy default value."
  :type 'list)

(defcustom ic/proxy-http '("127.0.0.1:1081" nil nil)
  "Http proxy default value: (url user password)"
  :type 'string )

(defcustom ic/system-terminal (if IS-WIN "start cmd %s"
                                "alacritty --working-directory %s -t floater")
  "System terminal, %s for workding directory."
  :type 'string)

;;
(defcustom ic/shengciben (loco "000/words.org")
  "Shengciben"
  :type 'string)


;;; Variables

(setq user-full-name "imfine"
      user-mail-address "lorniu@gmail.com"

      inhibit-startup-message t
      gnus-inhibit-startup-message t
      auto-mode-case-fold nil
      enable-local-eval t
      enable-local-variables :all

      byte-compile-warnings '(cl-functions))

(setq visible-bell nil
      track-eol t
      ring-bell-function 'ignore
      confirm-kill-processes nil
      disabled-command-function nil
      inhibit-compacting-font-caches t
      custom-unlispify-tag-names nil)

(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-step 1
      hscroll-margin 1)

(setq resize-mini-windows t
      enable-recursive-minibuffers t
      column-number-mode 1
      fringes-outside-margins t)

(setq kill-do-not-save-duplicates t
      kill-ring-max 250
      mark-ring-max 10
      select-enable-clipboard t
      select-enable-primary nil
      delete-by-moving-to-trash t)

(setq use-dialog-box nil
      woman-use-own-frame nil
      help-window-select t
      mouse-autoselect-window -0.05
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)

      man-notify-method 'pushy
      ad-redefinition-action 'accept)

(setq-default tab-width 4
	          indent-tabs-mode nil
	          truncate-lines t)

(fset 'yes-or-no-p 'y-or-n-p)


;;; Enviroments

(setenv "TZ" "PRC")
(setenv "LC_COLLATE" "C")
(setenv "LC_ALL" "en_US.UTF-8")

(when (and IS-WIN (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))


;;; Load/Exec Path

(with-eval-after-load 'predist ; make sure these are added before 'elpa/*' items
  (cl-loop for d in ic/external-paths
           if (file-exists-p d) do
           (setenv "PATH" (concat d (if IS-WIN ";" ":") (getenv "PATH")))
           (add-to-list 'exec-path d))

  (when IS-WIN (add-to-list 'ic/external-paths (loce "bin/w") t))

  (cl-loop for root in (append ic/external-load-path-tops (list (loce "extra") (loce "fork")))
           for dirs = (ignore-errors (directory-files root t))
           do (cl-loop for dir in dirs
                       if (and (not (eq (file-name-extension dir) "")) (file-directory-p dir)) do
                       (add-to-list 'load-path dir))))

(add-to-list 'custom-theme-load-path (loce "themes"))


;;; Custom File

(setq custom-file
      (if (boundp 'custome-file-private) custome-file-private
        (let ((n (format "init-%s@%s.el"
                         (string-join (reverse (split-string (downcase (system-name)) "\\.")) ".")
                         (user-real-login-name)))
              (d (loco "x.share/emacs/inits/")))
          (if (file-exists-p d)
              (expand-file-name n d)
            (loce n)))))

(unless (file-exists-p custom-file)
  (with-temp-file custom-file (insert (format ";; %s" (system-name)))))

(defun:around custom-set-faces$defer-run (f &rest args)
  ;; "`custom-set-face' has highest period!"
  (defun-hook after-init-hook/defer-custom () (apply f args)))

(load custom-file t t)


;;; Proxy

(setq url-gateway-local-host-regexp
      (concat "^"
              (regexp-opt
               '("localhost"
                 "127.0.0.1"
                 "192.168." "10."))))

(defun im/curl-options--replace-proxy (&optional proxy)
  (if (and (boundp 'request-curl-options) request-curl-options)
      (setq request-curl-options
            (remove "-x"
                    (remove
                     (nth
                      (+ (cl-position "-x" request-curl-options :test 'string=) 1)
                      request-curl-options)
                     request-curl-options)))
    (setq request-curl-options nil))
  (when proxy
    (push proxy request-curl-options)
    (push "-x" request-curl-options)))

(defun im/proxy (&optional type)
  (interactive (list (intern (completing-read "type: " '(disable :sock :http) nil t))))
  (cond ((eq type :http)
         (let ((url (car ic/proxy-http))
               (user (cadr ic/proxy-http))
               (password (caddr ic/proxy-http)))
           (setq url-gateway-method 'native
                 socks-server nil
                 url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp)
                                      ("http" . ,url)
                                      ("https" . ,url)))
           (im/curl-options--replace-proxy url)
           (when user
             (setq url-http-proxy-basic-auth-storage `((,url (,user . ,password)))))
           (message "Http proxy %s enabled." url)))
        ((eq type :sock)
         (setq url-gateway-method 'socks
               socks-server ic/proxy-sock
               url-proxy-services nil)
         (im/curl-options--replace-proxy (format "socks5://%s:%d" (cadr ic/proxy-sock) (caddr ic/proxy-sock)))
         (message "Sock proxy %s enabled." ic/proxy-sock))
        (t
         (setq url-gateway-method 'native
               socks-server nil
               url-proxy-services nil)
         (im/curl-options--replace-proxy nil)
         (message "Proxy disabled."))))

(if ic/proxy-type (im/proxy ic/proxy-type)) ; initial proxy


;;; Preferences

(setq auto-mode-alist
      (append '(("\\.class\\'"           . class-mode)
                ("\\.scm\\'"             . scheme-mode)
                ("\\.\\(ba\\)?sh\\'"     . sh-mode)
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

        ("." nil (reusable-frames . t))))

(defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
  (display-buffer-at-bottom buffer alist)
  (select-window (get-buffer-window buffer))
  (with-current-buffer buffer (view-mode 1)))

;; sudo edit
(cl-loop with sudo-face
         = (lambda ()
             (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
               (setq header-line-format (propertize " Edit as ROOT! " 'face '(:foreground "white" :background "red3")))))
         for hook in '(find-file-hook dired-mode-hook)
         do (add-hook hook sudo-face)
         finally
         (defun su ()
           "Edit file as Super User."
           (interactive)
           (let ((pt (point)) (old-buf (current-buffer))
                 (buf-name (expand-file-name (or buffer-file-name default-directory))))
             (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
             (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
               (find-file buf-name))
             (kill-buffer-if-not-modified old-buf)
             (goto-char pt))))

;; read-only filter
(defun-hook find-file-hook/readonly ()
  "Files that should be readonly."
  (let* ((case-fold-search nil)
         (match? (lambda (&rest items) (string-match-p (apply #'join-as-regor-group items) buffer-file-name))))
    (when (or (and ic/find-file-readonly-regexp
                   (string-match-p ic/find-file-readonly-regexp buffer-file-name))
              (and (not
                    (funcall match? ; exclude
                             "/usr/home"
                             "\\.cache/" "tmp/" "vvv/" "notes"
                             "autoloads.el$" "loaddefs.el$"))
                   (funcall match? ; include
                            "^/usr/"
                            ".emacs.d/packages"
                            ".roswell/lisp/quicklisp")))
      (view-mode 1)
      (make-local-variable 'view-mode-map)
      (define-key view-mode-map "q" 'View-kill-and-leave))))

;; clean before save
(defun-hook before-save-hook/cleanup ()
  (unless (cl-find major-mode '(org-mode))
    (delete-trailing-whitespace)))

;; auto byte-compile
(defun-hook after-save-hook/bytecompile ()
  (when nil
    (save-window-excursion
      (when (and (eq major-mode 'emacs-lisp-mode)
                 (string-match-p "^[a-z1-9]" (buffer-name))
                 (string-match-p "\\/\\.emacs\\.d\\/\\(core\\|extra\\)\\/" (buffer-file-name)))
        (byte-compile-file (buffer-file-name))))))


;;; Miscellaneous Helpers

(defun imup ()
  (if ic/up (if (string-match-p "@" ic/up) ic/up (concat "vip@" ic/up))))

(defun up-host ()
  (aif (and ic/up (split-string (imup) "@")) (cadr it)))

(defun up-user ()
  (aif (and ic/up (split-string (imup) "@")) (car it)))

(defun log/it (&rest args)
  "Output messsage to a buffer. Usage: (log/it [buffer-name] fmtString variable)."
  (let ((buffer-name (if (symbolp (car args))
                         (prog1 (symbol-name (car args))
                           (setq args (cdr args)))
                       "*logger*")))
    (with-current-buffer (get-buffer-create buffer-name)
      (local-set-key (kbd "q") 'bury-buffer)
      (goto-char (point-max))
      (insert "\n")
      (insert (apply 'format args)))))

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat 'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))


(provide 'cust)

;;; cust.el ends here
