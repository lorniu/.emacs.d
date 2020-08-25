;;; cust.el --- Basic Configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup imfine nil "Private custom variables"
  :group 'emacs
  :prefix "ic/")

(when (and IS-WIN (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

(setq user-full-name "imfine"
      user-mail-address "lorniu@gmail.com"

      inhibit-startup-message t
      gnus-inhibit-startup-message t
      auto-mode-case-fold nil
      enable-local-eval t
      enable-local-variables :all

      byte-compile-warnings '(cl-functions))

(setf (symbol-function 'loce-origin) (symbol-function 'locate-user-emacs-file))
(defun:override locate-user-emacs-file$ (new-name &optional _) (expand-file-name new-name ic/cache-dir))

(defun loce (subpath &optional nullp) (let ((f (loce-origin subpath))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun locc (&optional subpath nullp) (let ((f (locate-user-emacs-file (or subpath "./")))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun loco (subpath &optional nullp) (let ((f (expand-file-name subpath org-directory))) (if (or (not nullp) (file-exists-p f)) f)))


;;; Customization

(defcustom org-directory (let ((d "~/.notes")) (expand-file-name (if (file-exists-p d) d "~/org")))
  "Main Directory for GTD/Notes/Diary and others."
  :type 'string :group 'imfine)

(defcustom ic/up nil
  "Private Host"
  :type 'string)

(defcustom ic/cache-dir "~/.cache/emacs/"
  "Cache Directory"
  :type 'string)

(defcustom ic/elpa-use-mirror (if (current-vm-p) nil t)
  "Elpa use mirror or not."
  :type 'boolean)

(defcustom ic/package-lock-alist nil
  "Packages to disable or redirect. ((p . v)), v should be version/nil/dir."
  :type 'list)

(defcustom ic/faces nil
  "(:title a :font (cons b 120) :theme c)"
  :type 'list)

(defcustom ic/external-paths (list (loce "bin/x") (loco "x.bin"))
  "Extend the PATH environment."
  :type '(repeat string))

(defcustom ic/external-load-path-top nil
  "Extra load-path root."
  :type 'list)

;;
(defcustom ic/my-postgres (list "postgres" "root")
  "Postgres connection info, (db user [password] [host] [port]) format."
  :type 'list)

(defcustom ic/lisp-init-file (loce "share/lisp/sly.lisp")
  "Common Lisp init file."
  :type 'file)

(defcustom ic/feeds-org-file (or (loco "000/favors.org" t) (loce "elfeeds.org"))
  "Feeds Used for `elfeed', an org file."
  :type 'string)

;;
(defcustom ic/favorites nil
  "Favorite files, used by `im/view-favorites'."
  :type '(alist :key-type string))

(defcustom ic/favorite-filter nil
  "Extra filter. A function to filter current ITEM."
  :type 'string)

;;
(defcustom ic/proxy-type nil
  "Which proxy to use, http or sock."
  :type 'symbol
  :group 'imfine)

(defcustom ic/proxy-sock '("Default server" "127.0.0.1" 1080 5)
  "Socket proxy default value."
  :type 'list)

(defcustom ic/proxy-http '("127.0.0.1:8118" nil nil)
  "Http proxy default value: (url user password)"
  :type 'string )


;;; Helpers

(defun imup ()
  (if ic/up (if (string-match-p "@" ic/up) ic/up (concat "vip@" ic/up))))

(defun up-host ()
  (aif (and ic/up (split-string (imup) "@")) (cadr it)))

(defun up-user ()
  (aif (and ic/up (split-string (imup) "@")) (car it)))

(defmacro >>face>> (&rest body)
  `(with-eval-after-load 'face ,@body))

(defmacro >>init>> (&rest body)
  `(add-to-list 'after-init-hook
                (defun my-after-init-do-something ()
                  ,@body)))


;;; System-Variables

(setq track-eol t
      visible-bell nil
      ring-bell-function 'ignore
      confirm-kill-processes nil
      disabled-command-function nil
      inhibit-compacting-font-caches t
      custom-unlispify-tag-names nil

      scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-step 1
      hscroll-margin 1

      resize-mini-windows t
      enable-recursive-minibuffers t
      column-number-mode 1
      fringes-outside-margins t

      kill-do-not-save-duplicates t
      kill-ring-max 250
      mark-ring-max 10
      select-enable-clipboard t
      select-enable-primary nil
      delete-by-moving-to-trash t
      trash-directory (locc "trash")

      use-dialog-box nil
      woman-use-own-frame nil
      help-window-select t
      mouse-autoselect-window -0.05
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)

      man-notify-method 'pushy
      ad-redefinition-action 'accept

      create-lockfiles nil
      backup-directory-alist `((".*" . ,(locc "backup-files/")))

      auto-save-interval 0
      auto-save-visited-file-name t
      auto-save-no-message t
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix (locc (if IS-WIN "auto-save.list/_s" "auto-save-list/.saves-"))
      auto-save-list-file-name (expand-file-name "hahahaha~" auto-save-list-file-prefix ))

(setq-default tab-width 4
	          indent-tabs-mode nil
	          truncate-lines t)

(setenv "TZ" "PRC")
(setenv "LC_COLLATE" "C")
(setenv "LC_ALL" "en_US.UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)


;;; Load/Exec Path

(when IS-WIN
  (add-to-list 'ic/external-paths (loce "bin/w") t))

(cl-loop for d in ic/external-paths
         if (file-exists-p d) do
         (setenv "PATH" (concat d (if IS-WIN ";" ":") (getenv "PATH")))
         (add-to-list 'exec-path d))

(cl-loop for root in (append ic/external-load-path-top (list (loce "extra") (loce "fork")))
         for dirs = (directory-files root t)
         do (cl-loop for dir in dirs
                     if (and (not (eq (file-name-extension dir) "")) (file-directory-p dir)) do
                     (add-to-list 'load-path dir)))

(add-to-list 'custom-theme-load-path (loce "extra/themes"))


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
  "`custom-set-face' has highest period!"
  (add-hook-fun after-init-hook/defer-custom () (apply f args)))

(custom-set-variables)
(custom-set-faces)

(load custom-file t t)


;;; Proxy

(setq url-gateway-local-host-regexp
      (concat "^" (regexp-opt '("localhost" "127.0.0.1" "192.168." "10."))))

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
  (interactive (list (intern (completing-read "type: " '(disable SOCK HTTP) nil t))))
  (cond ((eq type 'HTTP)
         (let ((url (car ic/proxy-http))
               (user (cadr ic/proxy-http))
               (password (caddr ic/proxy-http)))
           (setq url-gateway-method 'native socks-server nil)
           (setq url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp)
                                      ("http" . ,url)
                                      ("https" . ,url)))
           (im/curl-options--replace-proxy url)
           (when user
             (setq url-http-proxy-basic-auth-storage `((,url (,user . ,password)))))
           (message "Http proxy %s enabled." url)))
        ((eq type 'SOCK)
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


;;; Favorites

(defvar favorites-default
  '(("notes/"                   org-directory  :project)
    ("emacs/"                   (loce "core/") :project)
    ("emacs.cache/"             (locc))
    ("conf: emacs.private-init" custom-file)
    ("conf: xmonad.hs"          (loce "share/xmonad/xmonad.hs"))

    ("host: Dropbox"            "https://dropbox.com")
    ("host: MvnRepository"      "https://mvnrepository.com")

    ((format "ssh: %s" (imup))  (format "/ssh:%s:~/" (imup))                         (imup))

    ("sys: /usr/local/"         "/sudo::/usr/local/"                                 IS-BSD)
    ("sys: /etc/systemd/"       "/sudo::/etc/systemd/system/multi-user.target.wants" (file-directory-p "/etc/systemd"))
    ("sys: pacman.conf"         "/sudo::/etc/pacman.conf"                            (file-exists-p "/etc/pacman.conf"))
    ("sys: Windows Homepath"    (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))    IS-WIN))
  "((label path modes)+)")

(defun im/view-favorites ()
  "View the favorites. 3rd args can be :interactive/:readonly/IS-LINUX..."
  (interactive)
  (cl-labels ((label-tag (label) ; (host/x xxx host)
                         (if (string-match-p ": " label)
                             (let ((ret (split-string label ": ")))
                               (append ret (list (if (string-match-p "/" (car ret))
                                                     (car (split-string label "/"))
                                                   (car ret)))))))
              (db-p (label)
                    (string-match-p "^db:" label))
              (host-p (label)
                      (aif (label-tag label) (string-match-p "^host" (car it)))))
    (let* ((selectrum-should-sort-p nil)
           (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                       (define-key map (kbd "TAB") 'selectrum-select-current-candidate)
                                       map))
           (candidates (mapcar (lambda (item)
                                 (let ((label (car item)) (path (cadr item)))
                                   (when (not (stringp label))
                                     (setq label (eval label)))
                                   (when (and (not (db-p label)) (not (stringp path)))
                                     (setq path (eval path)))
                                   ;; face
                                   (cond ((listp path))
                                         ((file-remote-p path)
                                          (setq label (propertize label 'face '(:weight bold))))
                                         ((file-directory-p path)
                                          (setq label (propertize label 'face dired-directory-face))))
                                   ;; tag/format
                                   (aif (label-tag label)
                                     (setq label
                                           (cond ((db-p label)
                                                  (format "%-6s %s"
                                                          (propertize (concat (car it) ":") 'face 'font-lock-keyword-face)
                                                          (cadr it)))
                                                 ((host-p label)
                                                  (format "%-22s %s"
                                                          (format "%-6s %s"
                                                                  (propertize (concat (car it) ":") 'face dired-header-face)
                                                                  (cadr it))
                                                          (propertize (concat "(" path ")") 'face dired-ignored-face)))
                                                 (t
                                                  (format "%-6s %s"
                                                          (propertize (concat (car it) ":") 'face 'font-lock-string-face)
                                                          (cadr it))))))
                                   (append (list label path) (cddr item))))
                               (append ic/favorites favorites-default)))
           (candidates-refind (cl-remove-if-not
                               (lambda (item)
                                 (let ((label (car item)) (path (cadr item))
                                       (sex-p (cl-find-if 'listp (cddr item))))
                                   (and path
                                        (not (cl-find :disabled (cddr item)))
                                        (or (null sex-p)
                                            (eval sex-p))
                                        (or (null ic/favorite-filter)
                                            (not (funcall ic/favorite-filter item)))
                                        (or (db-p label)
                                            (host-p label)
                                            (file-remote-p path)
                                            (file-exists-p path)))))
                               (append
                                (cl-remove-if #'label-tag candidates :key 'car)
                                (cl-sort (cl-remove-if-not #'label-tag candidates :key 'car)
                                         (lambda (x y)
                                           (let ((tag-x (label-tag x))
                                                 (tag-y (label-tag y))
                                                 (host-x-p (host-p x))
                                                 (host-y-p (host-p y)))
                                             (cond ((and host-x-p (not host-y-p)) nil)
                                                   ((and host-y-p (not host-x-p)) t)
                                                   ((and tag-x tag-y
                                                         (string-equal (caddr tag-x) (caddr tag-y))
                                                         (not (string-equal (car tag-x) (car tag-y))))
                                                    (< (length (car tag-x)) (length (car tag-y))))
                                                   (t (string-lessp x y)))))
                                         :key #'car))))
           (candidate (completing-read "Favor: " candidates-refind nil t nil nil (caar candidates-refind)))
           (item (or (cl-find-if (lambda (c) (string= (car c) candidate)) candidates-refind) (user-error "Nothing to do.")))
           (label (car item))
           (path (cadr item))
           (modes (cddr item)))
      (cond ((db-p label) ; DB link
             (sql-connect (car path)))

            ((host-p label) ; HOST link
             (condition-case _ (browse-url path)
               (error (browse-web path))))

            ;; directory
            ((and (file-directory-p path) (memq :project modes))
             (let ((default-directory path))
               (call-interactively 'projectile-find-file)))
            ((and (file-directory-p path) (memq :project-dir modes))
             (let ((default-directory path))
               (call-interactively 'projectile-find-dir)))
            ((file-directory-p path)
             (let ((default-directory path))
               (call-interactively 'dired)))

            ((memq :readonly modes) (im/open-file-view path))
            (t (find-file path))))))

(defun im/add-favorite (label path &optional first-p mode)
  "Add new file to `ic/favorites', MODE can be :interactive/:project/:project-dir/:readonly or (show-condition)."
  (interactive (let ((f (read-file-name "Choose File: " nil nil t)))
                 (list (file-name-nondirectory f) f)))
  (if (and (not (string-prefix-p "db" label))
           (not (string-prefix-p "host" label))
           (not (file-remote-p path))
           (not (file-exists-p path)))
      (user-error "Failed, file '%s' Not Found." path)
    (let ((favor (list label path)))
      (if mode (setq favor (append favor (list mode))))
      (add-to-list 'ic/favorites favor (not first-p)))
    (if (called-interactively-p 'any) (message "Favor file '%s' added successfully." label) label)))


;;; References

(defreference emacs
  ("https://gnu.org/software/emacs/download.html"
   "Windows Mirror: http://mirrors.ustc.edu.cn/gnu/emacs/windows/"
   "Alpha Version: https://alpha.gnu.org/gnu/emacs/pretest/windows/"
   "elc -> eln: http://akrl.sdf.org/gccemacs.html"
   "Melpa Homepage: https://melpa.org/#/"
   "Melpa Github: https://github.com/melpa/melpa"
   "Config: hlissner/doom-emacs"))

(defreference downloads
  ("SQLite3: https://sqlite.org/download.html"
   "Graphviz: http://graphviz.org/download/"))


(provide 'cust)

;;; cust.el ends here
