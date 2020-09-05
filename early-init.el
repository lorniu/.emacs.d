;; -*- lexical-binding: t -*-

(defvar quiet-init nil)

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-BSD     (eq system-type 'berkeley-unix))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WIN     (memq system-type '(cygwin windows-nt ms-dos)))


;;; Optimize

(defvar gc-cons-threshold-default (* 64 1024 1024)) ; 64mb
(defvar file-name-handler-alist-default file-name-handler-alist)

(setq-default inhibit-message t
              garbage-collection-messages nil
              gc-cons-threshold most-positive-fixnum
              gc-cons-percentage 0.6
              file-name-handler-alist nil

              default-directory "~/"

              load-prefer-newer noninteractive
              frame-inhibit-implied-resize t
              inhibit-compacting-font-caches t

              package-enable-at-startup t
              package--init-file-ensured t

              bidi-inhibit-bpa t
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(when IS-WIN
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

(add-hook 'emacs-startup-hook
          (defun my-reset-variables-after-startup ()
            (dolist (handler file-name-handler-alist)
              (add-to-list 'file-name-handler-alist-default handler))
            (setq gc-cons-threshold gc-cons-threshold-default
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-default)))

(add-hook 'minibuffer-setup-hook
          (defun my-defer-garbage-collection-h ()
            (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (defun my-restore-garbage-collection-h ()
            (run-at-time 3 nil (lambda () (setq gc-cons-threshold gc-cons-threshold-default)))))

(advice-add #'x-apply-session-resources :override #'ignore)


;;; Paths

(add-to-list 'load-path (locate-user-emacs-file "core"))

(setf (symbol-function 'locate-user-emacs-file-origin) (symbol-function 'locate-user-emacs-file))
(advice-add #'locate-user-emacs-file :override (defun locate-user-emacs-file$redirect-to-cache-dir (new-name &optional _) (expand-file-name new-name "~/.cache/emacs/")))


;;; Extra Early Config

(defun my-insert-with-newline (&rest args)
  (insert (mapconcat 'identity args "\n")))

(defun my-init-inis-content (inis-file)
  (with-temp-file inis-file
    (my-insert-with-newline
     "(setq quiet-init nil)"
     "(setq debug-on-error nil)")))

(defun my-quiet-init-setup ()
  (with-current-buffer "*scratch*"
    (my-insert-with-newline
     "(package-initialize)"
     "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))"
     "(package-refresh-contents)" ""
     "(progn (require 'selectrum) (selectrum-mode))" ""
     "(find-file \"~/.emacs.d/inis.el\")"))
  (setq custom-file nil)
  (show-paren-mode 1))

(let ((f (locate-user-emacs-file-origin "inis.el")))
  (if (file-exists-p f) (load f t t)
    (my-init-inis-content f)))


;;; Go

(require 'bm)

(if quiet-init
    (add-hook 'emacs-startup-hook 'my-quiet-init-setup)
  (imload 'util)
  (imload 'cust)
  (imload 'package)
  (imload 'preface)
  (imload 'predist))


;;; early-init.el ends here
