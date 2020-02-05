;;; -*- lexical-binding: t -*-

;;; Code

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-BSD     (eq system-type 'berkeley-unix))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WIN     (memq system-type '(cygwin windows-nt ms-dos)))
(defconst NATIVECOMP (if (fboundp 'native-comp-available-p) (native-comp-available-p)))


;;; Optimize

(defvar gc-cons-threshold-default (* 128 1024 1024))
(defvar file-name-handler-alist-default file-name-handler-alist)

(setq-default default-directory "~/"

              gc-cons-threshold most-positive-fixnum
              garbage-collection-messages nil
              file-name-handler-alist nil

              load-prefer-newer noninteractive
              frame-inhibit-implied-resize t
              inhibit-compacting-font-caches t

              package-enable-at-startup nil
              package--init-file-ensured t

              inhibit-message t
              inhibit-redisplay t

              bidi-inhibit-bpa t
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(when IS-WIN
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

(advice-add #'x-apply-session-resources :override #'ignore)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold-default)
            (setq file-name-handler-alist (delete-dups (append file-name-handler-alist file-name-handler-alist-default)))))

(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-message nil inhibit-redisplay nil)
            (redisplay)))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (run-at-time 3 nil (lambda () (setq gc-cons-threshold gc-cons-threshold-default)))))


;;; Paths & Helpers

(add-to-list 'load-path (locate-user-emacs-file "core"))

(setf (symbol-function 'locate-user-emacs-file-origin) (symbol-function 'locate-user-emacs-file))
(advice-add #'locate-user-emacs-file :override (defun locate-user-emacs-file$redirect-to-cache-dir (new-name &optional _) (expand-file-name new-name (locate-user-emacs-file-origin "var"))))

(defun loce (subpath &optional nullp) (let ((f (locate-user-emacs-file-origin subpath))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun locc (&optional subpath nullp) (let ((f (locate-user-emacs-file (or subpath "./")))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun loco (subpath &optional nullp) (let ((f (expand-file-name subpath org-directory))) (if (or (not nullp) (file-exists-p f)) f)))

(defmacro with-over (&rest args) `(with-eval-after-load 'over ,@args))


;;; Go.

(require 'cl-lib)
(require 'subr-x)

(let ((f (locate-user-emacs-file-origin "aux-init.el")))
  (if (file-exists-p f) (load f t t)
    (with-temp-file f
      "(setq debug-on-error nil)\n(setq benchmark-require nil)")))

(require 'bm)
(imload 'cust)
(imload 'utils)
(imload 'predist)
(imload 'preface)

(imload 'cust-proxy)
(imload 'cust-lists-and-hooks)

;;; early-init.el ends here
