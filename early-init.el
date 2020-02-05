;; -*- lexical-binding: t -*-

(setq debug-on-error nil)
(setq garbage-collection-messages nil)

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-BSD     (eq system-type 'berkeley-unix))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WIN     (memq system-type '(cygwin windows-nt ms-dos)))


;;; Optimize

(defvar gc-cons-threshold-default (* 64 1024 1024)) ; 64mb
(defvar file-name-handler-alist-default file-name-handler-alist)

(setq inhibit-message t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil

      default-directory "~/"

      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      bidi-inhibit-bpa t
      inhibit-compacting-font-caches t

      package-enable-at-startup t
      package--init-file-ensured t)

(setq-default bidi-display-reordering 'left-to-right bidi-paragraph-direction 'left-to-right)

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


;;; Prepare

(let ((f (locate-user-emacs-file "inis.el")))
  (if (file-exists-p f) (load f t t)
    (with-temp-file f (insert "(setq debug-on-error nil)"))))

(add-to-list 'load-path (locate-user-emacs-file "core"))


;;; Go

(require 'bm)
(imload 'util)
(imload 'package)
(imload 'cust)
(imload 'preface)
(imload 'predist)


;;; early-init.el ends here
