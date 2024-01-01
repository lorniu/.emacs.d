;;; -*- lexical-binding: t -*-

;;; Code:

(setq gc-cons-threshold most-positive-fixnum

      inhibit-message t
      inhibit-startup-screen t
      garbage-collection-messages nil

      inhibit-default-init t
      package-enable-at-startup nil

      inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t
      frame-inhibit-implied-resize t

      load-prefer-newer noninteractive
      read-process-output-max (* 512 1024) ; 512K

      bidi-inhibit-bpa t
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right)

(setq-default default-frame-alist `((menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (vertical-scroll-bars)))

(advice-add #'display-startup-screen :override #'ignore)

;; Debug mode
(when (or (getenv-internal "D") (getenv-internal "DEBUG") (getenv-internal "DM"))
  (setq init-file-debug t debug-on-error t benchmark-require t))

;; Suppress file handlers at startup to speed up `require' and `load'
(unless (or (daemonp) init-file-debug)
  (let ((bk file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda () (setq file-name-handler-alist (delete-dups (append file-name-handler-alist bk))))
              1001)))

;; Windows performance

(when (memq system-type '(cygwin windows-nt))
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-use-native-image-API t         ; use native w32 API
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (64K, was 4K)

;; Silence obnoxious obsoletion warnings

(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)

;;; early-init.el ends here
