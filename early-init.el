;;; Debug

(defvar idebug nil)

(when (or (getenv-internal "D") (getenv-internal "DEBUG") (getenv-internal "DM"))
  (setq init-file-debug t debug-on-error t)
  (setq idebug t benchmark-require t))

;;; Faces

(setq default-frame-alist `((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)))

;;; Startup optimizations

(defvar gc-cons-threshold-default (* 128 1024 1024))
(defvar file-name-handler-alist-default file-name-handler-alist)

(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum

      garbage-collection-messages nil
      file-name-handler-alist nil

      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t

      inhibit-message t
      inhibit-redisplay t
      redisplay-skip-fontification-on-input t

      bidi-inhibit-bpa t
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right)

(when (boundp 'w32-get-true-file-attributes)
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
            (setq inhibit-message nil inhibit-redisplay nil)
            (redraw-frame)))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (run-at-time 3 nil (lambda () (setq gc-cons-threshold gc-cons-threshold-default)))))
