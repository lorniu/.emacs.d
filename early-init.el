;;; Debug

(defvar idebug nil)

(when (or (getenv-internal "D") (getenv-internal "DEBUG") (getenv-internal "DM"))
  (setq init-file-debug t debug-on-error t)
  (setq idebug t benchmark-require t))

(setq default-frame-alist `((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)))

(setq gc-cons-threshold most-positive-fixnum
      byte-compile-warnings '(cl-defun)

      package-enable-at-startup nil
      garbage-collection-messages nil

      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t

      inhibit-message t
      redisplay-skip-fontification-on-input t)

(advice-add #'x-apply-session-resources :override #'ignore)
