;;; imod-auth.el --- Auth -*- lexical-binding: t -*-

;; Get password from .authinfo:
;;
;;   (lookup-password "vip" :port 444)
;;

;;; Code:

(x auth-source
   :init
   (setq auth-source-cache-expiry nil))

(x password-cache
   "Manage password in memory."
   :init
   (setq password-cache-expiry nil))

(x ssh-agency
   :if IS-WIN
   :after magit)



(cl-defun lookup-password (&rest params &key _user _host _port &allow-other-keys)
  "Query password stored in '.authinfo'."
  (require 'auth-source-pass)
  (when (stringp (car params))
    (setq params (cons :user params)))
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (user-error "Password not found for %S" params))))

(provide 'imod-auth)

;;; imod-auth.el ends here
