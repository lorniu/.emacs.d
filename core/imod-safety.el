;;; imod-safety.el --- Protect/Security/Auth/Cipher -*- lexical-binding: t -*-

;; Get password from .authinfo:
;;
;;   (lookup-password "vip" :port 444)
;;

;;; Code:

(defvar ic/backup-dir (locc "backup-files"))
(defvar ic/auto-save-dir (locc "auto-save"))

(setq create-lockfiles nil)   ; .#lock-file

(setq vc-make-backup-files t  ; also backup versioned files
      backup-inhibited nil    ; version backup file~~
      backup-by-copying t
      kept-new-versions 6 kept-old-versions 2 delete-old-versions t version-control t
      backup-directory-alist `(("\\.cache\\|\\.ssh") ("." . ,ic/backup-dir)))

(setq auto-save-default t ; save #periodically# to avoid crashes
      auto-save-interval 300
      auto-save-timeout 30
      auto-save-list-file-prefix (expand-file-name ".saves-" ic/auto-save-dir))

(setq delete-by-moving-to-trash t)

(x trashed :ref "shingo256/trashed")



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

(x aes/e
   "Builtin: (secure-hash 'sha256 obj)"
   :ref "Sauermann/emacs-aes")



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

(provide 'imod-safety)

;;; imod-safety.el ends here
