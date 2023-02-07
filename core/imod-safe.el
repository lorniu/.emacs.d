;;; -*- lexical-binding: t -*-

;; Get password from .authinfo:
;;
;;   (lookup-password "vip" :port 444)
;;

;;; Code:

(defvar ic/backup-dir (locc "backup-files"))
(defvar ic/auto-save-dir (locc "auto-save"))

(setq trusted-content :all)   ; trust

(setq create-lockfiles nil)   ; .#lock-file

(setq vc-make-backup-files t  ; also backup versioned files
      backup-inhibited nil    ; version backup file~~
      backup-by-copying t
      kept-new-versions 6 kept-old-versions 2 delete-old-versions t version-control t
      backup-directory-alist `(("." . ,ic/backup-dir)))

(setq auto-save-default t ; save #periodically# to avoid crashes
      auto-save-interval 300
      auto-save-timeout 30
      auto-save-list-file-prefix (expand-file-name ".saves-" ic/auto-save-dir))

(setq delete-by-moving-to-trash t)

(x trashed :ref "shingo256/trashed")



(x auth-source
   :config
   (setopt auth-source-cache-expiry nil))

(x password-cache
   "Manage password in memory."
   :config
   (setopt password-cache-expiry nil))

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
  (when-let* ((match (car (apply 'auth-source-search params))))
    (let ((secret (plist-get match :secret)))
      (if (functionp secret)
          (funcall secret)
        secret))))

(defun im/set-ssh-askpass-for-windows (&optional askpass.exe)
  "Maybe you should set path of askpass to solute the issue of password prompt."
  (interactive (list (if IS-WIN
                         (read-file-name "Path of askpass.exe: "
                                         (or (getenv "MSYS_HOME")
                                             (let ((bin (executable-find "bash")))
                                               (if bin
                                                   (file-name-parent-directory
                                                    (file-name-parent-directory
                                                     (file-name-directory bin)))))
                                             "c:/")
                                         nil t)
                       (user-error "This is used for Windows only"))))
  (if (and askpass.exe (file-regular-p askpass.exe))
      (progn (setenv "SSH_ASKPASS" askpass.exe)
             (when (called-interactively-p 'any)
               (message "Set SSH_ASKPASS to %s." askpass.exe)))
    (message "Error askpass file found, do nothing.")))
