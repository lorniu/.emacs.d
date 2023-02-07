;;; -*- lexical-binding: t -*-

;; Get password from .authinfo:
;;
;;   (lookup-password "vip" :port 444)
;;

;;; Code:

(xzz auth-source
  :config
  (setopt auth-source-cache-expiry nil))

(xzz password-cache
  "Manage password in memory."
  :config
  (setopt password-cache-expiry nil))

(xzz ssh-agency
  :if IS-WIN
  :after magit)

(xzz aes/e
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
