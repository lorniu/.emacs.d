;;; imod-tramp.el --- Tramp -*- lexical-binding: t -*-

;;; Code:

(x tramp/i
   :init
   (setq tramp-verbose 3)
   (setq tramp-persistency-file-name (locc "tramp"))
   (setq remote-file-name-inhibit-cache nil)
   (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

   (defun im/set-ssh-askpass ()
     "Maybe you should set path of askpass, for prompt password."
     (interactive)
     (unless IS-WIN (user-error "This is used for Windows System."))
     (let ((exe (read-file-name "askpass executable: "
                                (or (getenv "MSYS_HOME")
                                    (let ((bin (executable-find "bash")))
                                      (if bin
                                          (file-name-parent-directory
                                           (file-name-parent-directory
                                            (file-name-directory bin)))))
                                    "c:/")
                                nil t)))
       (if (and exe (file-exists-p exe))
           (progn (setenv "SSH_ASKPASS" exe)
                  (message "Set SSH_ASKPASS to %s." exe))
         (message "Error askpass file found, do nothing."))))

   :config
   ;; fix a bug on FreeBSD: echo `uname -r` does not return a valid Lisp expression...
   (when IS-BSD
     (require 'tramp-sh)
     (defun:before tramp-send-string$ (_ str)
       (when (string-match-p "`uname -sr`" str)
         (sit-for 1)))))

(provide 'imod-tramp)

;;; imod-tramp.el ends here
