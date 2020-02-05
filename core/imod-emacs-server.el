;;; imod-emacs-server.el --- Emacs Server -*- lexical-binding: t -*-

;;; Code:

(when IS-G
  (require 'server)
  (condition-case err
      (progn
        (setq server-auth-dir (locc "server/"))
        (delete-file (concat server-auth-dir "server"))
        (unless (server-running-p) (server-start)))
    (error (message "Server starting error: %s" err))))

(provide 'imod-emacs-server)

;;; imod-emacs-server.el ends here
