;;; iserver.el --- emacsclient -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when IS-G
  (require 'server)
  (condition-case err
      (progn
        (setq server-auth-dir (locc "server/"))
        (delete-file (concat server-auth-dir "server"))
        (unless (server-running-p) (server-start)))
    (error (message "Server starting error: %s" err))))


(provide 'iserver)

;;; iserver.el ends here
