;; -*- lexical-binding: t -*-

(unless (boundp 'pure-load-path) ; for Version < 27
  (load (locate-user-emacs-file "early-init.el") nil t))

(defconst IS-G         (display-graphic-p))
(defconst IS-NG        (not (display-graphic-p)))
(defconst IS-LINUX-G   (and IS-LINUX (display-graphic-p)))
(defconst IS-LINUX-NG  (and IS-LINUX (not (display-graphic-p))))



(imload 'dist)
(imload 'face)
(imload 'imccc)

(imload 'imoox)
(imload 'immor)
(imload 'imcoding)
(imload 'imdb)
(imload 'imtex)

(imload 'ickeys)
(imload 'ichydra)
(imload 'ickmacro)

(imload 'imsilly)
(imload 'imtips)



(when IS-G
  (require 'server)
  (condition-case err
      (progn
        (setq server-auth-dir (locc "server/"))
        (delete-file (concat server-auth-dir "server"))
        (unless (server-running-p) (server-start)))
    (error (message "Server starting error: %s" err))))


;;; init.el ends here
