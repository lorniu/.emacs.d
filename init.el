;;; init.el

(package-initialize)

(defvar +note+ "~/.notes")

(let* ((n (format "init-%s@%s.el" (user-real-login-name) (system-name)))
       (f (if (file-exists-p +note+)
              (format "%s/x.share/emacs/%s" +note+ n)
            (locate-user-emacs-file n))))
  (unless (file-exists-p f) (with-temp-buffer (write-file f)))
  (setq custom-file f))

(progn
  (add-to-list 'load-path (locate-user-emacs-file "core"))
  (require 'imfine))
