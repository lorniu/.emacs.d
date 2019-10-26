;;; init.el

(package-initialize)

(let ((f (locate-user-emacs-file (format "init_%s.el" (system-name)))))
  (unless (file-exists-p f) (with-temp-buffer (write-file f)))
  (setq custom-file f))

(progn
  (add-to-list 'load-path (locate-user-emacs-file "core"))
  (require 'imfine))
