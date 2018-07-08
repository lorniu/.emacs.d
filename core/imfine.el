;;; imfine.el --- This is my personal emacs configuration.

;; Copyright 2008 by imfine. All rights reserved.

;; Author: lorniu@gmail.com
;; Version: 0.02
;; License: GPLv3

;;; Commentary:

;;; Code:

(setq debug-on-error nil)

(let ((messaging-on nil)
      (file-name-handler-alist nil)
      (gc-cons-threshold (* 64 1024 1024)))

  (require 'bm)
  (require 'imutil)

;;; Environments

  (defmacro define-environments (envs)
    `(progn ,@(mapcar (lambda (e) `(defmacro ,(car e) (&rest body) `(when ,',(cdr e) ,@body ,',(cdr e)))) envs)))

  (define-environments
    ((env-windows       . (eq system-type 'windows-nt))
     (env-classroom     . (and (eq system-type 'windows-nt) (string= user-login-name "lol")))
     (env-out-classroom . (and (eq system-type 'windows-nt) (not (string= user-login-name "lol"))))
     (env-g             . (display-graphic-p))
     (env-ng            . (not (display-graphic-p)))
     (env-linux         . (eq system-type 'gnu/linux))
     (env-linux-g       . (and (env-linux) (env-g)))
     (env-linux-ng      . (and (env-linux) (env-ng)))
     (env-linux-vps     . (string= (system-name) "remote"))))

;;; Hook for special machine

  (defun -my/load-custom ()
    (let ((sp-in-fi (format "~/.emacs.d/init_%s.el" system-name)))
      (setq custom-file sp-in-fi)
      (when (file-exists-p sp-in-fi)
        (load sp-in-fi t t))))

;;; Modules
  (require 'cust)
  (require 'imface)
  (-my/load-custom)
  (require 'imkeys)
  (require 'imoox)
  (require 'immor)
  (require 'imnet)
  (require 'imsilly)
  (env-windows (im/start-server)))

(provide 'imfine)

;;; imfine.el ends here
