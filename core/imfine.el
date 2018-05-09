;;; imfine.el --- This is my personal emacs configuration.

;; Copyright 2008 by imfine. All rights reserved.

;; Author: lorniu@gmail.com
;; Version: 0.01
;; License: GPLv3

;;; Commentary:

;;; Code:

(setq debug-on-error nil)

(require 'bm)
(require 'imutil)

;;; Environments

(defmacro define-environments (envs)
  `(progn ,@(mapcar (lambda (e) `(defmacro ,(car e) (&rest body) `(when ,',(cdr e) ,@body ,',(cdr e)))) envs)))

(define-environments
  ((env-windows       . (eq system-type 'windows-nt))
   (env-classroom     . (and (eq system-type 'windows-nt) (string= user-login-name "lol")))
   (env-out-classroom . (and (eq system-type 'windows-nt) (not (string= user-login-name "lol"))))
   (env-linux         . (eq system-type 'gnu/linux))
   (env-linux-g       . (and (eq system-type 'gnu/linux) (display-graphic-p)))
   (env-linux-ng      . (and (eq system-type 'gnu/linux) (not (display-graphic-p))))
   (env-linux-vps     . (string= (system-name) "remote"))
   (env-graphic       . (display-graphic-p))))

;;; Hook for special machine

(let ((sp-in-fi (format "~/.emacs.d/_%s.el" system-name)))
  (when (file-exists-p sp-in-fi)
  (load sp-in-fi)))

;;; Modules

(let ((file-name-handler-alist nil)
      (gc-cons-threshold (* 100 1024 1024)))
  (require  'cust)
  (require  'imokeys)
  (require  'immor)
  (require  'imnet)
  (require  'imface)
  (require  'imsilly)
  (env-windows (im/start-server)))


(provide 'imfine)

;;; imfine.el ends here
