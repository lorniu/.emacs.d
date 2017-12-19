;;; This is my personal emacs configuration.

;; Copyright 2008 by imfine. All rights reserved.

;; Author: lorniu@gmail.com
;; Version: 0.01
;; License: GPLv3

;;; Code:


;;;
;;; different environments
;;;
(defmacro define-environments (envs)
  `(progn ,@(mapcar (lambda (e) `(defmacro ,(car e) (&rest body) `(when ,',(cdr e) ,@body ,',(cdr e)))) envs)))

(define-environments
  ((with-windows    . (eq system-type 'windows-nt))
   (with-classroom  . (string= user-login-name "lol"))
   (with-linux      . (eq system-type 'gnu/linux))
   (with-linux-g    . (and (eq system-type 'gnu/linux) (display-graphic-p)))
   (with-linux-vps  . (string= (system-name) "remote"))
   (with-graphic    . (display-graphic-p))))



;;;
;;; load-path and theme-path
;;;
(dolist (dir (directory-files "~/.emacs.d/ext" t))
  (if (and (not (eq (file-name-extension dir) ""))
           (file-directory-p dir))
      (add-to-list 'load-path dir)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/ext/themes")



;;;
;;; load modules
;;;
(require 'bm)
(require 'cust)
(require 'server)
(require 'imutil)
(require 'imokeys)
(require 'immor)
(require 'imface)
(with-windows (im/start-server))
(require 'imsilly)




(provide 'imfine)

;;; file ends here
