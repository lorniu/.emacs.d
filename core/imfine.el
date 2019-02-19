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
     (env-g             . (display-graphic-p))
     (env-ng            . (not (display-graphic-p)))
     (env-linux         . (eq system-type 'gnu/linux))
     (env-linux-g       . (and (env-linux) (env-g)))
     (env-linux-ng      . (and (env-linux) (env-ng)))
     (env-linux-vps     . (string= (system-name) "remote"))
     (env-macos         . (eq system-type 'darwin))))

;;; Load-Path/Theme-Path

  (dolist (dir (directory-files "~/.emacs.d/extra" t))
    (if (and (not (eq (file-name-extension dir) "")) (file-directory-p dir))
        (add-to-list 'load-path dir)))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/extra/themes")

;;; Hook for special endpoint

  (let ((pri_file (format "~/.emacs.d/init_%s.el" (system-name))))
    (setq custom-file pri_file)
    (when (file-exists-p pri_file) (load pri_file t t)))

;;; Modules

  (require 'cust)
  (require 'imface)
  (require 'imoox)
  (require 'immor)
  (require 'imnet)
  (require 'imsilly)
  (require 'imkeys)
  (require 'imkmacro)
  (env-windows (im/start-server)))

(provide 'imfine)

;;; imfine.el ends here
