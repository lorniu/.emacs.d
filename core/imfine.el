;;; imfine.el --- This is my personal emacs configuration. -*- lexical-binding: t -*-

;; Copyright 2008 by imfine. All rights reserved.

;; Author: lorniu@gmail.com
;; Version: 0.2
;; License: BSD

;;; Commentary:

;;; Code:

(defgroup imfine nil "Private Variables" :group 'emacs)

(setq debug-on-error nil)



(let ((inhibit-message t)
      (file-name-handler-alist nil)
      (gc-cons-threshold (* 64 1024 1024)))

  ;; modules
  (require 'bm)
  (require 'util)
  (require 'cist)
  (require 'cust)
  (require 'patches)

  (require 'ifavor)
  (require 'imface)

  ;; org-mode/etc
  (require 'imoox)
  (require 'imooy)
  (require 'imtex)

  ;; modes/etc
  (require 'immor)
  (require 'imcoding)
  (require 'imnet)
  (require 'imdb)
  (require 'implay)

  ;; commands/keys
  (require 'ic-keys)
  (require 'ic-hydra)
  (require 'ic-kmacro)

  ;; others
  (require 'imsilly)
  (require 'misc)

  ;; daemon server
  (require 'server)
  (ignore-errors
    (setq server-auth-dir "~/.emacs.d/.cache/server/")
    (delete-file (concat server-auth-dir "server"))
    (unless (server-running-p) (server-start))))


(provide 'imfine)

;;; imfine.el ends here
