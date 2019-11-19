;;; cust.el --- Basic Configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq user-full-name "imfine")
(setq user-mail-address "lorniu@gmail.com")
(setq default-directory "~/")


;;; System-Variables

(setq inhibit-startup-message t
      gnus-inhibit-startup-message t
      track-eol t
      visible-bell nil
      ring-bell-function 'ignore
      confirm-kill-processes nil
      disabled-command-function nil

      _CACHE_ "~/.cache/emacs/"
      bbdb-file (concat _CACHE_ "_bbdb")
      diary-file (concat _CACHE_ "_diary")
      abbrev-file-name (concat _CACHE_ "_abbrevs")
      bookmark-default-file (concat _CACHE_ "_bookmark")
      savehist-file (concat _CACHE_ "_history")
      url-configuration-directory (concat _CACHE_ "url")

      auto-save-interval 0
      auto-save-list-file-prefix nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      scroll-step 1
      scroll-margin 0
      hscroll-step 1
      hscroll-margin 1
      scroll-conservatively 101

      resize-mini-windows t
      enable-recursive-minibuffers t
      column-number-mode 1
      fringes-outside-margins t

      enable-local-eval t
      enable-local-variables :all

      kill-ring-max 250
      mark-ring-max 10
      select-enable-clipboard t
      select-enable-primary nil
      delete-by-moving-to-trash t
      trash-directory "~/.trash"
      mouse-autoselect-window -0.05

      help-window-select t
      man-notify-method 'pushy
      woman-use-own-frame nil)

(setq-default tab-width 4
	          indent-tabs-mode nil
	          truncate-lines t
              bidi-display-reordering nil)

(setenv "TZ" "PRC")
(setenv "LC_COLLATE" "C")

(fset 'yes-or-no-p 'y-or-n-p)


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
   (env-nix-vps       . (string= (system-name) "chaos"))
   (env-bsd           . (eq system-type 'berkeley-unix))
   (env-macos         . (eq system-type 'darwin))))


;;; Custom-Config

(setq custom-unlispify-tag-names nil)

(custom-set-variables)
(custom-set-faces)


(provide 'cust)

;;; cust.el ends here
