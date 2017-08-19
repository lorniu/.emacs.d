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
;;; customed main mode
;;;
(defgroup mmm nil "customed main mode" :group 'local :version 0.01)

(defcustom mmm/modes-to-load
  '(ivy-mode global-origami-mode beacon-mode  show-paren-mode
             global-anzu-mode yas-global-mode
             ;;global-hl-line-mode
             global-auto-revert-mode projectile-mode)
  "the modes should be enabled"
  :group 'mmm :type '(repeat symbol))

(defcustom mmm/list-to-dim
  '((yas-minor-mode " Y")
    ivy-mode auto-revert-mode abbrev-mode beacon-mode beacon-mode projectile-mode company-mode anzu-mode hs-minor-mode)
  "dim lighter of minor modes"
  :group 'mmm :type '(repeat (choice symbol (list symbol string))))

(defcustom mmm/keywords-to-add
  '((org-mode-hook
     ("\\<\\(FIXME\\|NOTE\\|AIA\\):" 1  'font-lock-warning-face prepend)
     ("\\\\\\\\$"                    0  'hi-org-break)))
  "define keywords for current mode"
  :group 'mmm
  :type '(alist :key-type symbol
                :value-type (repeat (list string integer symbol))))

(defmacro mmm/load-modes (&optional stat)
  "load/toggle these modes"
  (declare (indent defun))
  `(progn ,@(mapcar (lambda (mode) (list mode (or stat 1))) mmm/modes-to-load)))

(defmacro mmm/diminish (&optional stat)
  "`diminish-mode' helper.\n\nUsage:\n\n (mmm/diminish a b (c \"x\"))"
  `(progn
     (defun refresh-diminish () (interactive)
            ,@(mapcar (lambda (m)
                        (if (listp m) `(diminish ',(car m) ,(cadr m)) `(diminish ',m)))
                      mmm/list-to-dim))
     (if (and ,stat (integerp ,stat) (< ,stat 1))
         (remove-hook 'find-file-hook 'refresh-diminish)
       (add-hook 'find-file-hook 'refresh-diminish))))

(defmacro mmm/add-keywords ()
  "add face to words for any mode"
  `(progn
     ,@(mapcar (lambda (item)
                 `(add-hook ',(car item) (lambda () (font-lock-add-keywords nil ',(cdr item)))))
               mmm/keywords-to-add)))

(defvar mmm/keymap (make-sparse-keymap))

(define-minor-mode mmm/mode
  "The Main Mode of This Configuration"
  :group 'mmm :global t :keymap mmm/keymap
  (if mmm/mode
      (progn (mmm/load-modes)
             (mmm/diminish)
             (mmm/add-keywords))
    (mmm/load-modes -1)
    (mmm/diminish -1)))




;;;
;;; load modules
;;;
(require 'cl)
(require 'server)
(require 'cust)
(require 'imutil)
(require 'immor)
(require 'imokeys)
(require 'imface)
(mmm/mode 10086)                 ;; load main mode
(with-windows (im/start-server)) ;; start server
(require 'imsilly)




(provide 'imfine)

;;; file ends here
