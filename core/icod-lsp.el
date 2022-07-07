;;; icod-lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;  - 20181120, Servers as Elnode is more powerful but too old. Simpled-Httpd is simple and enough.
;;  - 20181120, Use Livereload replace Impatient! Websocket has a better experience than iframe.
;;  - 20191026, try lsp, still tooooo slow! remove js2, web-mode is better.

;;; Code:

(defreference lsp
  "emacs-lsp/lsp-mode"
  "joaotavora/eglot"
  "manateelazycat/lsp-bridge"
  "Specification: https://microsoft.github.io/language-server-protocol/specifications/specification-current/")



(x eglot
   :init
   (setq eglot-events-buffer-size 0)
   ;;(setq eglot-stay-out-of `(flymake))
   :defer-config
   (require 'eglot-jdt)
   (unless (and IS-WIN (executable-find "omnisharp")) (require 'eglot-cls)) ; cls not working on windows, strange.
   (require 'eglot-fsac))

(defmacro eglot-set-server (mode bin &rest args)
  `(with-eval-after-load 'eglot
     (when (executable-find ,bin)
       (add-to-list 'eglot-server-programs '(,mode . (,bin ,@args)))
       (cons ',mode ',bin))))

(defun im/eglot-events-buffer-size ()
  (interactive)
  (require 'eglot)
  (let* ((cs (number-to-string eglot-events-buffer-size))
         (pt (format "Events buffer size (%s): " cs))
         (cands (cl-remove-duplicates `("0" "2000000" ,cs) :test #'string=))
         (cand (completing-read pt cands))
         (n (ignore-errors (string-to-number cand))))
    (if (or (null n) (string= cs cand))
        (user-error "Nothing changed.")
      (setq eglot-events-buffer-size n)
      (ignore-errors (eglot-reconnect (eglot-current-server)))
      (message "`eglot-events-buffer-size' changed to %s." n))))



(x lsp-mode
   :init
   (setq lsp-keymap-prefix "C-c l")
   (setq lsp-auto-guess-root t)
   (setq lsp-diagnostic-package :auto)
   (setq lsp-completion-provider :none)

   :defer-config
   (x lsp-ui
      :init
      (setq lsp-ui-doc-enable t)
      (setq lsp-ui-sideline-enable nil)
      (setq lsp-ui-flycheck-enable t)
      (setq lsp-ui-doc-use-webkit t)))

(defun lsp-and-future ()
  (interactive)
  (lsp)
  (add-hook (intern (format "%s-hook" (symbol-name major-mode))) 'lsp))

(defun lsp-and-future-cancel ()
  (interactive)
  (lsp-mode -1)
  (remove-hook (intern (format "%s-hook" (symbol-name major-mode))) 'lsp))



(x lsp-bridge
   :commands (lsp-bridge-mode global-lsp-bridge-mode)
   :defer-config
   (require 'lsp-bridge)
   (require 'lsp-bridge-jdtls))

(provide 'icod-lsp)

;;; icod-lsp.el ends here
