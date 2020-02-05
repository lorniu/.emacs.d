;;; icod-lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

;;  - 20180111, Use Tide-Mode to Autocomplete instead of TERN.
;;  - 20181120, Servers as Elnode is more powerful but too old. Simpled-Httpd is simple and enough.
;;  - 20181120, Use Livereload replace Impatient! Websocket has a better experience than iframe.
;;  - 20191026, try lsp, still tooooo slow! remove js2, web-mode is better.

;;; Code:

(defreference lsp
  "emacs-lsp/lsp-mode"
  "joaotavora/eglot"
  "Specification: https://microsoft.github.io/language-server-protocol/specifications/specification-current/")



(x eglot
   :init
   (defmacro eglot-set-server (mode bin &rest args)
     `(with-eval-after-load 'eglot
        (when (executable-find ,bin)
          (add-to-list 'eglot-server-programs '(,mode . (,bin ,@args)))
          (cons ',mode ',bin)))))

(prog1 :event-log-switcher
  (defvar eglot-log-event-p nil)
  (defun:around jsonrpc--log-event$toggle-event-log (f &rest args)
    (when (and eglot-log-event-p
               (ignore-errors (eq (type-of (car args)) 'eglot-lsp-server)))
      (apply f args)))
  (defun im/toggle-eglot-event-log ()
    (interactive)
    (message "EGLOG event log is current: %s"
             (if (setq eglot-log-event-p (not eglot-log-event-p))
                 "ON"
               "OFF"))))



(x lsp-mode
   :init
   (setq lsp-keymap-prefix "C-c l")
   (setq lsp-auto-guess-root t)
   (setq lsp-diagnostic-package :auto)
   (setq lsp-completion-provider :none)

   (defun lsp-and-future ()
     (interactive)
     (lsp)
     (add-hook (intern (format "%s-hook" (symbol-name major-mode))) 'lsp))
   (defun lsp-and-future-cancel ()
     (interactive)
     (lsp-mode -1)
     (remove-hook (intern (format "%s-hook" (symbol-name major-mode))) 'lsp))

   :defer-config
   (x lsp-ui
      :init
      (setq lsp-ui-doc-enable t)
      (setq lsp-ui-sideline-enable nil)
      (setq lsp-ui-flycheck-enable t)
      (setq lsp-ui-doc-use-webkit t)))

(provide 'icod-lsp)

;;; icod-lsp.el ends here
