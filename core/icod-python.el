;;; icod-python.el --- Python -*- lexical-binding: t -*-

;; Plans:
;;
;; 1) elpy/anaconda
;;
;;   - elpy is out of the box, use jedi as backend
;;   - pip install jedi importmagic install flake8 autopep8 # [opt]
;;
;; 2) lsp/eglot + jedi/pyls/mspyls/pyright
;;
;;   - pyright is from MS, used in vscode
;;   - sudo npm install -g pyright
;;   - pip install PyQt5-stubs # for completion
;;
;; Recommand: eglot + pyright

;;; Code:

(x python
   :ref ("pyright/pylance: https://github.com/microsoft/pyright")
   :interpreter ("python" . python-mode)

   :init
   (setq python-indent-guess-indent-offset t)
   (setq python-indent-guess-indent-offset-verbose nil)

   :defer-config
   (when (executable-find "python")
     (when IS-WIN
       (setq python-shell-completion-native-enable nil)
       (add-hook 'inferior-python-mode-hook 'im/local-encoding))
     (eglot-set-server python-mode "pyright-langserver" "--stdio"))

   (defun:hook python-mode-hook ()
     (setq outline-prefer-p t)))

(provide 'icod-python)

;;; icod-python.el ends here
