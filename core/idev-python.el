;;; idev-python.el --- Python -*- lexical-binding: t -*-

;; Plans:
;;
;; 1) elpy/anaconda
;;
;;   - elpy is out of the box, use jedi as backend
;;   - pip install jedi importmagic install flake8 autopep8 # [opt]
;;
;; 2) eglot + jedi/pyls/mspyls/pyright
;;
;;
;;   pyright if you want good LSP, pylsp if you hate Microsoft or NodeJS
;;   pylance is good, but it only allowed used in vscode
;;
;;   - sudo npm install -g pyright (or pacman -S pyright)
;;   - pip install PyQt5-stubs # for completion
;;
;; Recommand: eglot + pyright
;;

;;; Code:

(x python
   :ref ("pyright/pylance: https://github.com/microsoft/pyright")
   :interpreter ("python" . python-mode)

   :init
   (setq python-indent-guess-indent-offset t)
   (setq python-indent-guess-indent-offset-verbose nil)

   :config
   (when (executable-find "python")
     (when IS-WIN
       (setq python-shell-completion-native-enable nil)
       (add-hook 'inferior-python-mode-hook 'im/local-encoding)))
   (defun:hook python-mode-hook ()))

(defun:around python-hideshow-forward-sexp-function//keep-only-one-line (fn &rest args)
  (let ((pt (apply fn args)))
    (goto-char pt)
    (skip-chars-forward " \n\t")
    (re-search-backward "\n\n" nil t)
    (when (< (point) pt) (goto-char pt))
    (point-marker)))

(provide 'idev-python)

;;; idev-python.el ends here
