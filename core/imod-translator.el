;;; imod-translator.el --- Translation -*- lexical-binding: t -*-

;;; Code:

(x go-translate/i
   :ref "lorniu/go-translate"
   :commands (gts-do-translate im/translate-buf im/translate-pop im/translate-buf-prompt)

   :init
   ;;(define-key gts-prompt-for-translate-keymap [f5] #'exit-minibuffer)
   (setq gts-translate-list '(("en" "zh")))

   :defer-config
   (defvar my-engines-1 (list (gts-bing-engine) (gts-google-engine :parser (gts-google-summary-parser)) (gts-google-rpc-engine)))
   (defvar my-engines-2 (list (gts-bing-engine) (gts-google-engine :parser (gts-google-summary-parser)) (gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))))
   (defvar my-translator-1 (gts-translator :picker (gts-noprompt-picker) :engines my-engines-1 :render (gts-buffer-render)))
   (defvar my-translator-2 (gts-translator :picker (gts-prompt-picker) :engines my-engines-1 :render (gts-buffer-render)))
   (defvar my-translator-3 (gts-translator :picker (gts-noprompt-picker) :engines my-engines-2 :render (gts-posframe-pop-render)))

   (defun im/translate-buf ()
     (interactive)
     (gts-translate my-translator-1))

   (defun im/translate-buf-prompt ()
     (interactive)
     (gts-translate my-translator-2))

   (defun im/translate-pop ()
     (interactive)
     (gts-translate my-translator-3)))

(provide 'imod-translator)

;;; imod-translator.el ends here
