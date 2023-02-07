;;; imod-tools.el --- Tools/Externals -*- lexical-binding: t -*-

;;; Code:

(x dupan
   :init
   (require 'dupan nil t))

(x dropbox
   :ref ("https://www.dropbox.com/home"
         "https://www.dropbox.com/developers/apps"
         "https://www.dropbox.com/developers/documentation")
   :init
   (require 'dropbox nil t))



(x go-translate/i
   :ref "lorniu/go-translate"
   :commands (gts-do-translate im/translate-buf im/translate-pop im/translate-buf-prompt)

   :init
   ;;(define-key gts-prompt-for-translate-keymap [f5] #'exit-minibuffer)
   (setq gts-translate-list '(("en" "zh")))

   :config
   (defvar im:engines-1 (list (gts-bing-engine) (gts-google-engine :parser (gts-google-summary-parser)) (gts-google-rpc-engine)))
   (defvar im:engines-2 (list (gts-bing-engine) (gts-google-engine :parser (gts-google-summary-parser)) (gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))))
   (defvar im:translator-1 (gts-translator :picker (gts-noprompt-picker) :engines im:engines-1 :render (gts-buffer-render)))
   (defvar im:translator-2 (gts-translator :picker (gts-prompt-picker) :engines im:engines-1 :render (gts-buffer-render)))
   (defvar im:translator-3 (gts-translator :picker (gts-noprompt-picker) :engines im:engines-2 :render (gts-posframe-pop-render)))

   (defun im/translate-buf ()
     (interactive)
     (gts-translate im:translator-1))

   (defun im/translate-buf-prompt ()
     (interactive)
     (gts-translate im:translator-2))

   (defun im/translate-pop ()
     (interactive)
     (gts-translate im:translator-3)))



(x aria2
   "aria2c client in emacs."
   :ref ("repo/download: aria2/aria2"
         "usage: https://aria2c.com/usage.html"
         "aria2.el: https://gitlab.com/ukaszg/aria2")
   :commands (aria2 aria2-add-uris aria2-add-file))

(x kubernetes
   :ref "kubernetes-el/kubernetes-el"
   :commands (kubernetes-overview)
   :init
   (setq kubernetes-poll-frequency 3600
         kubernetes-redraw-frequency 3600))

(provide 'imod-tools)

;;; imod-tools.el ends here
