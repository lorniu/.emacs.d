;;; imod-apps.el --- Applications/Externals -*- lexical-binding: t -*-

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



(x kubernetes
   :ref "kubernetes-el/kubernetes-el"
   :commands (kubernetes-overview)
   :init
   (setq kubernetes-poll-frequency 3600
         kubernetes-redraw-frequency 3600))



(x gimp
   :commands (connect-gimp gimp-mode))

(x mpv
   :commands (mpv-extra-options)
   :defer-config
   (defun mpv-extra-options ()
     (interactive)
     (let ((s (read-from-minibuffer "MPV Options: "
                                    (mapconcat #'identity mpv-default-options " "))))
       (setq mpv-default-options
             (if (string-equal "" s) nil
               (split-string s " ")))
       (message "%S" mpv-default-options))))

(x emms
   "Play music in emacs.
   "
   "First, install a player:
   "
   ": brew install mpv/vls...
   "
   :commands emms
   :init
   (setq emms-show-formatoo "Playing: %s")
   (setq emms-source-file-default-directory "~/playing/")
   (setq emms-directory (locc "emms"))
   (make-directory emms-directory t)
   :defer-config
   (require 'emms-setup)
   (emms-all)
   (emms-default-players)
   ;; keys
   (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
   (define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
   (define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
   (define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
   (define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
   (define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))
   (define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60))))

(provide 'imod-apps)

;;; imod-apps.el ends here
