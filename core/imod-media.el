;;; imod-media.el --- Media -*- lexical-binding: t -*-

;;; Code:

(x emms
   "Play music in emacs.
   "
   "Make sure some player in your PATH (brew install mpv/vls...)
   "
   "To edit tags, install mid3v2 first (pacman -S python-mutagen)
   "
   :commands emms
   :init
   (setq emms-show-formatoo "Playing: %s")
   (setq emms-source-file-default-directory (cl-find-if #'file-exists-p '("~/playing/" "~/music" "~/Downloads/music" "~/Downloads/")))
   (setq emms-directory (locc "emms"))
   (make-directory emms-directory t)
   :config
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

(x mpv
   :commands (mpv-extra-options)
   :config
   (defun mpv-extra-options ()
     (interactive)
     (let ((s (read-from-minibuffer "MPV Options: "
                                    (mapconcat #'identity mpv-default-options " "))))
       (setq mpv-default-options
             (if (string-equal "" s) nil
               (split-string s " ")))
       (message "%S" mpv-default-options))))

(provide 'imod-media)

;;; imod-media.el ends here
