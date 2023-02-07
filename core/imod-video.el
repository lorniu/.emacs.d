;;; -*- lexical-binding: t -*-

;;; Code:

(xzz emms
  "Play music in emacs.
   "
  "Make sure some player in your PATH (brew install mpv/vls...)
   "
  "To edit tags, install mid3v2 first (pacman -S python-mutagen)
   "
  :commands emms
  :init
  (setq emms-show-formatoo "Playing: %s"
        emms-playlist-buffer-name " *EMMS*"
        emms-directory (locc "emms"))
  :config
  (make-directory emms-directory t)
  (emms-all)
  (emms-default-players)
  ;; keys
  (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
  (define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
  (define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
  (define-key emms-playlist-mode-map (kbd "<right>") (lambdi () (emms-seek +10)))
  (define-key emms-playlist-mode-map (kbd "<left>")  (lambdi () (emms-seek -10)))
  ;; Silence
  (defun:override emms-cache-restore//inhibit-loading-message ()
    (interactive)
    (load emms-cache-file t 'no-message t)
    (setq emms-cache-dirty nil)))

(xzz mpvi
  :ref "lorniu/mpvi"
  :config
  (setq mpvi-danmaku2ass (file-truename (loce "share/danmaku2ass.py")))
  :commands (mpvi-open mpvi-open-from-favors mpvi-seek mpvi-insert mpvi-clip mpvi-emms-add mpvi-org-link-init))
