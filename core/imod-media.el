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
  (setq emms-show-format "Playing: %s"
        emms-playlist-buffer-name "*EMMS*"
        emms-directory (locc "emms")
        emms-source-file-default-directory "~/Music/"
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal)
  :config
  (make-directory emms-directory t)
  (emms-all)
  (emms-cache -1)
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
