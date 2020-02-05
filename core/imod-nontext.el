;;; imod-nontext.el --- Image/Audio/Video/PDF.. -*- lexical-binding: t -*-

;;; Code:

(x image-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))
   :defer-config
   (define-key image-mode-map "c" 'im/yank-current-buffer-name))

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

(x pdf-tools
   :ref "vedang/pdf-tools"
   :init
   (defun:hook find-file-hook/pdf-tools ()
     (and (string-equal (file-name-extension buffer-file-name) "pdf")
          (not (featurep 'pdf-tools))
          (require 'pdf-tools)))
   :defer-config
   ;; fire when epdfinfo exists
   (when (executable-find pdf-info-epdfinfo-program)
     (add-to-list 'magic-mode-alist (cons "%PDF" 'pdf-view-mode))
     (pdf-tools-install :no-query))
   ;; variable
   (setq-default pdf-view-display-size 'fit-page)
   (setq pdf-annot-activate-created-annotations t)
   (setq pdf-view-resize-factor 1.1)
   ;; keymap
   (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
   (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
   (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
   (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
   (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
   (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save))

(provide 'imod-nontext)

;;; imod-nontext.el ends here
