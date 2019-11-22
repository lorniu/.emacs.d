;;; implay.el --- Play around -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ic/feeds-org-file
  (let ((f (expand-file-name "000/favors.org" note-directory)))
    (if (file-exists-p f) f (locate-user-emacs-file "elfeeds.org")))
  "Feeds Used for `elfeed', an org file."
  :type 'string :group 'imfine)



;;; emms

(x emms
   "Play music in emacs.
   "
   "First, install a player:
   "
   ": brew install mpg123/mplayer/vls...
   "
   :commands emms
   :config
   (require 'emms-setup)

   (setq emms-directory (concat _CACHE_ "emms"))
   (setq emms-show-formatoo "Playing: %s")
   (setq emms-source-file-default-directory "~/music/")

   (emms-all)
   (emms-default-players))



;;; elfeed

(x elfeed-org/w
   "Read rss feeds."
   :init
   (setq elfeed-log-buffer-name " *elfeed-log*")
   (autoload 'elfeed "elfeed-org" nil t)

   :config
   (setq rmh-elfeed-org-files (list ic/feeds-org-file))
   (elfeed-org)

   (defun my-elfeed-show-mode-hook ()
     (pixel-scroll-mode t)
     (setq pixel-resolution-fine-flag t
           mouse-wheel-scroll-amount '(1)
           fast-but-imprecise-scrolling t
           jit-lock-defer-time 0
           mouse-wheel-progressive-speed nil)
     (setq line-spacing 3))

   (add-hook 'elfeed-search-update-hook 'im/set-buffer-mono-font)
   (add-hook 'elfeed-show-mode-hook 'my-elfeed-show-mode-hook))


(provide 'implay)

;;; implay.el ends here
