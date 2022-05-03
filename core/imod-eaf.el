;;; imod-eaf.el --- Application Framework -*- lexical-binding: t -*-

;;; Code:

(x eaf
   :ref ("emacs-eaf/emacs-application-framework")
   :commands (eaf-open eaf-open-browser eaf-open-browser-with-history)
   :init
   (setq eaf-kill-process-after-last-buffer-closed nil)
   (setq eaf-start-python-process-when-require t)
   :defer-config
   (let ((ms '(eaf eaf-browser eaf-pdf-viewer eaf-rss-reader
                   eaf-music-player eaf-video-player eaf-image-viewer
                   eaf-file-sender eaf-file-manager eaf-airshare
                   eaf-markdown-previewer eaf-org-previewer
                   eaf-git)))
     (dolist (m ms) (require m)))

   (setq browse-url-browser-function 'eaf-open-browser)
   (defalias 'browse-web #'eaf-open-browser)
   (setq eaf-browser-default-zoom 1.1)
   (setq eaf-browser-enable-adblocker t)
   (setq eaf-browser-default-search-engine "google")
   (setq eaf-browser-blank-page-url "https://www.google.com")

   (advice-remove #'find-file #'eaf--find-file-advisor)
   (advice-remove #'dired-find-file #'eaf--dired-find-file-advisor)
   (advice-remove #'dired-find-alternate-file #'eaf--dired-find-file-advisor)

   (defun:around find-file$eaf-open-smart (orig-fn file &rest args)
     (let ((fn (if (commandp 'eaf-open) 'eaf-open orig-fn)))
       (pcase (file-name-extension file)
         ((or "pdf" "pub")
          (when (or (and (fboundp 'dupan-file-p) (dupan-file-p file))
                    (and (fboundp 'dropbox-file-p) (dropbox-file-p file)))
            (setq file (file-local-copy file)))
          (apply fn file nil))
         (_ (apply orig-fn file args))))))

(provide 'imod-eaf)

;;; imod-eaf.el ends here
