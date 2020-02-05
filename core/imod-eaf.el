;;; imod-eaf.el --- Application Framework -*- lexical-binding: t -*-

;;; Code:

(x eaf
   :ref ("emacs-eaf/emacs-application-framework")
   :commands (eaf-open eaf-open-browser eaf-open-browser-with-history)
   :init
   (setq eaf-kill-process-after-last-buffer-closed nil)
   (setq eaf-start-python-process-when-require t)
   :defer-config
   ;; load modules
   (let ((ms '(eaf eaf-browser eaf-pdf-viewer eaf-rss-reader
                   eaf-music-player eaf-video-player eaf-image-viewer
                   eaf-file-sender eaf-airshare eaf-file-manager
                   eaf-markdown-previewer eaf-org-previewer)))
     (dolist (m ms) (require m)))
   ;; eaf-open advice
   (defun:around find-file$eaf (orig-fn file &rest args)
     (let ((fn (if (commandp 'eaf-open) 'eaf-open orig-fn)))
       (pcase (file-name-extension file)
         ("pdf"  (apply fn file nil))
         ("epub" (apply fn file nil))
         (_      (apply orig-fn file args)))))
   ;; browser
   (setq browse-url-browser-function 'eaf-open-browser)
   (defalias 'browse-web #'eaf-open-browser)
   (setq eaf-browser-default-zoom 1.1)
   (setq eaf-browser-enable-adblocker t)
   (setq eaf-browser-default-search-engine "google")
   (setq eaf-browser-blank-page-url "https://www.google.com"))

(provide 'imod-eaf)

;;; imod-eaf.el ends here
