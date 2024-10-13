;;; imod-tools.el --- Tools/Externals -*- lexical-binding: t -*-

;;; Code:

(x dupan
   :commands (dupan-find dupan-browser))

(x dropbox
   :ref ("https://www.dropbox.com/home"
         "https://www.dropbox.com/developers/apps"
         "https://www.dropbox.com/developers/documentation")
   :commands (dropbox-find dropbox-browser))



(x go-translate
   :ref "lorniu/go-translate"
   :commands (gt-do-translate gt-do-setup))



(x plz
   :ref ("alphapapa/plz.el"))

(x request
   :init
   (setq request-storage-directory (locc "request")))

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



(x keycast :ref "tarsius/keycast")

(provide 'imod-tools)

;;; imod-tools.el ends here
