;;; imod-apps.el --- Applications -*- lexical-binding: t -*-

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

(provide 'imod-apps)

;;; imod-apps.el ends here
