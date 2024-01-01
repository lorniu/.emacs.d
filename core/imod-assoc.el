;;; -*- lexical-binding: t -*-

;;; Code:

(setq auto-mode-alist
      (append '(("\\.class\\'"           . class-mode)
                ("\\.scm\\'"             . scheme-mode)
                ("\\.\\(ba\\)?sh\\'"     . sh-mode)
                ("\\.xaml\\'"            . nxml-mode)
                ("\\.\\(ini\\|inf\\|nmconnection\\)\\'" . conf-mode))
              auto-mode-alist))

(setq major-mode-remap-alist major-mode-remap-alist)

(setq magic-mode-alist magic-mode-alist)

(setq jka-compr-compression-info-list
      (prog1 ;; for .rar, you should install `unarchiver'
          (append `(["\\.plist$"
                     "converting text XML to binary plist" "plutil" ("-convert" "binary1" "-o" "-" "-")
                     "converting binary plist to text XML" "plutil" ("-convert" "xml1" "-o" "-" "-")
                     nil nil "bplist"])
                  jka-compr-compression-info-list)
        (jka-compr-update)))
