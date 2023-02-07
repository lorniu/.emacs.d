;;; -*- lexical-binding: t -*-

;;; Code:

(setq tramp-verbose 3
      tramp-persistency-file-name (locc "tramp")
      tramp-allow-unsafe-temporary-files t
      remote-file-name-inhibit-cache nil)

(xzz tramp
  :after vc
  :config
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))
