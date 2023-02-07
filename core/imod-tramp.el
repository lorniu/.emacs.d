;;; -*- lexical-binding: t -*-

;;; Code:

(setq tramp-verbose 3
      tramp-persistency-file-name (locc "tramp")
      remote-file-name-inhibit-cache nil
      vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
