;;; -*- lexical-binding: t -*-

;;; Code:

(defcustom ic/custom-file-directory (loco "share/emacs/inits/")
  "Directory of custom-file."
  :group 'imfine
  :type 'file)

(defcustom ic/custom-file-file-name
  (format "init-%s@%s.el"
          (string-join (reverse (split-string (downcase (system-name)) "\\.")) ".")
          (user-real-login-name))
  "File name of custom-file."
  :group 'imfine
  :type 'string)

(unless (and (boundp 'custom-file) custom-file)
  (makunbound 'custom-file)) ; Remove it when not config before, then defvar below will work.

(defvar custom-file
  (if (file-exists-p ic/custom-file-directory)
      (expand-file-name ic/custom-file-file-name ic/custom-file-directory)
    (loce ic/custom-file-file-name))
  "Change this in 'init-aux.el' if you want.")

(unless (file-exists-p custom-file)
  (with-temp-file custom-file (insert (format ";; %s" (system-name)))))

(message "[CUST] %s" custom-file)
(load custom-file nil t nil 'must-suffix)
