;;; figlet.el --- Annoy people with big, ascii art text -*- lexical-binding: t -*-

;; Copyright (C) 2014 Aurelien Aptel <aurelien.aptel@gmail.com>
;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.5
;; Package-Version: 20160218.2237
;; Package-Commit: 19a38783a90e151faf047ff233a21a729db0cea9

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this feature simple eval (require 'figlet) type M-x figlet
;; and you will be asked for a string. If you use a prefix (C-u M-x
;; figlet) then you will be asked for a font.

;; Have a look at `figlet-comment', `figlet-figletify-region' and
;; `figlet-figletify-region-comment'.

;; Warning, leaving large ascii art text in your teams codebase might
;; cause an outbreak of physical violence.

(defvar figlet-fonts '())

(defvar figlet-default-font "standard"
  "Default font to use when none is supplied.")

(defvar figlet-options '()
  "List of options for the figlet call.
This is a list of strings, e.g. '(\"-k\").")

(defvar figlet-font-directory nil
  "Figlet default font directory")

(defun figlet-get-font-dir ()
  "Return default font directory."
  (or figlet-font-directory
      (setq figlet-font-directory
            (let ((s (shell-command-to-string "figlet -I2")))
              (substring s 0 (1- (length s)))))))

(defun figlet-get-font-list ()
  "Get a list of figlet fonts."
  (or figlet-fonts
      (setq figlet-fonts
            (mapcar (lambda (f)
                      (replace-regexp-in-string "\\.flf$" "" f))
                    (directory-files (figlet-get-font-dir) nil "^[^.].+\\.flf$")))))

;;;###autoload
(defun figlet (&optional string font commentp)
  "Pass a string through figlet and insert the output at point. Use a prefix arg to make result commented."
  (interactive (if (executable-find "figlet")
                   (list (let ((s (if (use-region-p)
                                      (buffer-substring (region-beginning) (region-end))
                                    (read-string "String to fig: " nil 'figlet-history))))
                           (if (> (length s) 0) s (user-error "String is required")))
                         (completing-read "Font: "
                                          (figlet-get-font-list) nil t nil nil figlet-default-font)
                         current-prefix-arg)
                 (user-error "Please install 'figlet' first")))
  (let ((str (with-temp-buffer
               (apply #'call-process (append '("figlet" nil t t) figlet-options `("-f" ,(or font figlet-default-font) ,string)))
               (goto-char (point-min))
               (re-search-forward "^." nil t)
               (delete-region (point-min) (point-at-bol))
               (re-search-forward "^[[:blank:]]*$" nil t)
               (delete-region (point) (point-max))
               (delete-trailing-whitespace)
               (buffer-substring (point-min) (point-max)))))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (if commentp
        (let ((point (point)))
          (insert str)
          (comment-region point (point)))
      (insert str))))

;;;###autoload
(defun figlet-samples (&optional text)
  "View an example of each font in a new buffer."
  (interactive (list (read-string "Sample string: ")))
  (with-current-buffer (get-buffer-create "*Figlet Samples*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (f (figlet-get-font-list))
        (insert (format "* %s\n\n" f))
        (figlet (if (> (length text) 0) text f) f)
        (insert "\n\n")))
    (special-mode)
    (outline-minor-mode 1)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(provide 'figlet)

;;; figlet.el ends here
