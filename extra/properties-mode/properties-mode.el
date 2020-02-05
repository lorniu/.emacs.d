;;; properties-mode.el --- Major mode to edit Java properties file  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Iku Iwasa

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; URL: https://github.com/iquiw/properties-mode
;; Version: 2.0.0
;; Package-Requires: ((emacs "26"))
;; Keywords: conf java

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode to edit Java properties file.

;;; Code:

(defgroup properties nil
  "Major mode to edit Java properties file."
  :prefix "properties-"
  :group 'conf)

(defcustom properties-enable-auto-unicode-escape t
  "Whether to enable automatic conversion of unicode escape."
  :type 'boolean)

(defcustom properties-reference-language "en"
  "Language name to be used as reference for translation."
  :type 'string
  :initialize #'custom-initialize-default
  :set #'properties-change-reference-language)

(defcustom properties-unicode-escape-uppercase nil
  "Whether to use uppercase characters to escape unicode."
  :type 'boolean)

(defconst properties-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b C-d") #'properties-decode-buffer)
    (define-key map (kbd "C-c C-b C-e") #'properties-encode-buffer)
    (define-key map (kbd "C-c C-d") #'properties-decode-region)
    (define-key map (kbd "C-c C-e") #'properties-encode-region)
    (define-key map (kbd "C-c C-l") #'properties-change-reference-language)
    (define-key map (kbd "C-c C-v") #'properties-view-reference-file)
    map))

(defconst properties--langfile-regexp
  "\\(.+?\\)_\\([a-z]\\{2\\}\\(?:_[a-z]\\{2\\}\\)?\\)\\.\\(.+\\)\\'")

(defconst properties--separator-regexp
  "[[:blank:]]*[:=][[:blank:]]*")

(defconst properties--key-regexp
  (concat "^\\([^:=[:blank:]\n]+\\)" properties--separator-regexp))

(defvar-local properties--reference-file nil)

(defun properties-change-reference-language (_variable language &optional all)
  "Change reference language to LANGUAGE.
If ALL is non-nil or with a prefix argument, change reference language
in all `properties-mode' buffers."
  (interactive (list nil (read-string "Lanaguage: ") current-prefix-arg))
  (if all
      (progn
        (setq-default properties-reference-language language)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (eq major-mode 'properties-mode)
              (when (local-variable-p 'properties-reference-language)
                (kill-local-variable 'properties-reference-language))
              (setq properties--reference-file (properties--get-reference-name))))))
    (when (eq major-mode 'properties-mode)
      (setq-local properties-reference-language language)
      (setq properties--reference-file (properties--get-reference-name)))))

(defun properties-encode-buffer ()
  "Encode non-ASCII characters to unicode escape characters in the current buffer ."
  (interactive)
  (properties--encode-range (point-min) (point-max)))

(defun properties-encode-region (beg end)
  "Encode non-ASCII characters to unicode escape characters in the region between BEG and END."
  (interactive "r")
  (when (use-region-p)
    (properties--encode-range beg end)))

(defun properties--encode-range (beg end)
  "Encode non-ASCII characters to unicode escape characters between BEG and END."
  (save-excursion
    (let ((end-marker (make-marker)))
      (set-marker end-marker end)
      (goto-char beg)
      (while (and (< (point) end-marker)
                  (re-search-forward "[^[:ascii:][:cntrl:]]+" end-marker t))
        (let ((s (match-string-no-properties 0)))
          (delete-region (match-beginning 0) (match-end 0))
          (mapc
           (lambda (c)
             (insert (format (if properties-unicode-escape-uppercase
                                 "\\u%04X"
                               "\\u%04x")
                             c)))
           s)))
      (set-marker end-marker nil))))

(defun properties-decode-buffer ()
  "Decode unicode escape characters in the current buffer."
  (interactive)
  (properties--decode-range (point-min) (point-max)))

(defun properties-decode-region (beg end)
  "Decode unicode escape characters in the region between BEG and END."
  (interactive "r")
  (when (use-region-p)
    (properties--decode-range beg end)))

(defun properties--decode-range (beg end)
  "Decode unicode escape characters between BEG and END."
  (save-excursion
    (let ((end-marker (make-marker)))
      (set-marker end-marker end)
      (goto-char beg)
      (while (and (< (point) end-marker)
                  (re-search-forward "\\\\u\\([0-9a-f]\\{4\\}\\)" end-marker t))
        (let ((s (match-string-no-properties 1)))
          (delete-region (match-beginning 0) (match-end 0))
          (insert (char-to-string (string-to-number s 16)))))
      (set-marker end-marker nil))))

(defun properties-view-reference-file ()
  "Display reference file assosicated with the current buffer."
  (interactive)
  (when properties--reference-file
    (let ((key (properties--get-property-key))
          (buf (or (get-file-buffer properties--reference-file)
                   (find-file-noselect properties--reference-file))))
      (save-selected-window
        (switch-to-buffer-other-window buf t)
        (when key
          (goto-char (point-min))
          (re-search-forward
           (concat "^" (regexp-quote key) properties--separator-regexp)
           nil t)
          (forward-line 0))))))

(defun properties--buffer-modified-p ()
  "Check the current buffer is modified from corresponding file."
  (let ((file (buffer-file-name)))
    (or (not file)
        (not (string-equal (sha1 (current-buffer))
                           (with-temp-buffer
                             (insert-file-contents file)
                             (sha1 (current-buffer))))))))

(defun properties--find-reference-value ()
  "Find property value of same key at the current point in the reference file."
  (when (and properties--reference-file
             (not (equal properties--reference-file (buffer-file-name)))
             (file-regular-p properties--reference-file))
    (let ((key (properties--get-property-key))
          (lang properties-reference-language))
      (when key
        (with-current-buffer (find-file-noselect properties--reference-file)
          (format "%s: %s" lang (properties--find-value key)))))))

(defun properties--find-value (key)
  "Find property value of KEY in the current buffer.
Return nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((key-regexp (concat "^" (regexp-quote key) properties--separator-regexp)))
      (when (re-search-forward key-regexp nil t)
        (buffer-substring-no-properties
         (match-end 0)
         (line-end-position))))))

(defun properties--get-property-key ()
  "Get property key at the current line.
Return nil if not found."
  (save-excursion
    (forward-line 0)
    (when (looking-at properties--key-regexp)
      (match-string-no-properties 1))))

(defun properties--get-reference-name (&optional name)
  "Return reference properties file name for given NAME.
If NAME is omitted, value of function `buffer-file-name' is used.
Return nil if NAME does not have language part."
  (unless name
    (setq name (buffer-file-name)))
  (when (and name (string-match properties--langfile-regexp name))
    (unless (equal (match-string 2 name) properties-reference-language)
      (let ((ref-file (concat (match-string 1 name)
                              "_"
                              properties-reference-language
                              "."
                              (match-string 3 name))))
        ref-file))))

(defun properties--maybe-encode-buffer ()
  "Encode the current buffer to unicode escape characters according to user's choice."
  (if (y-or-n-p "Encode the buffer to unicode escape characters? ")
      (save-excursion
        (let ((modified (buffer-modified-p)))
          (properties-encode-buffer)
          (restore-buffer-modified-p (or modified
                                         (properties--buffer-modified-p)))))
    (restore-buffer-modified-p (properties--buffer-modified-p))))

(defun properties--revert-buffer (ignore-auto noconfirm)
  "Revert buffer with mode preserved.
IGNORE-AUTO and NOCONFIRM are passed to `revert-buffer'."
  (let (revert-buffer-function)
    (revert-buffer ignore-auto noconfirm t)
    (remove-hook 'change-major-mode-hook #'properties--maybe-encode-buffer t)
    (setq major-mode 'fundamental-mode)
    (properties-mode)))

(defun properties--save-buffer ()
  "Save properties mode buffer with encoded."
  (when properties-enable-auto-unicode-escape
    (restore-buffer-modified-p
     (if (buffer-modified-p)
         (let ((name (buffer-name))
               (file (buffer-file-name))
               (coding buffer-file-coding-system)
               (start (point-min))
               (end (point-max))
               modified
               modified-time)
           (with-temp-buffer
             (set-buffer-file-coding-system coding)
             (insert-buffer-substring name start end)
             (properties-encode-buffer)
             (let ((change-major-mode-with-file-name nil))
               (set-visited-file-name file t))
             (unwind-protect
                 (save-buffer)
               (setq modified (buffer-modified-p))
               (setq modified-time (visited-file-modtime))
               (set-visited-file-name nil)))
           (set-visited-file-modtime modified-time)
           (setq save-buffer-coding-system coding)
           modified)
       (message "(No changes need to be saved)")
       nil))
    t))

(define-derived-mode properties-mode conf-javaprop-mode "Props"
  "Major mode to edit Java properties file."
  (when properties-enable-auto-unicode-escape
    (let ((modified (buffer-modified-p)))
      (properties-decode-buffer)
      (restore-buffer-modified-p modified)))
  (setq properties--reference-file (properties--get-reference-name))
  (setq-local eldoc-documentation-function #'properties--find-reference-value)
  (setq-local revert-buffer-function #'properties--revert-buffer)
  (add-hook 'change-major-mode-hook #'properties--maybe-encode-buffer nil t)
  (add-hook 'write-contents-functions #'properties--save-buffer nil t))

(provide 'properties-mode)

;;; properties-mode.el ends here
