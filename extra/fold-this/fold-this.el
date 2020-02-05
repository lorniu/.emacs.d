;;; fold-this.el --- Just fold this region please -*- lexical-binding: t -*-

;; Copyright (C) 2012-2013 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.4.4
;; Package-Version: 20191107.1816
;; Package-Commit: c3912c738cf0515f65162479c55999e2992afce5
;; Keywords: convenience
;; Homepage: https://github.com/magnars/fold-this.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Just fold the active region, please.
;;
;; ## How it works
;;
;; The command `fold-this` visually replaces the current region with `[[…]]`.
;; If you move point into the ellipsis and press enter or `C-g` it is unfolded.
;;
;; You can unfold everything with `fold-this-unfold-all`.
;;
;; You can fold all instances of the text in the region with `fold-this-all`.
;;
;;; Code:
(require 'thingatpt)

(defgroup fold-this nil
  "Just fold this region please."
  :prefix "fold-this-"
  :group 'languages)

(defvar fold-this--overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'fold-this-unfold)
    (define-key map (kbd "C-g") 'fold-this-unfold)
    (define-key map (kbd "<mouse-1>") 'fold-this-unfold)
    map)
  "Keymap for `fold-this' but only on the overlay.")

(defcustom fold-this-persistent-folds nil
  "Should folds survive buffer kills and Emacs sessions.
Non-nil means that folds should survive buffers killing and Emacs
sessions. "
  :group 'fold-this
  :type 'boolean)

(defcustom fold-this-persistent-folds-file (locate-user-emacs-file ".fold-this.el")
  "A file to save persistent fold info to."
  :group 'fold-this
  :type 'file)

(defcustom fold-this-persistent-folded-file-limit 30
  "A max number of files for which folds persist.  Nil for no limit."
  :group 'fold-this
  :type '(choice (integer :tag "Entries" :value 1)
                 (const :tag "No Limit" nil)))

;;; {{{ My Improvements 2021-12-17

(defun fold-this--%correct-region (beg end)
  "Only support whole line."
  (cl-destructuring-bind (beg . end)
      (if (>= end beg) (cons beg end) (cons end beg))
    (save-excursion
      (goto-char beg)
      (if (eq (char-before) 12) (forward-char))
      (setq beg (point))
      (goto-char end)
      (cond ((eq (char-before) 12) (backward-char 2))
            ((and (eq (char-before) 10) (eq (char-after) 10)) (backward-char 1)))
      (setq end (point)))
    (if (> (count-lines beg end) 1)
        (save-excursion
          (save-restriction
            (let ((beg* (progn (goto-char beg)
                               (line-beginning-position)))
                  (end* (progn (goto-char end)
                               (line-end-position))))
              (cons beg* end*))))
      (cons nil nil))))

(defun fold-this--%get-header (beg end)
  "Extract folding header from region between BEG and END in BUFFER."
  (let ((info (format "<%d lines>" (count-lines beg end))))
    (save-excursion
      (goto-char beg)
      (cons (concat (truncate-string-to-width
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                     (- (min 100 (- (window-width) 5)) (length info))
                     nil 32 "…")
                    "    ")
            info))))

(defface fold-this-%overlay '((((background dark)) :background "dim grey" :foreground "#e0e0e0")
                              (((background light)) :background "cornsilk"))
  "Face used to highlight the fold overlay.")

(defface fold-this-%overlay-end '((((background dark)) :inherit fold-this-%overlay :foreground "green")
                                  (((background light)) :inherit fold-this-%overlay :foreground "red"))
  "Face used to highlight the fold overlay-end.")

(defface fold-this-%mouse-face '((t (:inherit fold-this-%overlay :weight bold)))
  "Face to use when mouse hovers over folded text.")

(defface fold-this-%fold-fringe '((t (:inherit font-lock-function-name-face)))
  "Face used to indicate folded text on fringe.")

;;; }}}

;;;###autoload
(defun fold-this (beg end &optional fold-header)
  "Fold the region between BEG and END."
  (interactive "r")
  (let ((h (fold-this--%correct-region beg end)))
    (setq beg (car h) end (cdr h)))
  (when (and beg end)
    (let* ((o (make-overlay beg end nil t nil))
           (header (fold-this--%get-header beg end))
           (hd (concat (propertize (car header) 'face 'fold-this-%overlay)
                       (propertize (cdr header) 'face 'fold-this-%overlay-end)
                       (propertize " " 'face 'fold-this-%overlay)))
           (fr (propertize "…" 'display (list 'left-fringe 'empty-line 'fold-this-%fold-fringe))))
      (overlay-put o 'type 'fold-this)
      (overlay-put o 'keymap fold-this--overlay-keymap)

      (overlay-put o 'display hd)
      (overlay-put o 'before-string fr)

      (overlay-put o 'pointer 'hand)
      (overlay-put o 'mouse-face 'fold-this-%mouse-face)
      (overlay-put o 'help-echo "Click to unfold the text")
      ;; isearch
      (overlay-put o 'isearch-open-invisible-temporary
                   (lambda (ov action)
                     (if action
                         (progn
                           (overlay-put ov 'display hd)
                           (overlay-put ov 'invisible t))
                       (progn
                         (overlay-put ov 'display nil)
                         (overlay-put ov 'invisible nil)))))
      (overlay-put o 'isearch-open-invisible 'fold-this--delete-my-overlay)
      (overlay-put o 'modification-hooks '(fold-this--delete-my-overlay)))
    (deactivate-mark)
    (goto-char beg)))

(defun fold-this-unfold (p)
  "Unfold at point."
  (interactive "d")
  (mapc 'fold-this--delete-my-overlay (overlays-at p)))

(defun fold-this-unfold-all ()
  "Unfold all overlays in current buffer.
If narrowing is active, only in it."
  (interactive)
  (mapc 'fold-this--delete-my-overlay
        (overlays-in (point-min) (point-max))))

(defun fold-this--delete-my-overlay (overlay &optional _after? _beg _end _length)
  "Delete the OVERLAY overlays only if it's an `fold-this'."
  (when (eq (overlay-get overlay 'type) 'fold-this)
    (delete-overlay overlay)))

(defun fold-this-sexp ()
  "Fold sexp around point.

If the point is at a symbol, fold the parent sexp.  If the point
is in front of a sexp, fold the following sexp."
  (interactive)
  (let* ((region
          (cond
           ((symbol-at-point)
            (save-excursion
              (when (nth 3 (syntax-ppss))
                (goto-char (nth 8 (syntax-ppss))))
              (backward-up-list)
              (cons (point)
                    (progn
                      (forward-sexp)
                      (point)))))
           ((looking-at-p (rx (* blank) "("))
            (save-excursion
              (skip-syntax-forward " ")
              (cons (point)
                    (progn
                      (forward-sexp)
                      (point)))))
           (t nil)))
         (header (when region
                   (save-excursion
                     (goto-char (car region))
                     (buffer-substring (point) (line-end-position))))))
    (when region
      (fold-this (car region) (cdr region) header))))

;;; Fold-this overlay persistence
;;

(defvar fold-this--overlay-alist nil
  "An alist of filenames mapped to fold overlay positions.")

(defvar fold-this--overlay-alist-loaded nil
  "Non-nil if the alist has already been loaded.")

(defun fold-this-restore ()
  "A hook restoring fold overlays."
  (interactive)
  (when (and fold-this-persistent-folds
             buffer-file-name
             (not (derived-mode-p 'dired-mode)))
    (when (not fold-this--overlay-alist-loaded)
      (fold-this--load-alist-from-file))
    (let* ((file-name buffer-file-name)
           (cell (assoc file-name fold-this--overlay-alist)))
      (when cell
        (mapc (lambda (pair) (fold-this (car pair) (cdr pair)))
              (cdr cell))
        (setq fold-this--overlay-alist
              (delq cell fold-this--overlay-alist))))))

(defun fold-this-save-this ()
  "A hook saving overlays."
  (interactive)
  (when (and fold-this-persistent-folds
             buffer-file-name
             (not (derived-mode-p 'dired-mode)))
    (when (not fold-this--overlay-alist-loaded)
      ;; is it even possible ?
      (fold-this--load-alist-from-file))
    (save-restriction
      (widen)
      (mapc 'fold-this--save-overlay-to-alist
	        (overlays-in (point-min) (point-max))))
    (when (alist-get buffer-file-name fold-this--overlay-alist)
      (fold-this--save-alist-to-file))))

(defun fold-this-save-all ()
  "A hook saving overlays in all buffers and dumping them into a file."
  (when (and fold-this-persistent-folds
             fold-this--overlay-alist-loaded)
    (fold-this--walk-buffers-save-overlays)
    (fold-this--save-alist-to-file)))

(defun fold-this--save-alist-to-file ()
  "Save current overlay alist to file."
  (fold-this--clean-unreadable-files)
  (when fold-this-persistent-folded-file-limit
    (fold-this--check-fold-limit))
  (let ((file (expand-file-name fold-this-persistent-folds-file))
        (coding-system-for-write 'utf-8)
        (version-control 'never))
    (with-current-buffer (get-buffer-create " *Fold-this*")
      (delete-region (point-min) (point-max))
      (insert (format ";;; -*- coding: %s -*-\n"
                      (symbol-name coding-system-for-write)))
      (let ((print-length nil)
            (print-level nil))
        (pp fold-this--overlay-alist (current-buffer)))
      (let ((version-control 'never))
        (condition-case nil
            (write-region (point-min) (point-max) file)
          (file-error (message "Fold-this: can't write %s" file)))
        (kill-buffer (current-buffer))))))

(defun fold-this--load-alist-from-file ()
  "Restore ovelay alist `fold-this--overlay-alist' from file."
  (let ((file (expand-file-name fold-this-persistent-folds-file)))
    (when (file-readable-p file)
      (with-current-buffer (get-buffer-create " *Fold-this*")
        (delete-region (point-min) (point-max))
        (insert-file-contents file)
        (goto-char (point-min))
        (setq fold-this--overlay-alist
              (with-demoted-errors "Error reading fold-this-persistent-folds-file %S"
                (car (read-from-string
                      (buffer-substring (point-min) (point-max))))))
        (kill-buffer (current-buffer))))
    (setq fold-this--overlay-alist-loaded t)))

(defun fold-this--walk-buffers-save-overlays ()
  "Walk the buffer list, save overlays to the alist."
  (let ((buf-list (buffer-list)))
    (while buf-list
      (with-current-buffer (car buf-list)
        (when (and buffer-file-name
                   (not (derived-mode-p 'dired-mode)))
          (setq fold-this--overlay-alist
                (delq (assoc buffer-file-name fold-this--overlay-alist)
                      fold-this--overlay-alist))
          (save-restriction
	        (widen)
	        (mapc 'fold-this--save-overlay-to-alist
		          (overlays-in (point-min) (point-max)))))
        (setq buf-list (cdr buf-list))))))

(defun fold-this--save-overlay-to-alist (overlay)
  "Add an OVERLAY position pair to the alist."
  (when (eq (overlay-get overlay 'type) 'fold-this)
    (let* ((pos (cons (overlay-start overlay) (overlay-end overlay)))
           (file-name buffer-file-name)
           (cell (assoc file-name fold-this--overlay-alist))
           overlay-list)
      (unless (member pos cell) ;; only if overlay is not already there
        (when cell
          (setq fold-this--overlay-alist
                (delq cell fold-this--overlay-alist)
                overlay-list (delq pos (cdr cell))))
        (setq fold-this--overlay-alist
              (cons (cons file-name (cons pos overlay-list))
                    fold-this--overlay-alist))))))

(defun fold-this--clean-unreadable-files ()
  "Check if files in the alist exist and are readable.
Drop non-existing/non-readable ones."
  (when fold-this--overlay-alist
    (let ((orig fold-this--overlay-alist)
          new)
      (dolist (cell orig)
        (let ((fname (car cell)))
          (when (file-readable-p fname)
            (setq new (cons cell new)))))
      (setq fold-this--overlay-alist
            (nreverse new)))))

(defun fold-this--check-fold-limit ()
  "Check if there are more folds than possible.
Drop the tail of the alist."
  (when (> fold-this-persistent-folded-file-limit 0)
    (let ((listlen (length fold-this--overlay-alist)))
      (when (> listlen fold-this-persistent-folded-file-limit)
        (setcdr (nthcdr (1- fold-this-persistent-folded-file-limit) fold-this--overlay-alist)
                nil)))))

(provide 'fold-this)
;;; fold-this.el ends here
