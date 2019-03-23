;;; super-window.el --- Extend window operation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup ip/sw-mode nil
  "My Special Window Mode."
  :group 'imfine)

(defcustom ip/other-window-skip-regexp nil
  "Let `other-window' skip some buffers."
  :type 'string
  :group 'ip/sw-mode)

(defvar-local ip/sw--mode-flag '(:eval (ip/sw--update-mode-line)))

(defvar ip/sw-mode-map (make-sparse-keymap) "Mode map for sw-mode.")

(defun ip/sw--update-mode-line ()
  "Add delight flag for skipped buffers."
  (when (and ip/other-window-skip-regexp
             (> (length ip/other-window-skip-regexp) 0)
             (string-match-p ip/other-window-skip-regexp (buffer-name)))
    (let ((flag "X")
          (face '((t (:foreground "red" :strike-through t)))))
      (propertize flag 'face face))))

;;;###autoload
(define-minor-mode ip/sw-mode
  "Some special actions with windows.

eg:

  Make `other-window' command skip some special windows via `ip/other-window-skip-regexp'.

"
  :lighter (:eval (ip/sw--update-mode-line))
  :keymap ip/sw-mode-map
  :group 'ip/sw-mode
  :global t)

;;;###autoload
(defun ip/other-window+ ()
  "Extend `other-window', make it skip some special buffers."
  (interactive)
  (let* ((window-list (delq (selected-window) (window-list)))
         (filtered-window-list (cl-remove-if
                                (lambda (w)
                                  (and ip/other-window-skip-regexp
                                       (string-match-p ip/other-window-skip-regexp (buffer-name (window-buffer w)))))
                                window-list)))
    (if filtered-window-list
        (select-window (car filtered-window-list)))))

;;;###autoload
(defun ip/other-window-skip-regexp (regexp)
  "Config the REGEXP used by `ip/other-window+'."
  (interactive (list ip/other-window-skip-regexp))
  (setq ip/other-window-skip-regexp (read-from-minibuffer "Buffer to exclude (regexp): " regexp)))

;;;###autoload
(defun ip/other-window-skip-this ()
  "Add current buffer to skip list."
  (interactive)
  (ip/other-window-skip-regexp
   (concat ip/other-window-skip-regexp
           (and ip/other-window-skip-regexp
                (> (length ip/other-window-skip-regexp) 0)
                "\\|")
           (regexp-quote (buffer-name)))))


(provide 'super-window)

;;; super-window.el ends here
