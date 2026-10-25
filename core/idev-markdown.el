;;; -*- lexical-binding: t -*-

;;; Code:


(require 'markdown-ts-mode)

(add-hook 'markdown-ts-mode-hook #'markdown-ts-appear-mode)

(defun im:markdown-appear-toggle-cursor ()
  (when (memq 'markdown-ts-appear-mode minor-mode-list)
    (if buffer-read-only
        (markdown-ts-appear-stop)
      (markdown-ts-appear-start))))

(add-hook 'view-mode-hook #'im:markdown-appear-toggle-cursor)
(add-hook 'read-only-mode-hook #'im:markdown-appear-toggle-cursor)

(defun im/markdown-hide-levels ()
  "Hide to Lv-2."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "##+ " nil t)
      (outline-hide-entry)
      (forward-line 1))))
