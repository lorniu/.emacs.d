;;; -*- lexical-binding: t -*-

;;; Code:

(xzz gptel
  :ref "karthink/gptel"
  :config
  (setq gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-prompt-prefix-alist '((markdown-mode . "## ")
                                    (org-mode . "** ")
                                    (text-mode . "## ")))

  (defun:hook gptel-mode-hook/indent ()
    (toggle-truncate-lines -1)
    (when (and (derived-mode-p 'org-mode)
               (bound-and-true-p org-indent-mode))
      (org-indent-mode -1)))

  (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send))

(defun im/gptel-mark-region-role (start end role)
  "Mark region from START to END with the llm or user role."
  (interactive (list (region-beginning) (region-end)
                     (completing-read "Mark region role as: " '(response prompt ignore))))
  (if (or (not transient-mark-mode)
          (use-region-p))
      (with-silent-modifications
        (if (equal role "prompt")
            (remove-text-properties start end '(gptel))
          (put-text-property start end 'gptel (intern role)))
        (message "Region marked as %s" role))
    (message "No region specified, action canceled")))
