;;; corfu-grouping.el --- Narrow via sorting for Corfu -*- lexical-binding: t -*-

;;; Code:

(require 'corfu)
(eval-when-compile (require 'cl-lib))

(defvar-local corfu-grouping-enable nil)

(defun corfu-grouping-sort (candicates)
  "Sort CANDICATES by original rule, then resort according `corfu-grouping-enable'."
  (let* ((fn (or (corfu--metadata-get 'display-sort-function) corfu-sort-function))
         (ordered (funcall fn candicates))
         (kindfn (plist-get completion-extra-properties :company-kind)))
    (when (and corfu-grouping-enable (functionp kindfn))
      (let ((kinds (delete-dups (mapcar (lambda (c) (funcall kindfn c)) ordered))))
        (when (> (length kinds) 1)
          (setq ordered
                (cl-loop for kind in kinds
                         append (cl-remove-if-not
                                 (lambda (c) (equal (funcall kindfn c) kind))
                                 ordered))))))
    ordered))

;;;###autoload
(defun corfu-grouping-toggle ()
  "Redisplay candicates according `corfu-grouping-enable'.
If `corfu-grouping-enable' is true, then force display candicates
grouping by kind, else restore the order."
  (interactive)
  (setq corfu-grouping-enable (not corfu-grouping-enable))
  (let ((corfu--input nil)
        (cand (nth corfu--index corfu--candidates)))
    (corfu--update)
    (setq corfu--index (cl-position cand corfu--candidates :test #'string=))))

(setq corfu-sort-override-function #'corfu-grouping-sort)

(define-key corfu-map "\M-r" #'corfu-grouping-toggle)

(provide 'corfu-grouping)

;;; corfu-grouping.el ends here
