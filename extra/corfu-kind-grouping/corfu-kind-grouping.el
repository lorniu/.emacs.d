;;; corfu-kind-grouping.el --- Narrow via sorting for Corfu -*- lexical-binding: t -*-

;;; Code:

(require 'corfu)
(eval-when-compile (require 'cl-lib))

(defvar-local corfu-kind-grouping-enable nil)

(defun corfu-kind-grouping-sort (candicates)
  "Sort CANDICATES by original rule, then resort according `corfu-kind-grouping-enable'."
  (let* ((fn (or (corfu--metadata-get 'display-sort-function) corfu-sort-function))
         (ordered (funcall fn candicates))
         (kindfn (plist-get corfu--extra :company-kind)))
    (when (and corfu-kind-grouping-enable (functionp kindfn))
      (let ((kinds (delete-dups (mapcar (lambda (c) (funcall kindfn c)) ordered))))
        (when (> (length kinds) 1)
          (setq ordered
                (cl-loop for kind in kinds
                         append (cl-remove-if-not
                                 (lambda (c) (equal (funcall kindfn c) kind))
                                 ordered))))))
    ordered))

;;;###autoload
(defun corfu-kind-grouping-toggle ()
  "Redisplay candicates according `corfu-kind-grouping-enable'.
If `corfu-kind-grouping-enable' is true, then force display candicates
grouping by kind, else restore the order."
  (interactive)
  (setq corfu-kind-grouping-enable (not corfu-kind-grouping-enable))
  (let ((corfu--input nil)
        (cand (nth corfu--index corfu--candidates)))
    (corfu--update)
    (setq corfu--index (cl-position cand corfu--candidates :test #'string=))))

(setq corfu-sort-override-function #'corfu-kind-grouping-sort)

(define-key corfu-map "\M-r" #'corfu-kind-grouping-toggle)

(provide 'corfu-kind-grouping)

;;; corfu-kind-grouping.el ends here
