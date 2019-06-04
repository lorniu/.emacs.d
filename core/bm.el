;;; bm.el --- Benchmark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar bm/time nil)
(defvar bm/last-time nil)
(defvar loaded-messages nil)

(defun time-subtract-seconds (b a)
  (float-time (time-subtract b a)))

(defun time-subtract-millis (b a)
  (* 1000.0 (time-subtract-seconds b a)))

(defun bm/show-time (&optional sortp)
  (ppp (if sortp
           (let ((time (copy-tree bm/time)))
             (sort time (lambda (a b) (> (cdr a) (cdr b)))))
         bm/time)))

(defun bm/differ ()
  (ppp
   (sort (seq-difference bm/time bm/last-time)
         (lambda (a b) (> (cdr a) (cdr b)))))
  (setq bm/last-time (copy-tree bm/time)) t)

(defun bm/require-statics-advice (oldfun feature &optional filename noerror)
  (let* ((already-loaded (memq feature features))
         (start-time (and (not already-loaded) (current-time))))
    (prog1
        (funcall oldfun feature filename noerror)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (time-subtract-millis (current-time) start-time)))
          (add-to-list 'bm/time (cons feature time) t))))))

(add-function :around (symbol-function 'require) 'bm/require-statics-advice)

(defun display-startup-echo-area-message ()
  (message "%s%s>> Loaded successfully in %.2f seconds."
           (mapconcat 'identity (reverse loaded-messages) "\n")
           (if loaded-messages "\n" "")
           (time-subtract-seconds after-init-time before-init-time)))


(provide 'bm)

;;; bm.el ends here
