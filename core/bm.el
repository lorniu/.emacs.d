;;; bm.el --- BenchMarking
;;; Commentary:
;;; Code:

(defvar bm/time nil)

(defvar bm/last-time nil)

(defun time-subtract-seconds (b a)
  (float-time (time-subtract b a)))

(defun time-subtract-millis (b a)
  (* 1000.0 (time-subtract-seconds b a)))

(defadvice require (around bm/build-time (feature &optional filename noerror) activate)
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'bm/time (cons feature time) t))))))

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

(defun display-startup-echo-area-message ()
  (message ">> Loaded success with %.2f Seconds."
           (time-subtract-seconds after-init-time before-init-time)))


(provide 'bm)

;;; bm.el ends here
