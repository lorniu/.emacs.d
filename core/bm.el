;;; bm.el --- For benchmark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar messages-on-load nil)



(defvar bm/require-times nil)
(defvar bm/require-times-from-last-diff nil)

(defun time-subtract-seconds (b a)
  (float-time (time-subtract b a)))

(defun time-subtract-millis (b a)
  (* 1000.0 (time-subtract-seconds b a)))

(defun bm/show-require-times (&optional not-sortp)
  (pp/list (if not-sortp
               bm/require-times
             (let ((time (copy-tree bm/require-times)))
               (sort time (lambda (a b) (> (cdr a) (cdr b))))))))

(defun bm/show-require-differ-times ()
  (pp/list
   (sort (seq-difference bm/require-times bm/require-times-from-last-diff )
         (lambda (a b) (> (cdr a) (cdr b)))))
  (setq bm/require-times-from-last-diff  (copy-tree bm/require-times)) t)

(defun bm/require-statics-advice (oldfun feature &optional filename noerror)
  (let* ((already-loaded (memq feature features))
         (start-time (and (not already-loaded) (current-time))))
    (prog1
        (funcall oldfun feature filename noerror)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (time-subtract-millis (current-time) start-time)))
          (add-to-list 'bm/require-times (cons feature time) t))))))

(add-function :around (symbol-function 'require) 'bm/require-statics-advice)

(defun display-startup-echo-area-message ()
  (setq inhibit-message nil)
  (message "%s>> Loaded successfully in %.2f seconds."
           (if messages-on-load
               (format "\n%s\n\n" (mapconcat 'identity (reverse messages-on-load) "\n"))
             "")
           (time-subtract-seconds after-init-time before-init-time)))



(defvar bm/imload-times nil)

(defmacro bm/record-imload-time (label &rest rest)
  `(let ((start-time (current-time)))
     ,@rest
     (push (cons ,label (time-subtract-millis (current-time) start-time)) bm/imload-times)))

(defun imload (mod &optional noerror)
  (bm/record-imload-time mod (require mod nil noerror)))

(defun bm/show-imload-time (&optional nsortp)
  (pp/list (if nsortp
               bm/imload-times
             (append (sort (copy-tree bm/imload-times) (lambda (a b) (> (cdr a) (cdr b))))
                     (list (cons 'TOTAL-TIME (apply '+ (mapcar 'cdr bm/imload-times))))))))


(provide 'bm)

;;; bm.el ends here
