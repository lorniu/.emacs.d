;;; transient-command.el --- Tansient Command -*- lexical-binding: t -*-

;; Save a command, invoke it at once, or it will be cleared.
;;
;; (im/transient-command-set 'ssss)
;; (im/transient-command-set 'tttt :repeat)
;; (im/transient-command-set (lambda () (message "hello")))
;;
;; <F4>

;;; Code:

(defvar im/transient-command nil)
(defvar im/transient-command-timer nil)
(defvar im/transient-command-ttl 45)

(defun im/transient-command-clear ()
  (setq im/transient-command nil)
  (if im/transient-command-timer (cancel-timer im/transient-command-timer))
  (setq im/transient-command-timer nil))

(defun im/transient-command-timer-task ()
  (run-with-timer im/transient-command-ttl nil 'im/transient-command-clear))

(defun im/transient-command-set (command &optional repeat-p)
  (setq im/transient-command (cons command repeat-p))
  (when im/transient-command-timer
    (cancel-timer im/transient-command-timer))
  (setq im/transient-command-timer (im/transient-command-timer-task)))

(defun im/transient-command-invoke ()
  (let ((cmd (car im/transient-command))
        (repeat? (cdr im/transient-command)))
    (when cmd
      (funcall cmd)
      (im/transient-command-clear)
      (when repeat?
        (im/transient-command-set cmd repeat?))
      t)))

(global-set-key [f4] (lambda ()
                       (interactive)
                       (or (im/transient-command-invoke)
                           (call-interactively 'kmacro-end-or-call-macro))))

(provide 'transient-command)

;;; transient-command.el ends here
