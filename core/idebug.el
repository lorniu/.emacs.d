;;; idebug.el --- Debug and Profiler -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defmacro pm (expr)
  `(progn (pp (macroexpand-1 ',expr)) t))

(defun ppp (list)
  (dolist (l list t) (princ l) (terpri)))

(defun logit (&rest args)
  "Output messsage to a buffer. Usage: (logit [buffer-name] fmtString variable)."
  (let ((buffer-name (if (symbolp (car args))
                         (prog1 (symbol-name (car args))
                           (setq args (cdr args)))
                       "*logger*")))
    (with-current-buffer (get-buffer-create buffer-name)
      (local-set-key (kbd "q") 'bury-buffer)
      (goto-char (point-max))
      (insert "\n")
      (insert (apply 'format args)))))

(defun logdb (&optional msg cat ext table)
  "Log message to postgres. Should table 'elog' and macro `with-my-pg' exist."
  (with-my-pg
   "insert into %s (cat, msg, ext) values (%s, %s, %s)"
   (list (or table "elog")
         (if cat (format "'%s'" cat) "null")
         (if msg (format "'%s'" msg) "null")
         (if ext (format "'%s'" ext) "null"))))

(defun logdb-current (&optional arg)
  "Send current selection or buffer to private postgres db."
  (interactive "P")
  (let ((send-string
         (if (and arg (y-or-n-p "Log current buffer to db?"))
             (buffer-string)
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-string "String to log: ")))))
    (message "%s" (logdb send-string))))

(defmacro im/profile (&rest body)
  "Profiler BODY form."
  `(progn (profiler-start 'cpu)
          (ignore-errors ,@body (profiler-report))
          (profiler-stop)))

(add-hook 'edebug-mode-hook (lambda () (view-mode -1)))


(provide 'idebug)

;;; idebug.el ends here
