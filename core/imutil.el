;;; imutil.el --- Functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(eval-when-compile (require 'cl-lib))



;;; Helper

(defmacro aif (test then &optional else)
  (declare (indent defun))
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro awhen (test &rest rest)
  (declare (indent defun))
  `(aif ,test (progn ,@rest)))

(defmacro lambdai (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro add-hook-lambda (hook &rest body)
  (declare (indent defun))
  `(add-hook ,hook (lambda () ,@body)))

(defun insert-here (content)
  (save-excursion (insert (format "\n%s" content))))

(defun list-nn (&rest elements)
  "Make ELEMENTS to list, ignore nil."
  (cl-loop for e in elements when (not (null e)) collect e))

(defmacro append-local (where &rest what)
  `(progn
     (make-variable-buffer-local ,where)
     ,@(mapcar (lambda (w) `(add-to-list ,where ,w)) what)))

(defmacro without-msg (&rest form)
  `(let ((inhibit-message t)) ,@form))

(defmacro without-rencentf (&rest form)
  `(unwind-protect
       (progn (without-msg (recentf-mode -1)) ,@form)
     (without-msg (recentf-mode +1))))

(defmacro save-undo (&rest body)
  "Disable undo ring temporaryly during execute BODY."
  `(progn
     (undo-boundary)
     (cl-letf (((symbol-function 'undo-boundary) (lambda ()))) ,@body)
     (undo-boundary)))



;;; More Utils

(defun time (&optional time nano)
  "Format TIME to String. if TIME is nil, return current time."
  (format-time-string
   (if nano (concat "%F %T.%" (number-to-string nano) "N") "%F %T")
   time))

(defun string-repeat (init times)
  "Make a new string repeat TIMES for INIT."
  (apply 'concat (make-list times init)))

(defun string-join-newline (&rest strings)
  "Join list STRINGS with newline, return one String."
  (mapconcat 'identity strings "\n"))

(defun padding-left-to-string (item &optional needle)
  "Padding every line of ITEM with NEEDLE. If ITEM is a list, then join it with padding."
  (mapconcat (lambda (s) (concat (or needle "  ") s))
             (if (listp item) item (split-string item "\n")) "\n"))

(defun defer-til-hook (fun-or-funs hook)
  "Defer the execution of FUN-OR-FUNS until HOOK triggled."
  (mapc (lambda (fun) (advice-add fun :around
				                  (lambda (f &rest args)
						            (add-hook hook (lambda () (apply f args))))))
	    (if (consp fun-or-funs) fun-or-funs (list fun-or-funs))))

(defun ensure-file (file-name)
  "Create file with name FILE-NAME if not exists."
  (unless (file-exists-p file-name)
    (with-temp-buffer (write-file file-name))) file-name)

(defun im/read-file-content (file &optional callback)
  "Read the FILE content as string, file can be a url."
  (with-temp-buffer
    (if (string-match-p "^\\(http\\|file://\\)" file)
        (url-insert-file-contents file)
      (insert-file-contents-literally file))
    (let ((buffer-string (buffer-substring-no-properties
                          (point-min) (point-max))))
      (if callback
          (funcall callback buffer-string)
        (buffer-string)))))

(defmacro im/open-file-view (file &rest args)
  "Open a FILE with View-Mode."
  `(progn (find-file ,file ,@args) (view-mode +1)))

(defun im/replace-all-in-buffer (list &optional pre-hook)
  "Replace all occurs in current buffer. eg:\n
(im/replace-all-in-buffer '((aaa . bbb) (ccc . ddd)) (lambda () (message xxx))) "
  (if pre-hook (funcall pre-hook))
  (dolist (needle list)
    (goto-char (point-min))
    (while (re-search-forward (car needle) nil t)
      (replace-match (cdr needle)))))

(defun im/call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(defun decode-html-entities (html)
  "Decode &lt; style string. ie. html entity."
  (with-temp-buffer
    (save-excursion (insert html))
    (xml-parse-string)))


(provide 'imutil)

;;; imutil.el ends here
