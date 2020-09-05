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
  `(add-hook ',hook (lambda () ,@body)))

(defmacro append-local (where &rest what)
  `(progn
     (make-local-variable ',where)
     ,@(mapcar (lambda (w) `(add-to-list ',where ,w)) what)))

(defmacro with-message (action &rest body)
  "Usage:
(with-message nil ...) to suppress all messages.
(with-message sym ...) to dispatch messages to function sym.
(with-message sexp ...) to dispatch messages to sexp, ie: lambda.
"
  (declare (indent 1))
  (if action
      `(cl-letf (((symbol-function 'message) ,action)) ,@body)
    `(let ((message-log-max nil))
       (with-temp-message (or (current-message) "") ,@body))))

(defmacro with-supress-recentf (&rest form)
  "Disable recentf temporaryly."
  `(cl-letf (((symbol-function 'recentf-track-opened-file) (lambda () nil)))
     ,@form))

(defmacro save-undo (&rest body)
  "Disable undo ring temporaryly during execute BODY."
  `(progn
     (undo-boundary)
     (cl-letf (((symbol-function 'undo-boundary) (lambda ()))) ,@body)
     (undo-boundary)))

(defmacro im/profile (&rest body)
  "Profiler BODY form."
  `(progn (profiler-start 'cpu)
          (ignore-errors ,@body (profiler-report))
          (profiler-stop)))

(defmacro with-scratchpad-name (name &rest body)
  "Provide for Named Scratchpad in XMonad."
  (declare (indent 1))
  `(progn (select-frame (make-frame '((name . ,name))))
          ,@body))

(defmacro pp/macro (expr)
  `(progn (pp (macroexpand-1 ',expr)) t))

(defmacro pp/list (lst)
  `(cl-loop for i in ,lst
            do (princ i) (terpri)
            finally (return
                     (let ((v [:length nil :items nil]))
                       (aset v 1 (length ,lst))
                       (aset v 3 ,lst)
                       v))))


;;; Utils

(defun list-nn (&rest elements)
  "Make ELEMENTS to list, ignore nil."
  (cl-loop for e in elements when (not (null e)) collect e))

(defun time-str (&optional time nano)
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

(defun plist-merge (&rest plists)
  "Merge all PLISTS to one, latter override early."
  (let ((rtn (copy-sequence (pop plists))) ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq rtn (plist-put rtn (pop ls) (pop ls)))))
    rtn))

(defun ensure-file (file-name)
  "Create file with name FILE-NAME if not exists."
  (unless (file-exists-p file-name)
    (with-temp-buffer (write-file file-name))) file-name)

(defun im/read-file-content (file &optional callback)
  "Read the FILE content as string, file can be a url."
  (with-temp-buffer
    (if (string-match-p "^\\(http\\|file://\\)" file)
        (url-insert-file-contents file)
      (insert-file-contents file))
    (let ((bs (buffer-substring-no-properties (point-min) (point-max))))
      (if callback (funcall callback bs) bs))))

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

(defun powershell-execute (cmd)
  "Execute this string as command in PowerShell."
  (shell-command (format "powershell -Command \"& {%s}\""
                         (encode-coding-string
                          (replace-regexp-in-string "\n" " " cmd)
                          (keyboard-coding-system)))))

(defun join-as-regor-group (&rest items)
  "Join ITEMS as \\(aaa\\)\\|\\(bbb\\) style."
  (mapconcat (lambda (x) (format "\\(%s\\)" x)) items "\\|"))

(defun assoc-for (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun file-name-parent-directory (dir)
  "Get the parent directory for current DIR."
  (when (and dir (not (equal "/" dir)))
    (file-name-directory (directory-file-name dir))))

(defun im/thing-at-region-or-point (&optional thing)
  "Return string at point. Region first, THING then."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (cond ((functionp thing) (funcall thing))
          ((symbolp thing) (thing-at-point (or thing 'word) 'no-properties))
          (t thing))))


;;; Wraps

(defmacro >>init>> (&rest body)
  `(add-to-list 'after-init-hook (defun my-after-init-do-something () ,@body)))
(defmacro >>face>> (&rest body)
  `(with-eval-after-load 'face ,@body))

(defun loce (subpath &optional nullp) (let ((f (locate-user-emacs-file-origin subpath))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun locc (&optional subpath nullp) (let ((f (locate-user-emacs-file (or subpath "./")))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun loco (subpath &optional nullp) (let ((f (expand-file-name subpath org-directory))) (if (or (not nullp) (file-exists-p f)) f)))

(defmacro defun-hook (hook params &rest body)
  "Usage: (defun-hook hook-name/tag () xxx), tag can be ignored."
  (declare (indent defun))
  (let* ((sym-name (symbol-name hook))
         (name-arr (split-string sym-name "hook/"))
         (hook-name (intern (concat (car name-arr) (if (cdr name-arr) "hook"))))
         (hook-fun-name (intern (concat "imhook:"
                                        (car name-arr)
                                        (if (cadr name-arr) (format "hook~%s" (cadr name-arr)))))))
    `(progn
       (defun ,hook-fun-name ,params ,@body)
       (unless (and (boundp ',hook-name) (member ',hook-fun-name ,hook-name))
         (add-hook ',hook-name ',hook-fun-name))
       ,hook-name)))

(cl-macrolet ((generate-all-defun:where-macros
               nil
               `(progn
                  ,@(cl-loop for where in (mapcar 'car advice--where-alist)
                             collect
                             `(defmacro ,(intern (format "defun%s" where)) (advice-name params &rest body)
                                ,(format "Usage: (defun%s function$label (args) body), tag cannot be ignored." where)
                                (declare (indent defun))
                                (let* ((fun-arr (split-string (symbol-name advice-name) "\\$"))
                                       (fun-name (intern (car fun-arr))))
                                  (unless (cdr fun-arr)
                                    (user-error "Advice name not available"))
                                  `(progn
                                     (defun ,advice-name ,params ,@body)
                                     (advice-add #',fun-name ,,where #',advice-name)
                                     (list ',fun-name ,,where ',advice-name))))))))
  (generate-all-defun:where-macros))


(provide 'util)

;;; imutil.el ends here
