;;; utils.el --- Functions -*- lexical-binding: t -*-

;;; Code:

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

(defmacro block-undo (&rest body)
  "Disable undo ring temporaryly during execute BODY."
  `(progn
     (undo-boundary)
     (cl-letf (((symbol-function 'undo-boundary) (lambda ()))) ,@body)
     (undo-boundary)))

(defun block-undo-advice (fn &rest args)
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(defmacro im-profile (&rest body)
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

(defun time-str (&optional time nano)
  "Format TIME to String. if TIME is nil, return current time."
  (format-time-string
   (if nano (concat "%F %T.%" (number-to-string nano) "N") "%F %T")
   time))

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

(defun im-ensure-file (file-name)
  "Create file with name FILE-NAME if not exists."
  (unless (file-exists-p file-name)
    (with-temp-buffer (write-file file-name)))
  file-name)

(defun im-ensure-dir (dir-name)
  "Create dir with name DIR-NAME if not exists."
  (unless (file-exists-p dir-name)
    (make-directory dir-name))
  dir-name)

(defun im-read-file-content (file &optional callback)
  "Read the FILE content as string, file can be a url."
  (with-temp-buffer
    (if (string-match-p "^\\(http\\|file://\\)" file)
        (url-insert-file-contents file)
      (insert-file-contents file))
    (let ((bs (buffer-substring-no-properties (point-min) (point-max))))
      (if callback (funcall callback bs) bs))))

(defmacro im-open-file-view (file &rest args)
  "Open a FILE with View-Mode."
  `(progn (find-file ,file ,@args) (view-mode +1)))

(defun im-replace-all-in-buffer (list &optional pre-hook)
  "Replace all occurs in current buffer. eg:\n
(im-replace-all-in-buffer '((aaa . bbb) (ccc . ddd)) (lambda () (message xxx))) "
  (if pre-hook (funcall pre-hook))
  (dolist (needle list)
    (goto-char (point-min))
    (while (re-search-forward (car needle) nil t)
      (replace-match (cdr needle)))))

(defun im-call-process-to-string (program &rest args)
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

(defun im-thing-at-region-or-point (&optional thing)
  "Return string at point. Region first, THING then."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (cond ((functionp thing) (funcall thing))
          ((symbolp thing) (thing-at-point (or thing 'word) 'no-properties))
          (t thing))))

(defun s%indent-string (s)
  "Used to move spaces at beginning of every line of S."
  (with-temp-buffer
    (insert s)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun im-in-comment-p (&optional pos)
  "Test if character at POS is comment.  If POS is nil, character at `(point)' is tested"
  (unless pos (setq pos (point)))
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (or (member 'font-lock-comment-face fontfaces)
        (member 'font-lock-comment-delimiter-face fontfaces))))

(defun im-grab-line (regexp &optional expression)
  "Return a match string for REGEXP if it matches text before point."
  (let ((inhibit-field-text-motion t))
    (when (looking-back regexp (point-at-bol))
      (or (match-string-no-properties (or expression 0)) ""))))

(defun im-capf-functions-of-mode (mode)
  "Get value of `completion-at-point-functions' by MODE."
  (condition-case _
      (with-temp-buffer (funcall mode) completion-at-point-functions)
    (error completion-at-point-functions)))

(defmacro im-make-buffer-buriable ()
  `(progn (set-buffer-modified-p nil)
          (page-break-lines-mode 1)
          (local-set-key "q" 'bury-buffer)))


;;; Common Logger

(defvar ic/temp-log-buffer " look-im-the-temp-log")

(defmacro my-string-interpolation (string)
  (let (fmt-string forms)
    (setq fmt-string
          (replace-regexp-in-string
           "{\\([^}]+\\)}"
           (lambda (substring)
             (push (read (match-string 1 substring)) forms)
             "%s")
           string))
    `(format ,fmt-string ,@(nreverse forms))))

(defmacro cw (&rest inputs)
  "Console.WriteLine to a buffer... Support {interpolation} syntax."
  ;; (cw "name: {name} age: {age}" other :buffer xxx)
  (let (messages buffer)
    (while inputs
      (let ((o (pop inputs)))
        (if (equal o :buffer)
            (setq buffer (let ((b (pop inputs))) (if (stringp b) b (read b))))
          (push o messages))))
    (setq messages (nreverse messages))
    `(with-current-buffer (get-buffer-create (or ,buffer ic/temp-log-buffer))
       (local-set-key "q" #'bury-buffer)
       (local-set-key "C" #'erase-buffer)
       (let ((line (propertize "⌁ " 'face 'font-lock-comment-face)))
         (cl-labels ((ladd (x) (setq line (concat line x)))
                     (mdot () (when (> (length line) 2) (ladd (propertize " ⋅ " 'face '(:foreground "red"))))))
           ,@(cl-loop for _m in messages
                      collect `(mdot)
                      for m1 = (pop messages)
                      for norm = (if (stringp m1) m1 (format "{%s}" m1))
                      collect `(ladd (my-string-interpolation ,norm)))
           (ladd "\n")
           (goto-char (point-max))
           (insert line)
           (when-let ((w (get-buffer-window (current-buffer) 'visible)))
             (set-window-point w (point-max)))
           (set-buffer-modified-p nil))))))

(defun cw-1 () (interactive) (display-buffer (get-buffer-create ic/temp-log-buffer)))


;;; Hooks & advice helper

(defmacro defun:hook (hook params &rest body)
  "Usage: (defun:hook hook-name/tag () xxx), tag can be ignored."
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
                   ,@(cl-loop for where in (mapcar 'car (if (boundp 'advice--how-alist) advice--how-alist advice--where-alist)) ; v29 rename to how
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


;;; Helper & Log

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat 'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))


;;; Miscellaneous

(defun imup () (if ic/up (if (string-match-p "@" ic/up) ic/up (concat "vip@" ic/up))))
(defun up-host () (aif (and ic/up (split-string (imup) "@")) (cadr it)))
(defun up-user () (aif (and ic/up (split-string (imup) "@")) (car it)))

(provide 'utils)

;;; utils.el ends here
