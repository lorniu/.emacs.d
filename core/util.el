;;; util.el --- Functions -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

(defmacro lambdai (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro setq-unless (var &rest args)
  (declare (indent 1))
  `(unless ,var (setq ,var (progn ,@args))))

(defmacro ppi (obj)
  "`pp' and insert the result to current point."
  `(progn
     (insert "\n\n" (pp-to-string ,obj))
     (skip-chars-backward " \t\n")))

(defmacro pp/list (lst)
  `(cl-loop for i in ,lst
            do (princ i) (terpri)
            finally (return
                     (let ((v [:length nil :items nil]))
                       (aset v 1 (length ,lst))
                       (aset v 3 ,lst)
                       v))))

(defmacro im:with-message (action &rest body)
  "Usage:
(im:with-message nil ...) to suppress all messages.
(im:with-message sym ...) to dispatch messages to function sym.
(im:with-message sexp ...) to dispatch messages to sexp, ie: lambda.
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

(defmacro with-profiler (&rest body)
  "Profiler BODY form."
  `(progn (profiler-start 'cpu)
          (ignore-errors ,@body (profiler-report))
          (profiler-stop)))

(defmacro with-scratchpad-name (name &rest body)
  "Provide for Named Scratchpad in XMonad."
  (declare (indent 1))
  `(progn (select-frame (make-frame '((name . ,name))))
          ,@body))

;; string

(defun im:string-pad (item &optional needle)
  "Padding every line of ITEM with NEEDLE. If ITEM is a list, then join it with padding."
  (mapconcat (lambda (s) (concat (or needle "  ") s))
             (if (listp item) item (split-string item "\n")) "\n"))

(defun im:string-indent (s)
  "Used to move spaces at beginning of every line of S."
  (with-temp-buffer
    (insert s)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun im:join-as-regor-group (&rest items)
  "Join ITEMS as \\(aaa\\)\\|\\(bbb\\) style."
  (mapconcat (lambda (x) (format "\\(%s\\)" x)) items "\\|"))

(defmacro im:string-interpolation (string)
  (let (fmt-string forms)
    (setq fmt-string
          (replace-regexp-in-string
           "{\\([^}]+\\)}"
           (lambda (substring)
             (push (read (match-string 1 substring)) forms)
             "%s")
           string))
    `(format ,fmt-string ,@(nreverse forms))))

;; list

(defun assoc-for (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun im:plist-merge (&rest plists)
  "Merge all PLISTS to one, latter override early."
  (let ((rtn (copy-sequence (pop plists))) ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq rtn (plist-put rtn (pop ls) (pop ls)))))
    rtn))

(defun im:remove-nth (n list)
  (nconc (cl-subseq list 0 n) (nthcdr (1+ n) list)))

;; file

(defun im:ensure-file (file-name)
  "Create file with name FILE-NAME if not exists."
  (unless (file-exists-p file-name)
    (with-temp-buffer (write-file file-name)))
  file-name)

(defun im:ensure-dir (dir-name)
  "Create dir with name DIR-NAME if not exists."
  (unless (file-exists-p dir-name)
    (make-directory dir-name))
  dir-name)

(defun im:the-file (file &optional default)
  "Use FILE if it exists, else use DEFAULT."
  (when-let* ((f (or (if (file-exists-p file) file) default)))
    (expand-file-name f)))

(defun file-name-parent-directory (dir)
  "Get the parent directory for current DIR."
  (when (and dir (not (equal "/" dir)))
    (file-name-directory (directory-file-name dir))))

(defun im:read-file-content (file &optional callback)
  "Read the FILE content as string, file can be a url."
  (with-temp-buffer
    (if (string-match-p "^\\(http\\|file://\\)" file)
        (url-insert-file-contents file)
      (insert-file-contents file))
    (let ((bs (buffer-substring-no-properties (point-min) (point-max))))
      (if callback (funcall callback bs) bs))))

(defun im:file-mime (file)
  "Return the MIME type of FILE."
  (with-temp-buffer
    (cond
     ((executable-find "file")
      (when (zerop (call-process "file" nil t nil "--mime-type" "--brief" (file-truename file)))
        (string-trim (buffer-string))))
     (IS-WIN ; fallback to query from reg
      (when-let* ((ext (if (string-match-p "\\." file) (file-name-extension file) file)))
        (let ((name (concat "HKEY_LOCAL_MACHINE\\Software\\Classes\\." ext))
              (value "Content Type"))
          (when (zerop (call-process "reg.exe" nil t nil "query" name "/v" value))
            (car (last (split-string (buffer-string)))))))))))

;; buffer

(defun im:thing-at-region-or-point (&optional thing)
  "Return string at point. Region first, THING then."
  (if (use-region-p)
      (if (bound-and-true-p rectangle-mark-mode)
          (string-join (extract-rectangle (region-beginning) (region-end)) "\n")
        (buffer-substring-no-properties (region-beginning) (region-end)))
    (cond ((null thing) nil)
          ((functionp thing) (funcall thing))
          ((symbolp thing) (thing-at-point (or thing 'word) 'no-properties))
          (t thing))))

(defun im:in-comment-p (&optional pos)
  "Test if character at POS is comment.  If POS is nil, character at `(point)' is tested"
  (unless pos (setq pos (point)))
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (or (member 'font-lock-comment-face fontfaces)
        (member 'font-lock-comment-delimiter-face fontfaces))))

(defun im:make-buffer-buriable ()
  (set-buffer-modified-p nil)
  (page-break-lines-mode 1)
  (local-set-key "q" 'bury-buffer))

(defun im:replace-all-in-buffer (list &optional pre-hook)
  "Replace all occurs in current buffer. eg:\n
(im:replace-all-in-buffer '((aaa . bbb) (ccc . ddd)) (lambda () (message xxx))) "
  (if pre-hook (funcall pre-hook))
  (dolist (needle list)
    (goto-char (point-min))
    (while (re-search-forward (car needle) nil t)
      (replace-match (cdr needle)))))

;; others

(defun time-str (&optional time nano)
  "Format TIME to String. if TIME is nil, return current time."
  (format-time-string
   (if nano (concat "%F %T.%" (number-to-string nano) "N") "%F %T")
   time))

(defmacro im:compose (&rest functions)
  "Compose FUNCTIONS like in haskell.
Use macro, can expand to the pretty lambda style."
  (setq functions (nreverse functions))
  (cl-symbol-macrolet ((f (car functions)))
    (let ((form `(apply ,(if (listp f) f `(function ,f)) arguments)))
      (while (setq functions (cdr functions))
        (if (memq (car-safe f) '(function quote)) (setf f (cadr f)))
        (setq form (if (listp f) `(funcall ,f ,form) `(,f ,form))))
      `(lambda (&rest arguments) ,form))))

(defun im:decode-html-entities (html)
  "Decode &lt; style string. ie. html entity."
  (with-temp-buffer
    (save-excursion (insert html))
    (xml-parse-string)))

(defun im:ordered-completion-table (collection &optional sort-fn category)
  (lambda (input pred action)
    (if (eq action 'metadata)
        (let ((r `((display-sort-function . ,(or sort-fn #'identity)))))
          (if category (push (cons 'category category) r)) r)
      (complete-with-action action collection input pred))))

(defun im:capf-functions-of-mode (mode)
  "Get value of `completion-at-point-functions' by MODE."
  (condition-case _
      (with-temp-buffer (funcall mode) completion-at-point-functions)
    (error completion-at-point-functions)))

(defun im:call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(defun im:powershell-execute (command)
  "Execute this string as COMMAND in PowerShell."
  (shell-command (format "powershell -Command \"& {%s}\""
                         (encode-coding-string
                          (replace-regexp-in-string "\n" " " command)
                          (keyboard-coding-system)))))

(defun im:run-choose-command (&rest commands)
  (let ((prompt (if (stringp (car commands)) (pop commands) "Command: ")))
    (call-interactively (intern (completing-read prompt (im:ordered-completion-table commands) nil t)))))

(defun im:execute-command-matching (prompt include &optional exclude)
  "Executing commands matching INCLUDE and not matching EXCLUDE.
INCLUDE and EXCLUDE is string or list. If is list, the element should be regexp
string or command symbol, they will be joined to a single regexp string.
PROMPT is string for minibuffer prompt."
  (declare (indent 1))
  (cl-flet ((norm (items)
              (when items
                (cons (when-let* ((r (cl-remove-if #'symbolp items)))
                        (mapconcat (lambda (r) (format "%s" r)) r "\\|"))
                      (cl-remove-if-not #'symbolp items))))
            (filter (match cmd)
              (when match
                (or (cl-find cmd (cdr match))
                    (if (car match) (string-match-p (car match) (symbol-name cmd)))))))
    (let* ((include (norm (ensure-list include)))
           (exclude (norm (ensure-list exclude)))
           (extended-command-versions
            (if prompt
                (let ((n (copy-tree extended-command-versions)))
                  (setf (caar n) (propertize (caar n) 'display prompt))
                  n)
              extended-command-versions))
           (read-extended-command-predicate
            (lambda (s b)
              (and (command-completion-default-include-p s b)
                   (filter include s)
                   (if exclude (not (filter exclude s)) t)))))
      (call-interactively #'execute-extended-command))))


;;; Logger

(defvar ic/temp-log-buffer " look-the-temp-log")

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
                      collect `(ladd (im:string-interpolation ,norm)))
           (ladd "\n")
           (goto-char (point-max))
           (insert line)
           (when-let* ((w (get-buffer-window (current-buffer) 'visible)))
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
                              `(defmacro ,(intern (format "defun%s" where)) (name params &rest body)
                                 ,(format "Usage: (defun%s function//label (args) body)." where)
                                 (declare (indent defun))
                                 (let* ((fun-arr (split-string (symbol-name name) "//"))
                                        (fun-name (intern (car fun-arr)))
                                        (advice-name (intern (format "imadv:%s" name))))
                                   `(progn
                                      (defun ,advice-name ,params ,@body)
                                      (advice-add #',fun-name ,,where #',advice-name)
                                      (list ',fun-name ,,where ',advice-name))))))))
  (generate-all-defun:where-macros))


;;; Miscellaneous

(defmacro with-over (&rest args) `(with-eval-after-load 'over ,@args))

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat #'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))

(setf (symbol-function 'locate-user-emacs-file-origin)
      (symbol-function 'locate-user-emacs-file))
(advice-add #'locate-user-emacs-file :override
            (defun locate-user-emacs-file$redirect-to-cache-dir (new-name &optional _)
              (expand-file-name new-name (locate-user-emacs-file-origin ".var"))))

(defun loce (subpath &optional nullp) (let ((f (locate-user-emacs-file-origin subpath))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun locc (&optional subpath nullp) (let ((f (locate-user-emacs-file (or subpath "./")))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun loco (subpath &optional nullp) (let ((f (expand-file-name subpath org-directory))) (if (or (not nullp) (file-exists-p f)) f)))

(provide 'util)

;;; util.el ends here
