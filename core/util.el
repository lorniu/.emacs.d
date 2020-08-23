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

(defmacro pm (expr)
  `(progn (pp (macroexpand-1 ',expr)) t))

(defmacro im/profile (&rest body)
  "Profiler BODY form."
  `(progn (profiler-start 'cpu)
          (ignore-errors ,@body (profiler-report))
          (profiler-stop)))


;;; Utils

(defun list-nn (&rest elements)
  "Make ELEMENTS to list, ignore nil."
  (cl-loop for e in elements when (not (null e)) collect e))

(defun ppl (list)
  "Print print LIST"
  (dolist (l list t) (princ l) (terpri)))

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

(defun powershell-execute (cmd)
  "Execute this string as command in PowerShell."
  (shell-command (format "powershell -Command \"& {%s}\""
                         (encode-coding-string
                          (replace-regexp-in-string "\n" " " cmd)
                          (keyboard-coding-system)))))

(defun join-as-regor-group (&rest items)
  "Join ITEMS as \\(aaa\\)\\|\\(bbb\\) style."
  (mapconcat (lambda (x) (format "\\(%s\\)" x)) items "\\|"))

(defun current-vm-p ()
  "Judge if this is a VM host. Can used in BSD now."
  (cl-find-if (lambda (x) (string-prefix-p "vtnet" (car x)))
              (network-interface-list)))


;;; Wrap

(defmacro add-hook-fun (hook params &rest body)
  "Usage: (add-hook-fun hook-name/tag () xxx), tag can be ignored."
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


;;; Help/Tip/Info

(defun log/it (&rest args)
  "Output messsage to a buffer. Usage: (log/it [buffer-name] fmtString variable)."
  (let ((buffer-name (if (symbolp (car args))
                         (prog1 (symbol-name (car args))
                           (setq args (cdr args)))
                       "*logger*")))
    (with-current-buffer (get-buffer-create buffer-name)
      (local-set-key (kbd "q") 'bury-buffer)
      (goto-char (point-max))
      (insert "\n")
      (insert (apply 'format args)))))

(defmacro deftips (name args format-string &rest str-args)
  "Open an org buffer to show the information.
Plist ARGS can be :buffer/line/pre/post/startup/title/notitle."
  `(defun ,name ()
     (interactive)
     (let ((bn ,(or (plist-get args :buffer) "*Help-Tip*")))
       (with-output-to-temp-buffer bn
         (with-current-buffer bn
           (let ((inhibit-read-only t))
             ,(aif (plist-get args :pre) `(insert ,(format "%s\n" it)))
             (insert ,(cl-loop for startup in (aif (plist-get args :startup) (if (listp it) it (list it)) nil)
                         concat (format "#+STARTUP: %s\n" startup)))
             ,(unless (plist-get args :notitle)
                `(insert ,(format "#+TITLE: %s\n\n" (or (plist-get args :title) name))))
             (insert
              ,(if (null str-args) format-string
                 (apply #'format format-string
                        (cl-loop for sa in str-args
                           for s = (eval sa)
                           collect (if (string-match "^\\(.*\\) \\(-+\\)$" s)
                                       (concat (match-string 1 s)
                                               (cl-loop for i below (length (match-string 2 s)) concat "\n"))
                                     s)))))
             ,(aif (plist-get args :post) `(progn ,it))
             (set-buffer-modified-p nil)
             (goto-char (point-min))
             (forward-line ,(or (plist-get args :line) 4)))
           (org-mode)
           (view-mode 1)
           (make-local-variable 'view-mode-map)
           (define-key view-mode-map "q" 'View-kill-and-leave)
           (pop-to-buffer bn))))))

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat 'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))

(defmacro defreference (name &rest refs)
  "Make a function of NAME to browse the REFS."
  (let* ((flatten (cl-loop for item in refs
                     if (stringp item) collect item
                     if (listp item) append
                       (if (stringp (car item))
                           (cl-loop for r in item collect (eval r))
                         (list (eval item)))))
         (consed (if (null flatten) (user-error "No suitable reference.")
                   (cl-loop for item in flatten
                      if (string-match "^\\(.*\\): \\(.*\\)$" item) collect
                        (cons (match-string 2 item) (match-string 1 item))
                      else collect (cons item nil))))
         (formatted (cl-loop for item in consed
                       for ref = (car item)
                       if (not (string-prefix-p "http" ref)) do
                         (setq ref (format "https://github.com/%s" ref))
                       collect (cons ref (cdr item))))
         (propertized (cl-loop with face = 'font-lock-doc-face
                         for item in formatted
                         for label = (cdr item)
                         if label do
                           (let ((len (cl-loop for i in formatted if (cdr i) maximize (1+ (length (car i))))))
                             (setq label (format (format "%%-%ds (%%s)" len) (car item) label))
                             (add-face-text-property (length (car item)) (length label) face nil label))
                         else do
                           (setq label (car item))
                         collect label))
         (fun (intern (format (concat (unless (string-match-p "/" (symbol-name name)) "ref/") (if (cdr flatten) "%s*" "%s")) name))))
    `(defun ,fun ()
       ,(format "%s\n\nBrowser/Yank it." propertized)
       (interactive)
       (let* ((refs ',propertized)
              (selectrum-should-sort-p nil)
              (selectrum-minibuffer-bindings (append selectrum-minibuffer-bindings
                                                     `(("M-w" . ,(selectrum-make-action (ref)
                                                                   (setq ref (car (split-string ref " ")))
                                                                   (kill-new ref)
                                                                   (message "Copy REF: %s" ref))))))
              (ref (car (split-string (completing-read "REF: " refs nil t) " "))))
         (browse-url ref)))))


(provide 'util)

;;; imutil.el ends here
