;;; -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

(defmacro lambdi (args &rest body)
  (declare (indent 1))
  `(lambda ,args (interactive) ,@body))

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

(defun im:completion-table (collection &optional sort-fn category)
  (lambda (input pred action)
    (if (eq action 'metadata)
        (let ((r `((display-sort-function
                    . ,(unless (eq t sort-fn) (or sort-fn #'identity))))))
          (if category (push (cons 'category category) r))
          (cons 'metadata r))
      (complete-with-action action collection input pred))))

(defmacro im:with-current-view-buffer (buffer &rest keywords-and-body)
  "Open a dedicated buffer for view. (:focus/:append/:wc/:fontify)."
  (declare (indent 1))
  (let (append focus wc fontify post mode body)
    (setq body
          (cl-loop for lst on keywords-and-body by #'cddr
                   if (and (keywordp (car lst)) (cdr lst))
                   do (pcase (car lst)
                        (:append (setq append (cadr lst)))
                        (:focus (setq focus (cadr lst)))
                        (:wc (setq wc (cadr lst)))
                        (:fontify (setq fontify (cadr lst)))
                        (:post (setq post (cadr lst)))
                        (:mode (setq mode (cadr lst))))
                   else return lst))
    `(with-current-buffer ,(if (stringp buffer) `(get-buffer-create ,buffer) buffer)
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         ,(if append
              `(insert (if (> (point) (point-min)) "\n" ""))
            `(erase-buffer))
         (save-excursion ,@body)
         ,(if (symbolp post) `(eval ,post) post)
         ,(if mode `(funcall ,mode))
         ,@(when fontify
             `((font-lock-add-keywords nil ,fontify)
               (font-lock-flush)))
         (set-buffer-modified-p nil)
         (setq buffer-read-only t)
         (local-set-key "q" #'kill-buffer-and-window)
         (,(if focus 'pop-to-buffer 'display-buffer) (current-buffer) ,wc)))))

(cl-defun im:buffer-view (message &key buffer (focus t) (wc '((display-buffer-below-selected))) fontify post mode)
  "Popup a buffer to display message."
  (declare (indent 1))
  (let ((buf (cond ((stringp buffer) (get-buffer-create buffer))
                   ((bufferp buffer) buffer)
                   (t (get-buffer-create (format "*view-%d*" (float-time)))))))
    (im:with-current-view-buffer buf
      :fontify fontify :post post :wc wc :mode mode
      (insert (if (stringp message) message (format "%s" message))))
    (if focus (pop-to-buffer buf))))

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

(defalias 'loop 'cl-loop)

(defun delq-nil (lst)
  (delq nil lst))

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

(cl-defmacro im/walk-directory ((dir regexp &optional include-directories predicate follow-symlinks) &rest body)
  "Wrapper of `directory-files-recursively', used to batch deal files."
  (declare (indent 1))
  `(cl-loop with its = (directory-files-recursively ,dir ,regexp ,include-directories ,predicate ,follow-symlinks)
            with cnt = (length its) for it in its for idx from 1
            do (cw ">>> ({idx}/{cnt})" it) do ,@body
            finally (cw) (cw "------ Done! ------") (cw)))

;; buffer && window

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
\\=(im:replace-all-in-buffer '((aaa . bbb) (ccc . ddd)) (lambda () (message xxx))) "
  (if pre-hook (funcall pre-hook))
  (dolist (needle list)
    (goto-char (point-min))
    (while (re-search-forward (car needle) nil t)
      (replace-match (cdr needle)))))

(defun im:window-id (&optional window)
  ;; Is there a simpler way to get the window id?
  (unless window
    (setq window (selected-window)))
  (let ((ws (format "%s" window)))
    (string-match "^#<window \\([0-9]+\\)" ws)
    (string-to-number (match-string 1 ws))))

;; other functions

(defun time-str (&optional time nano)
  "Format TIME to String. if TIME is nil, return current time."
  (format-time-string
   (if nano (concat "%F %T.%" (number-to-string nano) "N") "%F %T")
   time))

(defun im:history-replace (hist-sym value)
  "Replace origin history ITEM with this new VALUE."
  (set hist-sym (cdr (symbol-value hist-sym)))
  (add-to-history hist-sym value))

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
    (call-interactively (intern (completing-read prompt (im:completion-table commands) nil t)))))

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


;;; Commands

(defun im/smart-yank ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'im/duplicate-lines)
    (call-interactively #'yank)))

(defun im/smart-kill-ring-save ()
  "Copy-Current-Line-Or-Region."
  (interactive)
  (cond ((eq last-command 'eval-last-sexp)
         (let* ((obj (or elisp-last-eval-sexp-value
                         (eval-expression (preceding-sexp) nil)))
                (res (if current-prefix-arg
                         (pp-to-string obj)
                       (prin1-to-string obj))))
           (kill-new res)
           (message (concat (propertize "Saved: " 'face 'warning) res))))
        ((use-region-p)
         (call-interactively 'kill-ring-save)
         (message "Region Yanked."))
        ((and (eq major-mode 'org-mode)
              (char-equal (char-after (line-beginning-position)) ?#)
              (equal (org-element-type (org-element-at-point)) 'src-block))
         (save-mark-and-excursion
           (org-edit-special)
           (copy-region-as-kill (point-min) (point-max))
           (org-edit-src-exit)
           (message "Src-Block Yanked.")))
        (t
         (copy-region-as-kill (line-beginning-position) (line-end-position))
         (message "No Region, Whole Line Yanked."))))

(defun im/smart-open-line ()
  (interactive)
  (cond ((equal major-mode 'org-mode)
         (call-interactively 'org-return))
        ((looking-at-p "[ \t]*$")
         (newline-and-indent))
        ((looking-back "^[ \t]*" nil)
         (beginning-of-line)
         (open-line 1)
         (indent-for-tab-command))
        (t
         (newline-and-indent)
         (newline-and-indent)
         (forward-line -1)
         (indent-for-tab-command))))

(defun im/backward-word ()
  "Don't back too much."
  (if (string-match-p "^\\W*$" (buffer-substring-no-properties (line-beginning-position) (point)))
      (forward-same-syntax -1)
    (call-interactively 'backward-word)))

(defun im/backward-delete-word ()
  "Don't put into kill ring, don't delete too much."
  (interactive)
  (let ((p (point)))
    (im/backward-word)
    (delete-region (point) p)))

(defun im/rectangle-number-lines ()
  "Interactively fill number for everyline.

The start number and stepper will provide as:

   [empty]            ; then act as the builtin `rectangle-number-lines', from 1 incr by 1
   2                  ; only the start num: start from 2 and incr by 1
   (* it 4)           ; only a form: start from 1, and the next num will eval from this form. \\='it' is special as the prev num
   2 (+ it 3)         ; a start num and then a form: start from 2 and increment by 3
"
  (interactive)
  (require 'rect)
  (dlet ((it 1))
    (let ((stepper '(+ it 1))
          (input (string-trim (read-string "Start number and stepper form (Empty or Number or Form or Number Form): "))))
      (when (cl-plusp (length input))
        (with-temp-buffer
          (insert input)
          (goto-char (point-min))
          (when (looking-at "[0-9]+")
            (setq it (string-to-number (match-string 0)))
            (goto-char (match-end 0))
            (skip-chars-forward " \t"))
          (when (looking-at "(.*) *$")
            (setq stepper (read (match-string 0)))
            (goto-char (match-end 0)))
          (when (looking-at ".+")
            (user-error "Maybe wrong number or stepper form input"))))
      (let ((incr (eval `(lambda () (prog1 it (setq it ,stepper))))))
        (apply-on-rectangle (lambda (beg end fmt)
                              (delete-rectangle-line beg end t)
                              (insert (format fmt (funcall incr))))
                            (region-beginning) (region-end)
                            (read-string "Format string: " "%d"))))))

(defun im/go-to-char (&optional backwardp)
  "Jump to the next CHAR, like `f' in vim, when backwardp is t, then search backward."
  (interactive)
  (let (char n p)
    (setq p (point))
    (push-mark p)
    (setq char (read-char "Go to char: ") n 1)
    (while (or (= char 20)
               (and (> char 31) (< char 127))
               (and (> char 134217776) (< char 134217786)))
      (condition-case nil
          (cond ((= char 20) (message "Turn back: ") (setq backwardp (not backwardp) n 1))
                ((> char 134217776) (setq n (- char 134217776)))
                (t (if backwardp
                       (search-backward (string char) nil nil n)
                     (search-forward (string char) nil nil n))
                   (setq n 1)))
        (error (message "No more...")))
      (setq char (read-char)))
    (cond ((= char 13) (message "Reached."))
          ((= char 23) (kill-region p (point)) (message "Killed to ring."))
          ((= char 134217847) (copy-region-as-kill p (point)) (message "Saved to ring."))
          (t (setq unread-command-events (list last-input-event))))))

(defun im/match-paren (&optional ins)
  "Like `%' in vim."
  (interactive "P")
  (if ins (insert "%")
    (cond ((or (derived-mode-p 'web-mode) (derived-mode-p 'sgml-mode))
           (require 'sgml-mode)
           (cond ((looking-at "<")   (sgml-skip-tag-forward 1))
                 ((looking-back ">" nil) (sgml-skip-tag-backward 1))
                 (t                  (insert "%"))))
          ((looking-at "\\s(\\|\\s{\\|\\s[")     (forward-list))
          ((looking-back "\\s)\\|\\s}\\|\\s\\]" nil) (backward-list))
          (t (insert "%")))))

(defun im/tiny-code ()
  "Indent codes according mode."
  (interactive)
  (cond ((and (eq major-mode 'org-mode) ; org-src-block
              (eq (org-element-type (org-element-context)) 'src-block))
         (org-edit-special)
         (save-mark-and-excursion
           (indent-region (point-min) (point-max)))
         (org-edit-src-exit))
        (t (let (beg end)
             (cond ((use-region-p) ; region
                    (setq beg (region-beginning)
                          end (region-end)))
                   (current-prefix-arg ; whole buffer
                    (setq beg (point-min)
                          end (point-max)))
                   ((string-match-p ")\\|}" (char-to-string (preceding-char))) ; sexp end
                    (save-excursion
                      (setq end (point))
                      (backward-sexp 1)
                      (setq beg (point)))))
             (cond (t (if (and beg end) ; default
                          (indent-region beg end)
                        (indent-according-to-mode))))))))

(defun im/tiny-code-buffer ()
  "Indent codes according mode for whole buffer."
  (interactive)
  (save-excursion
    (call-interactively 'mark-whole-buffer)
    (im/tiny-code)))

(defun ascii-table-show ()
  "Print ASCII table."
  (interactive)
  (with-current-buffer
      (switch-to-buffer "*ASCII table*" 'norecord)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i 0))
      (insert (propertize
               "                         [ASCII table]\n\n"
               'face 'font-lock-comment-face))
      (while (< i 32)
        (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
          (insert (concat
                   (propertize (format "%3d " tmp)
                               'face 'font-lock-function-name-face)
                   (propertize (format "[%2x]" tmp)
                               'face 'font-lock-constant-face)
                   " "
                   (propertize (format "%3s" (single-key-description tmp))
                               'face 'font-lock-string-face)
                   (unless (= tmp (+ 96 i))
                     (propertize "  |  " 'face 'font-lock-variable-name-face)))))
        (newline)
        (setq i (+ i 1)))
      (goto-char (point-min)))
    (im:make-buffer-buriable)
    (local-set-key "Q" 'kill-this-buffer)
    (read-only-mode 1)))

(defun im/pick-lines (&optional regexp)
  "Pick all lines matching the REGEXP in current buffer or region.
ps. builtin `kill-matching-lines' almost do the same thing."
  (interactive)
  (let* ((regionp (use-region-p))
         (re (or regexp (read-string (format "Pick lines containing match for regexp%s: " (if regionp " (region)" ""))))))
    (cl-assert (> (length re) 0) nil "REGEXP is too short!")
    (let ((beg (if regionp (region-beginning) (point-min)))
          (end (if regionp (region-end) (point-max)))
          (marker (prepare-change-group))
          (result-buffer (get-buffer-create "*matched-results*")))
      (unwind-protect
          (save-match-data
            (save-excursion
              (goto-char beg)
              (while (re-search-forward re end t)
                (let ((lbeg (line-beginning-position))
                      (lend (line-beginning-position 2)))
                  (princ (buffer-substring lbeg lend) result-buffer)
                  (unless current-prefix-arg
                    (delete-region lbeg lend))))))
        (undo-amalgamate-change-group marker))
      (with-current-buffer result-buffer
        (local-set-key "q" #'bury-buffer)
        (display-buffer result-buffer)))))

(defun im/duplicate-lines (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies."
  (interactive "*p")
  (let* ((use-region (use-region-p))
         (range (if use-region
                    (list (region-beginning) (region-end))
                  (list (line-beginning-position) (line-end-position))))
         (text (apply #'buffer-substring range)))
    (undo-boundary)
    (dotimes (_ (abs (or n 1)))
      (save-mark-and-excursion
        (goto-char (if use-region (region-end) (line-end-position)))
        (if (derived-mode-p 'org-mode)
            (newline)
          (newline-and-indent))
        (insert text)))
    (when use-region
      (setq deactivate-mark nil)
      (when (= (point) (region-beginning))
        (exchange-point-and-mark)))
    (undo-boundary)))

(defun im/count-words (beg end)
  "Count words in marked region(BEG to END)."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end)
          total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Count Result: %d words(cn: %d, en: %d), %d bytes."
                     total-word cn-word en-word total-byte))))

(defun im/count-lines-in-directory ()
  (interactive)
  (let* ((cs "find . -path ./.git -prune -o -type f -execdir cat {} \\; | wc -l")
         (cmd (read-string "Command: " cs nil cs)))
    (async-shell-command
     (concat "echo `pwd`; echo; " cmd))))

(defun im/clear-comment ()
  "Delete all comments in the buffer."
  (interactive)
  (let (pmin pmax lines kill-ring)
    (if (use-region-p)
        (setq pmin (region-beginning) pmax (region-end))
      (setq pmin (point-min) pmax (point-max)))
    (save-excursion
      (setq lines (count-lines pmin pmax))
      (when lines (goto-char pmin) (comment-kill lines)))))

(defun im/insert-date (&optional timep)
  (interactive "P")
  (insert (format-time-string (if timep "%F %T" "%F"))))

(defun im/yank-current-directory ()
  (interactive)
  (if-let* ((d default-directory))
      (progn
        (kill-new d)
        (message "Yanked: %s" d))
    (user-error "Nothing to copy")))

(defun im/yank-current-buffer-name ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffname (buffer-name)))
    (if filename (kill-new filename))
    (kill-new buffname)
    (message "Yanked: %s" buffname)))

(defun im/yank-current-full-name (e)
  (interactive "e")
  (let ((filename (buffer-file-name (window-buffer (caadr e)))) r)
    (if filename (setq r filename)
      (setq r (buffer-name)))
    (kill-new r)
    (message "Yanked: %s" r)))

(defun im/yank-current-dir-and-buffer-name (e)
  (interactive "e")
  (let ((bn (buffer-name (window-buffer (caadr e))))
        (dn default-directory))
    (kill-new bn)
    (kill-new dn)
    (message "Yanked: %s - %s" bn dn)))

(defun im/bind-key-to-command ()
  (interactive)
  (let ((fn (read-string "Command: " (symbol-name (symbol-at-point)))))
    (if (< (length fn) 4)
        (user-error "Maybe the command is not right")
      (let ((key (read-key-sequence (format "Key for `%s': " fn))))
        (if (and (not (vectorp key)) (< (length key) 2))
            (user-error "Maybe the key is not right")
          (when (y-or-n-p (format "Bind '%s' to command '%s' ?" (key-description key) fn))
            (global-set-key (if (vectorp key) key (kbd key)) (intern fn))
            (message "Bind '%s' to '%s' successfully." fn (key-description key))))))))

(defun im/set-indent-width-or-offset ()
  (interactive)
  (cl-flet ((getn (&optional n) (read-number "Set indent-width/offset to: " n)))
    (cl-macrolet ((cond+ (&rest forms)
                    `(cond ,@(cl-loop for form in forms
                                      if (or (eq (car form) t) (consp (car form))) collect `,form
                                      else collect `((eq major-mode ',(car form))
                                                     (let ((n (getn ,(cadr form))))
                                                       ,@(cl-loop for v in (cdr form) collect `(setq-local ,v n))
                                                       (message "(setq-local %S %d)" ',(if (= 1 (length (cdr form))) (cadr form) (cdr form)) n)))))))
      (cond+ (sgml-mode   sgml-basic-offset)
             (nxml-mode   nxml-child-indent nxml-attribute-indent)
             (mhtml-mode  sgml-basic-offset)
             (js-mode     js-indent-level)
             (css-mode    css-indent-offset)
             (web-mode    web-mode-markup-indent-offset web-mode-css-indent-offset web-mode-code-indent-offset)
             (t (user-error "[%s] Nothing to do with %s" this-command major-mode))))))

(defun im/trailing-whitespace-mode ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show-Trailing-Whitespace: %S" show-trailing-whitespace))

(defun im/find-url-at-point ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (rename-buffer url t)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point-min) (1+ (point)))
        (search-forward "><")
        (replace-match ">\n<")
        (delete-blank-lines))
      (web-mode)
      (set-buffer-multibyte t)
      (pop-to-buffer (current-buffer) nil 'norecord))))

(defun im/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun im/trace-defuns (&optional arg)
  "Output trace info for functions in current buffer.
With ARG not nil for prompt the trace logic."
  (interactive "P")
  (let (all (mkad (lambda (f) (intern (format "$im:trace-%s" f)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^(defun +\\([^ ]+\\)" nil t)
        (push (match-string 1) all)))
    (if-let* ((funcs (completing-read-multiple "Functions (* for all, empty for clean): " all nil nil (which-function))))
        (let ((logic (if arg (read--expression "Logic to run: "))))
          (if (equal (list "*") funcs) (setq funcs all))
          (message "[trace] Advice for trace: %s" funcs)
          (cl-loop for funs in funcs
                   for func = (intern funs)
                   for ad = (funcall mkad func)
                   do (eval `(defun ,ad (f &rest args)
                               (condition-case err
                                   ,(if logic `,logic
                                      `(let ((str (if args (mapconcat (lambda (s) (format "%s" s)) args " ") "")))
                                         (cw "[Trace] {(if (subrp f) (subr-name f) f)}" str)))
                                 (error (message "[trace] %s" err)))
                               (apply f args))
                            t)
                   do (advice-add func :around ad)
                   finally (message "Add traces to %d functions." (length funcs))))
      (cl-loop for funs in all
               for func = (intern funs)
               for ad = (funcall mkad func)
               if (fboundp ad) do (advice-remove func ad) and do (fmakunbound ad) and collect ad into ads
               finally (message "Removed all %d traces." (length ads))))))

(defun im/escape-sequence ()
  "ASII Escape Sequence, used in shell script."
  (interactive)
  (let* ((items '(("default"      . "0")
                  ("Black"        . "30")
                  ("Red"          . "31")
                  ("Green"        . "32")
                  ("Brown/Orange" . "33")
                  ("Blue"         . "34")
                  ("Purple"       . "35")
                  ("Cyan"         . "36")
                  ("Light Gray"   . "37")
                  ("Dark Gray"    . "1;30")
                  ("Light Red"    . "1;31")
                  ("Light Green"  . "1;32")
                  ("Yellow"       . "1;33")
                  ("Light Blue"   . "1;34")
                  ("Light Purple" . "1;35")
                  ("Light Cyan"   . "1;36")
                  ("White"        . "1;37")))
         (color (completing-read "Escape Sequence (\\033[...m): "
                                 (lambda (input pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (category . "escape-sequence")
                                                  (display-sort-function . ,#'identity)
                                                  (annotation-function . ,(lambda (x) (format (format " %%%ss" (- 20 (length x))) (cdr (assoc x items))))))
                                     (complete-with-action action items input pred))) nil t))
         (es (format "\\033[%sm" (cdr (assoc color items)))))
    (when (called-interactively-p 'any)
      (kill-new es)
      (message (concat "> Escape sequence " (propertize es 'face 'font-lock-keyword-face) " is now in kill-ring.")))
    es))

(defun im/describe-major-mode-keymap (&optional mode)
  (interactive)
  (let ((map-sym (intern (format "%s-map" (or mode major-mode)))))
    (if (and (boundp map-sym) (keymapp (symbol-value map-sym)))
        (describe-keymap map-sym)
      (user-error "No keymap found for major mode (%s)" (or mode major-mode)))))

;; Open With

(defun im/open-externally (&optional fname)
  "Open file in OS way."
  (interactive)
  (if (cl-member major-mode '("eshell-mode" "shell-mode" "eat-mode") :test #'string=)
      (im/popup-system-terminal)
    (let ((files (list (or fname
                           (buffer-file-name)
                           (if (y-or-n-p "No location, goto default dir or QUIT?" )
                               default-directory
                             (user-error "Do Nothing"))))))
      (shell-command-do-open files))))

(defun im/open-directory-externally ()
  "Open current directory externally."
  (interactive)
  (shell-command-do-open (list default-directory)))

(defun im/open-current-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))

;; fresh word

(defun im/pick-freshword (word)
  (interactive (list (read-string (format "Save to %s: "
                                          (file-name-nondirectory org-default-words-file))
                                  (im:thing-at-region-or-point))))
  (deactivate-mark)
  (when (zerop (length word))
    (user-error "Word is empty, nothing done"))
  (with-current-buffer (find-file-noselect org-default-words-file)
    (goto-char (point-min))
    (re-search-forward (format org-complex-heading-regexp-format "Picker") nil t)
    (skip-chars-forward " \n")
    (insert (format "- %-35s (%s)\n" word (time-str)))
    (save-buffer))
  (message "[%s] saved." (propertize word 'face 'font-lock-string-face)))

(defun im/view-freshwords ()
  (interactive)
  (with-current-buffer (find-file-noselect org-default-words-file)
    (goto-char (point-min))
    (re-search-forward (format org-complex-heading-regexp-format "Picker") nil t)
    (beginning-of-line)
    (outline-show-subtree)
    (pop-to-buffer (current-buffer))))


;;; Logger

(defvar im.temp-log-buffer " look-the-temp-log")

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
    `(with-current-buffer (get-buffer-create (or ,buffer im.temp-log-buffer))
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

(defun cw-1 () (interactive) (display-buffer (get-buffer-create im.temp-log-buffer)))


;;; Defun & Hooks & Advices

(defmacro defun:when (fun condition &rest args)
  "Define function only when CONDITION is t."
  (declare (indent 3))
  `(when ,condition (cl-defun ,fun ,@args)))

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


;;; Helpers

(defmacro with-over (&rest args) `(with-eval-after-load 'over ,@args))

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat #'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))

(defun:override locate-user-emacs-file//redirect-to-cache-dir (new-name &optional _)
  (when (consp new-name)
    (setq new-name (or (cl-find-if (lambda (item) (not (string-suffix-p ".eld" item))) new-name)
                       (car new-name))))
  (expand-file-name new-name (expand-file-name ".var" user-emacs-directory)))

(defun loce (subpath &optional nullp) (let ((f (expand-file-name subpath user-emacs-directory))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun locc (&optional subpath nullp) (let ((f (locate-user-emacs-file (or subpath "./")))) (if (or (not nullp) (file-exists-p f)) (expand-file-name f))))
(defun loco (subpath &optional nullp) (let ((f (expand-file-name subpath org-directory))) (if (or (not nullp) (file-exists-p f)) f)))

;;; utils.el ends here
