;;; imutil.el --- Functions And Utils
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))


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

(defmacro pm (expr)
  `(pp (macroexpand-1 ',expr)))

(defun ppp (list)
  (dolist (l list t) (princ l) (terpri)))

(defmacro mmm (&rest expr)
  `(message ,@expr))

(defun im/patch () (load "patches" nil nil))

(defun im/proxy (&optional args)
  (interactive "P")
  (if (and args (not (= args 1)))
      (setq url-gateway-method 'native)
    (setq url-gateway-method 'socks)
    (setq socks-server '("Default server" "127.0.0.1" 1080 5))))

(defmacro im/profile (&rest body)
  "Profiler BODY form."
  `(progn (profiler-start 'cpu)
          (ignore-errors ,@body (profiler-report))
          (profiler-stop)))

(defun insert-here (content)
  (save-excursion (insert (format "\n%s" content))))

(defmacro append-local (where &rest what)
  `(progn
     (make-variable-buffer-local ,where)
     ,@(mapcar (lambda (w) `(add-to-list ,where ,w)) what)))


;;; Hack

(defun im/yank-more ()
  "Copy-Current-Line-Or-Region."
  (interactive)
  (if (use-region-p)
      (progn
        (call-interactively 'kill-ring-save)
        (message "Region Yanked."))
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (message "No Region, Whole Line Yanked.")))

(defun im/open-line ()
  (interactive)
  (cond
   ((looking-at-p "[ \t]*$")
    (newline-and-indent))
   ((looking-back "^[ \t]*")
    (beginning-of-line)
    (open-line 1)
    (indent-for-tab-command))
   (t
    (newline-and-indent)
    (newline-and-indent)
    (forward-line -1)
    (indent-for-tab-command))))

(defun im/backward-kill-word ()
  (interactive)
  (let ((init-pos (point))
        (back-pos (let ((csb (char-syntax (char-before))))
                    (if (or (= csb 34) (= csb 46)) (backward-char))
                    (forward-same-syntax (if (= csb 32) -2 -1))
                    (point))))
    (delete-region back-pos init-pos)))

(defun im/isearch-regexp ()
  "Isearch+, default with region word, enable regexp."
  (interactive)
  (if (use-region-p)
      (progn
        (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (isearch-resume string nil nil t string nil)))
    (call-interactively 'isearch-forward-regexp)))

(defvar messaging-on t "Control whether to print message to minibuffer")
(defun -my/message-switch (f &rest args) (when messaging-on (apply f args)))
(advice-add 'message :around '-my/message-switch)


;;; Commands

(defun im/view-url-cursor ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (1+ (point)))
    (search-forward "><")
    (replace-match ">\n<")
    (delete-blank-lines)
    (set-auto-mode)))

(defun grep-cursor (word)
  "Grep the current WORD in the files."
  (interactive (list (if (use-region-p)
                         (buffer-substring (region-beginning) (region-end))
                       (current-word))))
  (if (or (not word) (< (length word) 3))
      (message "word not available")
    (let* ((oldcmd grep-find-command)
           (ext (aif (file-name-extension (buffer-name))
                  (concat "." it) ""))
           (newcmd (format "find . -maxdepth 1 -type f -name '*%s' -exec grep -nH -e '%s' {} + " ext word)))
      (unwind-protect
          (progn
            (grep-apply-setting 'grep-find-command newcmd)
            (call-interactively 'grep-find))
        (grep-apply-setting 'grep-find-command oldcmd)))))

(defun im/tiny-code ()
  "Indent codes according mode."
  (interactive)
  (cond ((use-region-p) (indent-region (region-beginning) (region-end)))
        ((string-match-p ")\\|}" (char-to-string (preceding-char)))
         (let ((point-ori (point)))
           (backward-sexp 1)
           (indent-region (point) point-ori)
           (forward-sexp 1)))
        (t (indent-according-to-mode))))

(defun im/tiny-code-buffer ()
  "Indent codes according mode for whole buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (im/tiny-code)))

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

(defun his-match-paren (n)
  "Like `%' in vim. N is self-insert times."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(]" next-char) (forward-sexp 1))
          ((string-match "[\]})]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or n 1))))))

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

(defun ascii-table-show ()
  "Print ASCII table."
  (interactive)
  (with-current-buffer
      (switch-to-buffer "*ASCII table*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i 0) (tmp 0))
      (insert (propertize
               "                         [ASCII table]\n\n"
               'face font-lock-comment-face))
      (while (< i 32)
        (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
          (insert (concat
                   (propertize (format "%3d " tmp)
                               'face font-lock-function-name-face)
                   (propertize (format "[%2x]" tmp)
                               'face font-lock-constant-face)
                   " "
                   (propertize (format "%3s" (single-key-description tmp))
                               'face font-lock-string-face)
                   (unless (= tmp (+ 96 i))
                     (propertize "  |  " 'face font-lock-variable-name-face)))))
        (newline)
        (setq i (+ i 1)))
      (goto-char (point-min)))
    (local-set-key "q" 'bury-buffer)
    (local-set-key "Q" 'kill-this-buffer)
    (read-only-mode 1)))

(defun resume-scratch ()
  "This sends you to the *Scratch* buffer."
  (interactive)
  (let ((eme-scratch-buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer eme-scratch-buffer)
    (funcall initial-major-mode)))

(defun im/copy-lines (&optional direction)
  "Copy lines down/up, like in Eclipse."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (goto-char (setq end (line-end-position)))
        (kill-ring-save beg end)
        (newline)
        (yank)
        (if direction (goto-char end)))
    (kill-ring-save (line-beginning-position) (line-end-position))
    (end-of-line)
    (newline)
    (yank)
    (forward-line -1)))

(defun im/kill-lines ()
  "Fast move/del like in eclipse."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (goto-char (setq end (line-end-position)))
        (kill-region beg end))
    (kill-whole-line)))

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

(defun im/toggle-dedicated ()
  "Whether the current active window is dedicated."
  (interactive)
  (face-remap-add-relative
   'mode-line-buffer-id
   (if (let ((window (get-buffer-window (current-buffer))))
         (set-window-dedicated-p
          window
          (not (window-dedicated-p window))))
       '(:foreground "red")
     '(:foreground "black")))
  (current-buffer))

(defun iv/normalize-gradle-dependency ()
  (interactive)
  (replace-regexp "group: " "" nil (line-beginning-position) (line-end-position))
  (replace-regexp "', name: '\\|', version: '" ":" nil (line-beginning-position) (line-end-position))
  (beginning-of-line))


;;; Miscellaneous

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

(defun im/replace-all-in-buffer (list &optional pre-hook)
  "Replace all occurs in current buffer. eg:\n
(im/replace-all-in-buffer '((aaa . bbb) (ccc . ddd)) (lambda () (message xxx))) "
  (if pre-hook (funcall pre-hook))
  (dolist (needle list)
    (goto-char (point-min))
    (while (re-search-forward (car needle) nil t)
      (replace-match (cdr needle)))))

(defun im/find-ft (&rest names)
  "Find the proper font in NAMES."
  (catch 'ret
    (if (listp (car names))
        (setq names (car names)))
    (let ((fonts (font-family-list)) res-font)
      (dolist (name names)
        (setq rs-font (seq-find (lambda (font) (string-match-p (format ".*%s.*" name) font)) fonts))
        (if rs-font (throw 'ret rs-font))))))

(defun im/mono-font-for-buffer (&optional font-height)
  "Return the Mono Font can be used."
  (interactive "sFont Height: ")
  (let ((face (im/find-ft im/probe-mono-fonts)) face-plist)
    (if (null face)
        (message "No proper mono fonts found")
      (setq face-plist `(:family ,face))
      (setq font-height (or font-height im/mono-buffer-height))
      (if (stringp font-height) (setq font-height (string-to-number font-height)))
      (if (and font-height (> font-height 0))
          (nconc face-plist `(:height ,font-height)))
      (setq buffer-face-mode-face face-plist)
      (buffer-face-mode))))

(defun im/start-server ()
  "Wrapper for Start Emacs server."
  (require 'server)
  (setq server-auth-dir "~/.emacs.d/.cache/server/")
  (ignore-errors (delete-file (concat server-auth-dir "server")))
  (server-start))

(defmacro im/open-file-view (file &rest args)
  "Open a file with View-Mode."
  `(progn (find-file ,file ,@args) (view-mode +1)))

(provide 'imutil)

;;; imutil.el ends here
