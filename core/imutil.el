;;; imutil.el --- Functions And Utils
;;; Commentary:

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
  `(add-hook ,hook (lambda () (wn ,@body))))

(defmacro wn (&rest expr)
  "Wrap hook code for not running when org-mode publish...,
eg. disable some minor modes,
I do not know whether there is better idea."
  `(when (or (not (boundp 'org-publishing)) (not org-publishing)) ,@expr))


;;; Helpers

(defmacro pm (expr)
  `(pp (macroexpand-1 ',expr)))

(defun ppp (list)
  (dolist (l list t) (princ l) (terpri)))

(defmacro mmm (&rest expr)
  `(message ,@expr))


;;; Utility

(defun im/proxy (&optional args)
  (interactive "P")
  (if (and args (not (= args 1)))
      (setq url-gateway-method 'native)
    (setq url-gateway-method 'socks)
    (setq socks-server '("Default server" "127.0.0.1" 1080 5))))

(defun time (&optional time nano)
  "Format TIME to String. if TIME is nil, return current time."
  (format-time-string
   (if nano (concat "%F %T.%" (number-to-string nano) "N") "%F %T")
   time))

(defun padding-left-to-string (item &optional needle)
  "Padding every line of ITEM with NEEDLE. If ITEM is a list, then join it with padding."
  (mapconcat (lambda (s) (concat (or needle "  ") s))
             (if (listp item) item (split-string item "\n")) "\n"))

(defun string-repeat (init times)
  "Make a new string repeat TIMES for INIT."
  (apply 'concat (make-list times init)))

(defun string-join-newline (&rest strings)
  "Join list STRINGS with newline, return one String."
  (mapconcat 'identity strings "\n"))

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

(defun wy-go-to-char (n char)
  "Jump to the Nth CHAR inline, as `f' in vim."
  (interactive "p\ncGo to char:")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

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

(defun im/trans-word (word)
  "Translate WORD with YouDao online."
  (interactive (list
                (let ((w (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (current-word))))
                  (if current-prefix-arg
                      (read-string "Word to translate: " w)
                    w))))
  (defvar im/trans-limit 5)
  (message "Translating \"%s\"..." word)
  (if (or (not word) (< (length word) 2)
          (string-match-p "[a-z] +[a-z]+" word))
      (error "Invalid input, long or short?"))
  (let ((link "http://m.youdao.com/dict?le=eng&q=")
        (url-request-extra-headers
         '(("Content-Type" . "text/html;charset=utf-8"))))
    (url-retrieve
     (concat link (url-encode-url word))
     (lambda (status word)
       (if (plist-get status :error)
           (error (plist-get status :error)))
       (let* ((num -1)
              (regstr "<[^>]*>\\(.+?\\)</.+>")
              (pnetic (progn
                        (unless (re-search-forward
                                 "#\\(ce\\)\\|\\(ec\\)\"" nil t)
                          (error "No Such Word Found [%s]" word))
                        (re-search-forward
                         "=\"phonetic\">\\(.+\\)</span>" nil t 2)
                        (string-as-multibyte
                         (or (match-string 1) ""))))
              (pnetic (propertize pnetic 'face 'font-lock-string-face))
              (beg (re-search-forward "<ul>" nil t))
              (end (re-search-forward "</ul>" nil t))
              (str (propertize word 'face '(:foreground "red")))
              (res (list (concat str " " pnetic))))
         (goto-char beg)
         (while (and (< (setq num (1+ num)) im/trans-limit)
                     (re-search-forward regstr end t))
           (setq str (string-as-multibyte
                      (or (replace-regexp-in-string
                           "<.+?>" ""
                           (match-string-no-properties 1)) "")))
           (string-match "^\\([a-z.]*\\)\\(.*\\)" str)
           (setq str (concat " "
                             (propertize (match-string 1 str) 'face '(:foreground "blue"))
                             (match-string 2 str)))
           (add-to-list 'res str t))
         (message (mapconcat #'identity res "\n"))
         (kill-buffer)))
     (list word) t t)))

(defun try-expand-slime (old)
  "Hippie Expand OLD word for slime."
  (when (not old)
    (he-init-string (slime-symbol-start-pos) (slime-symbol-end-pos))
    (setq he-expand-list
          (or (equal he-search-string "")
              (sort (slime-simple-completions he-search-string) #'string-lessp))))
  (if (null he-expand-list)
      (progn (if old (he-reset-string)) ())
    (he-substitute-string (car he-expand-list))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    (message "Slime Expand") t))

(defun im/copy-current-line ()
  "Copy-current-line-or-region."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (message "Copied")))

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

(defun im/find-ft (&rest names)
  "Find the proper font in NAMES."
  (catch 'ret
    (dolist (name names)
      (let ((full-name (-im/find-font-in-sys name)))
        (if full-name
            (throw 'ret full-name))))))

(defun -im/find-font-in-sys (name &optional filter)
  (let* ((fs (sort (x-list-fonts name) 'string-greaterp)))
    (seq-find (lambda (f) (string-match-p (format ".*%s.*" (or filter "8859-1")) f)) fs)))

(defun im/start-server ()
  "Wrapper for Start Emacs server."
  (require 'server)
  (setq server-auth-dir "~/.emacs.d/.cache/server/")
  (ignore-errors (delete-file (concat server-auth-dir "server")))
  (server-start))

(defmacro im/open-file-view (file &rest args)
  "Open a file with View-Mode."
  `(progn (find-file ,file ,@args) (view-mode +1)))

(defmacro im/with-mode-silent (modes &rest something)
  "Doing SOMETHING with some MODES absent."
  (declare (indent 1))
  (unless (listp modes) (setq modes (list modes)))
  `(progn
     ,@(mapcar (lambda (m) `(,m -1)) modes)
     ,@something
     ,@(mapcar (lambda (m) `(,m +1)) modes)))

(defmacro im/profile (&rest body)
  "Profiler BODY form."
  `(progn (profiler-start 'cpu)
          (ignore-errors ,@body (profiler-report))
          (profiler-stop)))


;;; Miscellaneous

(defun im/toggle-fullscreen ()
  "Toggle fullscreen, use [F12] to save the window configuration before delete windows."
  (interactive)
  (if (> (count-windows) 1)
      (progn (window-configuration-to-register 99)
             (delete-other-windows))
    (if (get-register 99)
        (jump-to-register 99)
      (message "never save window configurations"))))

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


(provide 'imutil)

;;; imutil.el ends here
