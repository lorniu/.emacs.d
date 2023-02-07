;;; -*- lexical-binding: t -*-

;;; Code:

(defun im/smart-yank ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'im/duplicate-lines)
    (call-interactively #'yank)))

(defun im/smart-kill-ring-save ()
  "Copy-Current-Line-Or-Region."
  (interactive)
  (cond ((use-region-p)
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

(defun im/isearch-regexp ()
  "Isearch+, default with region word, enable regexp."
  (interactive)
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-resume string nil nil t string nil))
    (call-interactively 'isearch-forward-regexp)))

(defun im/rectangle-number-lines ()
  "Interactively fill number for everyline.

The start number and stepper will provide as:

   [empty]            ; then act as the builtin `rectangle-number-lines', from 1 incr by 1
   2                  ; only the start num: start from 2 and incr by 1
   (* it 4)           ; only a form: start from 1, and the next num will eval from this form. 'it' is special as the prev num
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


;;; Open With

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


;;; Miscellaneous

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

(defun im/toggle-lisp-indent-style-locally ()
  "Toggle the current indent-function."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (user-error "Should be invoked in emacs-lisp-mode"))
  (cl-loop for item in '("common-lisp-indent-function" "lisp-indent-function")
           unless (string= item (symbol-name lisp-indent-function))
           collect item into cs
           finally do
           (let ((exp (completing-read "Change to: " cs nil t)))
             (setq-local lisp-indent-function (intern exp))
             (message "Changed to `%s' locally." lisp-indent-function))))

(defun im/trailing-whitespace-mode ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show-Trailing-Whitespace: %S" show-trailing-whitespace))

(defun im/view-url-cursor ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url) 'norecord)
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (1+ (point)))
    (search-forward "><")
    (replace-match ">\n<")
    (delete-blank-lines)
    (set-auto-mode)))

(defun im/youtube-dl-url (&optional url)
  "Run 'youtube-dl' over the URL. If URL is nil, use URL at point."
  (interactive (list
                (let ((up (thing-at-point-url-at-point)))
                  (or up (read-string "youtube url: ")))))
  (if (zerop (length url))
      (user-error "URL not found")
    (let ((eshell-buffer-name "*youtube-dl*")
          (directory (seq-find (lambda (dir)
                                 (and (file-directory-p dir) (expand-file-name dir)))
                               '("~/temp") ".")))
      (eshell)
      (when (eshell-interactive-process) (eshell t))
      (eshell-interrupt-process)
      (insert (format " cd '%s' && youtube-dl " directory) url)
      (eshell-send-input))))

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

(defun im/edit-hosts-file ()
  "Edit hosts file in Linux/Windows."
  (interactive)
  (let ((post (lambda (buf)
                (with-current-buffer buf
                  (when (= (point) 1)
                    (re-search-forward "^[^#]\\{3\\}" nil t)
                    (beginning-of-line)))))
        h1 h2)
    (cond
     ((file-exists-p (setq h1 "/etc/hosts"))
      (funcall post (find-file (concat "/sudo::" h1))))
     ((file-exists-p (setq h2 "C:/Windows/system32/drivers/etc/hosts"))
      (funcall post (find-file-read-only h2))
      (start-process-shell-command (format-time-string "host-%s") nil (concat "wsudo notepad " h2)))
     (t (user-error "No hosts file found for current system")))))

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

(defun im/systemd-new-unit ()
  "Snippet for new systemd unit file."
  (interactive)
  (let* ((dir (expand-file-name
               (read-directory-name "Location: " "/etc/systemd/system/" nil t)))
         (default-directory (if (string-match-p "^/home/" dir) dir
                              (format "/sudo::%s" dir)))
         (name (format "%s.service" (cl-gensym "systemd-unit-"))))
    (unless (file-exists-p default-directory)
      (user-error "Maybe `%s' is not available" default-directory))
    (with-current-buffer (get-buffer-create name)
      (insert "[Unit]
Description=unit 1
After=network.target network-online.target
#Wants/After/Before=docker.service\n
[Service]
#User/Group=vip
#Type=simple/forking/oneshot/notify/dbus/idle
#Environment=PS1=1
ExecStart=/usr/bin/x
#ExecReload=/bin/kill -HUP $MAINPID
#ExecStartPre/ExecStartPost/ExecStop/ExecStopPost=
#Restart/RestartSec/PIDFile/TimeoutSec/KillMode\n
[Install]
WantedBy=multi-user.target")
      (systemd-mode)
      (pop-to-buffer (current-buffer)))))

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

;; agenda

(defun im/org-agenda-lines ()
  "Read and goto headline in `org-agenda-files'."
  (interactive)
  (consult-org-agenda "+LEVEL<3+ITEM<>{^[<\\[]}-Archived-elfeed-Config")
  (im:org-show-plain-text-and-children-headlines))

(defun im/org-agenda-files ()
  "Fast open file in `org-agenda-files'."
  (interactive)
  (let ((f (completing-read "Agenda files: " (im:ordered-completion-table org-agenda-files nil "file") nil t)))
    (find-file f)
    (when (= (point) 1)
      (re-search-forward "^\\*+ " nil t)
      (beginning-of-line))))

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
