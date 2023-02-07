;;; ikmd-keys-and-commands.el --- Keys and Commands -*- lexical-binding: t -*-

;;; Code:

(global-unset-key (kbd "C-x i"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-k"))

(bind-keys
 ( [f2]            .  im/kmacros                                  )
 ( [f5]            .  gt-do-translate                             )
 ( [f7]            .  toggle-truncate-lines                       )
 ( [f8]            .  mpvi-seek                                   )
 ( [f9]            .  compile                                     )
 ( [f10]           .  im/silly                                    )
 ( [f11]           .  im/toggle-layout                            )
 ( [(control f11)] .  toggle-frame-fullscreen                     )
 ( [f12]           .  im/local-action                             )

 ( "C-."           .  im/transient-retrieve                       )
 ( "C-x C-r"       .  im/transient-fast                           )
 ( "C-x i"         .  im/transient-goto                           )
 ( "C-x w"         .  im/transient-windows                        )
 ( "C-x /"         .  find-dired                                  )
 ( "C-C /"         .  im/rg-smart-search                          )
 ( "C-c a"         .  org-agenda                                  )
 ( "C-c c"         .  im/transient-agenda                         )
 ( "C-c d"         .  im/transient-dashbox                        )
 ( "C-c C-d"       .  im/transient-dashbox                        )
 ( "C-c g"         .  magit-status                                )
 ( "C-c m"         .  im/local-action                             )
 ( "C-c i"         .  im/transient-insert                         )
 ( "C-c p"         .  org-capture                                 )
 ( "C-c s"         .  f/setup                                     )
 ( "C-c '"         .  edit-indirect-region                        )
 ( "C-c `"         .  im/toggle-diagnostics-buffer                )
 ( "C-c RET"       .  im/dispatch-macroexpand                     )

 ( "%"             .  im/match-paren                              )
 ( "C-#"           .  cua-rectangle-mark-mode                     )
 ( "C-s"           .  im/isearch-regexp                           )
 ( "C-S-s"         .  isearch-forward                             )
 ( "M-w"           .  im/smart-kill-ring-save                     )
 ( "C-y"           .  im/smart-yank                               )
 ( "C-S-y"         .  im/duplicate-lines                          )
 ( "M-y"           .  consult-yank-pop                            ) ; temporary, yank-pop in emacs28 not put the pop item to the front now.
 ( "C-o"           .  im/smart-open-line                          )
 ( "M-q"           .  im/tiny-code                                )
 ( "M-Q"           .  im/tiny-code-buffer                         )
 ( "M-P"           .  previous-buffer                             )
 ( "M-N"           .  next-buffer                                 )
 ( "<C-backspace>" .  im/backward-delete-word                     )
 ( "s-k"           .  (lambda () (interactive) (if IS-MAC (ns-next-frame))))

 ( "<insertchar>"  .  undo                                        )
 ( "C-{"           .  shrink-window                               )
 ( "C-}"           .  enlarge-window                              )

 ( "C-x f"         .  im/smart-find-file                          )
 ( "C-x F"         .  find-dired                                  )
 ( "C-x d"         .  im/project-find-directory                   )
 ( "C-x C-b"       .  ibuffer                                     )
 ( "C-x b"         .  consult-buffer                              )
 ( "C-x l"         .  consult-line                                )
 ( "C-c l"         .  consult-line                                )
 ( "C-x n"         .  im/transient-narrow                         )
 ( "C-x C-o"       .  ace-window                                  )
 ( "C-x 4 b"       .  consult-buffer-other-window                 )
 ( "C-x 5 b"       .  consult-buffer-other-frame                  )
 ( "C-x C-j"       .  ffap                                        )
 ( "C-x C-k"       .  kill-this-buffer                            )
 ( "C-x r N"       .  im/rectangle-number-lines                   )

 ( "M-g g"         .  consult-goto-line                           )
 ( "M-g M-g"       .  consult-goto-line                           )

 ( "M-s m"         .  consult-multi-occur                         )

 ( "M-."           .  xref-find-definitions+citre                 )
 ( "C-,"           .  er-expand-region                            )

 ( "C-h C-h"       .  im/transient-help                           )
 ( "C-h c"         .  quick-calc                                  )
 ( "C-h C-c"       .  calc                                        )
 ( "C-h C"         .  calc-embedded                               )
 ( "C-h g"         .  im/toggle-gnus                              )
 ( "C-h t"         .  im/popup-system-terminal                    )
 ( "C-h C-t"       .  im/popup-system-terminal                    )
 ( "C-h e"         .  im/toggle-eat-buffer                        )
 ( "C-h C-e"       .  im/toggle-eshell-buffer                     )
 ( "C-h s"         .  im/toggle-ielm-buffer                       )
 ( "C-h C-s"       .  im/toggle-scratch-buffer                    )
 ( "C-h w"         .  im/toggle-messages-buffer                   )
 ( "C-h C-w"       .  im/toggle-messages-buffer                   )
 ( "C-h l"         .  im/toggle-common-lisp-dev-buffer            )
 ( "C-h C-l"       .  im/toggle-common-lisp-dev-buffer            ))

(global-set-key [mouse-2] 'mouse-drag-region)
(global-set-key [C-down-mouse-1] 'mouse-drag-region)


;;; Enhanced

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

(defun im/smart-find-file (&optional arg)
  (interactive "P")
  (if (and (project-current nil) (not arg))
      (call-interactively #'project-find-file)
    (call-interactively #'fuzzy-finder)))

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

(defun im:native-open (file)
  "Open FILE with OS native way. If FILE is nil then try to open current buffer-file."
  (when (or (and (not file) (not (buffer-file-name)))
            (and file (not (file-exists-p file))))
    (user-error "File unavailable, do nothing"))
  (let ((f (or file (buffer-file-name))))
    (cond (IS-WIN
           (if (and (executable-find "code")
                    (let* ((ext (file-name-extension f))
                           (mime (im:file-mime f)))
                      (or (member ext '("js")) (and mime (string-match-p "text\\|json\\|xml" mime)))))
               (im/open-current-with-vscode)
             (w32-shell-execute "open" f)))
          (IS-MAC (shell-command (concat "open " (shell-quote-argument f))))
          (IS-G (let ((process-connection-type nil)) (start-process "" nil "xdg-open" f)))
          (t (user-error "Not the supported system")))))

(defun im/open-externally (&optional fname)
  "Open file in OS way."
  (interactive)
  (if (cl-member major-mode '("eshell-mode" "shell-mode" "eat-mode") :test #'string=)
      (im/popup-system-terminal)
    (let ((files
           (if fname (list fname)
             (cond ((string-equal major-mode "dired-mode")
                    (or (dired-get-marked-files) default-directory))
                   (t (list (unless (buffer-file-name)
                              (if (y-or-n-p "No location, goto default dir or QUIT?" )
                                  default-directory
                                (user-error "Do Nothing")))))))))
      (when (or (<= (length files) 5) (y-or-n-p "Open more than 5 files? "))
        (mapc #'im:native-open files)))))

(defun im/open-directory-externally ()
  "Open current directory externally."
  (interactive)
  (im:native-open default-directory))

(defun im/open-current-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))


;;; Mode specific action (C-c m / <f12>)

(defun im/local-action ()
  "Run the command define by `im:local-action' method."
  (interactive)
  (im:local-action major-mode))

(cl-defmethod im:local-action (_default)
  "The general action for mode."
  (let ((bn (intern (format "im/transient-%s" (string-trim (buffer-name))))))
    (if (commandp bn)
        (call-interactively bn)
      (let ((tn (intern (format "im/transient-%s" major-mode))))
        (if (commandp tn)
            (call-interactively tn)
          (message "This is %s, nice to meet you."
                   (propertize (format "%s" major-mode) 'face 'warning)))))))


;;; Commands

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
  (if-let (d default-directory)
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
    (if-let (funcs (completing-read-multiple "Functions (* for all, empty for clean): " all nil nil (which-function)))
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

(provide 'ikmd-keys-and-commands)

;;; ikmd-keys-and-commands.el ends here
