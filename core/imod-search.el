;;; imod-search.el --- Search and replace -*- lexical-binding: t -*-

;;; Code:

(x isearch
   :config
   (setq isearch-lazy-count t) ; replace Anzu
   (setq isearch-allow-motion t)
   (define-key isearch-mode-map "\C-c" 'isearch-toggle-regexp)
   (define-key isearch-mode-map "\C-e" 'isearch-edit-string))

(x replace
   :config
   (global-set-key (kbd "M-s o") ; Word at-point first
                   (defun im/occur (str)
                     (interactive (list (im:thing-at-region-or-point)))
                     (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))
   (global-set-key (kbd "M-s O") ; use 'rg' as backend
                   (defun moccur (str)
                     (interactive (list (im:thing-at-region-or-point)))
                     (rg (read-from-minibuffer "Multi-Occurs: " str nil nil 'regexp-history)
                         (rg-read-files)
                         (read-directory-name "In directory: " nil default-directory t))
                     (if-let (it (get-buffer-window "*rg*")) (select-window it))))
   (defun:after occur//follow (&rest _) (if-let (it (get-buffer-window "*Occur*")) (select-window it))))

(x rg
   "pacman -S ripgrep"
   :ref ("dajva/rg.el"
         "ripgrep download page: BurntSushi/ripgrep/releases")
   :init
   (setq rg-group-result nil)
   (setq rg-ignore-case 'smart)
   (setq rg-align-column-number-field-length 4)

   :config
   (defun im/rg-rerun-toggle-group ()
     "Toggle show group in `rg-cur-search`."
     (interactive)
     (setq rg-group-result (not rg-group-result))
     (rg-rerun))

   (define-key rg-mode-map "G" 'im/rg-rerun-toggle-group))

(x fuzzy-finder
   "pacman -S fzf"
   :ref "10sr/fuzzy-finder-el"
   :config
   (setq fuzzy-finder-executable "fzf"))

(defun im/grep-cursor (word)
  "Grep the current WORD in the files."
  (interactive (list (im:thing-at-region-or-point)))
  (if (or (not word) (< (length word) 3))
      (message "word not available")
    (let* ((oldcmd grep-find-command)
           (ext (if-let (it (file-name-extension (buffer-name))) (concat "." it) ""))
           (newcmd (format "find . -maxdepth 1 -type f -name '*%s' -exec grep -nH -e '%s' {} + " ext word)))
      (unwind-protect
          (progn
            (grep-apply-setting 'grep-find-command newcmd)
            (call-interactively 'grep-find))
        (grep-apply-setting 'grep-find-command oldcmd)))))


;;; C-c /  [input]  [n input]  [.*.el input]  [n.el input]  [w.dotnet* headline]

(defcustom ic/rg-groups
  `(("e"   (loce ""))
    ("a"   agenda-directory)
    ("n"   org-directory)
    ("s"   ic/srcdir "ic/srcdir not config")
    ("w"   ic/workdir "ic/workdir not config")
    ("p"   (project-root (project-current)) "No project found"))
  "Groups used by `im/rg-smart-search'. Format: (group-char search-dir prompt-when-non-exist*)+"
  :group 'imfine
  :type 'list)

(defvar im:rg-candicates nil)

(defvar im:rg-search-history nil)

(defun im:rg-dispatch-to-rg.el ()
  "Use rg.el to show the results in Occur buffer."
  (interactive)
  (require 'rg)
  (require 'compile)
  (im:exit-minibuffer-with-message (c (im:completion-compat :current))
    (pcase-let* ((`(_ ,regexp ,dir ,oinput) im:rg-candicates)
                 (rg-required-command-line-flags (if-let (glob (plist-get oinput :glob))
                                                     (cons (concat "--glob='" glob "'") rg-required-command-line-flags)
                                                   rg-required-command-line-flags)))
      ;; dispatch query to rg.el
      (rg regexp "*" dir)
      ;; jump to current candidate in the *rg* buffer.
      (with-current-buffer (pop-to-buffer (rg-buffer-name))
        (setq-local compilation-finish-functions
                    (list
                     (lambda (_a _b)
                       (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                         (let ((file-name (replace-regexp-in-string "^[\\./]*" "" (cl-subseq (match-string-no-properties 1 c) (plist-get oinput :beg))))
                               (line-number (match-string-no-properties 2 c)))
                           (if rg-group-result
                               (progn
                                 (search-forward (format "File: %s\n" file-name) nil t)
                                 (re-search-forward (format "^ *%s" line-number) nil t))
                             (search-forward (format "%s:%s:" file-name line-number) nil t))
                           (beginning-of-line)))))))
      ;; save input to history
      (when-let (item (plist-get oinput :origin))
        (im:history-replace 'im:rg-search-history item)))))

(defun im:rg-parse-input (input)
  "[input]  [n input]  [.*.el input]  [n.el input]  [w.dotnet* headline]"
  (let* ((beg 0) grp ret glob)
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (when (looking-at (format "\\([%s]\\)\\(?: +\\|\\. *\\)" (mapconcat #'car ic/rg-groups "")))
        (setq grp (match-string 1))
        (forward-char 1)
        (if (equal (char-after) ? ) (skip-chars-forward " "))
        (setq beg (- (point) 1)))
      (when (looking-at "\\.\\([^ ]+\\) *")
        (setq beg (- (match-end 0) 1))
        (setq glob (match-string 1))
        (cond ; some sugars
         ((string-match-p "^[a-z][a-z0-9]+$" glob) (setq glob (concat "*." glob))) ; .ext
         ((string-match-p "^[a-zA-Z0-9_-]+\\*$" glob) (setq glob (concat "**/*" glob "/**"))))) ; .dir*
      (when (>= (- (length (encode-coding-string input 'utf-8)) beg) 3)
        (setq ret (cl-subseq input beg)))
      (list :grp grp :beg beg :glob glob :input ret :origin input))))

(defun im:rg-candicates (in &optional dir)
  ;; return: (lines regexp dir in)
  ;; example: (length (car (im:rg-candicates (list :input "require.*" :glob "*.el"))))
  (let ((default-directory (file-truename (or dir default-directory))))
    (if (or (string= "/" default-directory)
            (string-match-p"^.:[/\\]$" default-directory)
            (string= (file-name-as-directory default-directory) (file-name-as-directory (getenv "HOME"))))
        "Should not search from root or home directory"
      (when-let (input (plist-get in :input))
        (let* ((regexp (condition-case nil (rxt-elisp-to-pcre input) (error input)))
               (glob (plist-get in :glob))
               (command (grep-expand-template
                         "rg --no-heading --no-config --color=never [.^.] -n -S -z -e <R> ."
                         regexp))
               (extra (if-let (g (plist-get in :glob)) (concat "-g '" g "'") ""))
               (ret (with-temp-buffer
                      (insert command)
                      (goto-char (point-min))
                      (search-forward "[.^.]")
                      (delete-region (point) (match-beginning 0))
                      (insert extra)
                      (setq command (buffer-string))
                      (erase-buffer)
                      (process-file-shell-command command nil (current-buffer))
                      (split-string (buffer-string) "\n"))))
          (cl-loop for line in ret
                   if (string-match "\\`\\([^:]+\\):\\([^:]+\\):" line)
                   do (progn (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil line)
                             (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil line))
                   and collect line into lines
                   finally (return (list lines regexp default-directory in))))))))

(defun im:rg-completion-table (input _pred action)
  (pcase action
    ('metadata
     '(metadata (category . ripgrep)
                (cycle-sort-function . identity)
                (display-sort-function . identity)))
    (`(boundaries . ,suffix)
     (let* ((parsed (im:rg-parse-input (concat input suffix)))
            (bds (if (plist-get parsed :input)
                     (cons (min (length input) (plist-get parsed :beg)) (length suffix))
                   (cons (length input) 0))))
       `(boundaries . ,bds)))
    (_
     (condition-case err
         (let* ((parsed (im:rg-parse-input (minibuffer-contents-no-properties)))
                (group (assoc (plist-get parsed :grp) ic/rg-groups)) collection)
           (when group
             (add-face-text-property (minibuffer-prompt-end) (+ 1 (minibuffer-prompt-end)) 'font-lock-warning-face))
           (setq im:rg-candicates (if group
                                      (let ((dir (eval (cadr group))))
                                        (if (and dir (file-directory-p dir))
                                            (im:rg-candicates parsed dir)
                                          (or (caddr group) "Directory not existed")))
                                    (im:rg-candicates parsed)))
           (if (stringp im:rg-candicates) (error im:rg-candicates)
             (setq collection (car im:rg-candicates))
             (if (null collection) (error "Nothing found...")
               (complete-with-action action collection "" nil))))
       (error (setq im:rg-candicates nil) (cdr err))))))

(defun im/rg-smart-search ()
  "Search with ripgrep smartly.

Default search current directory, if input begin with 'p ' then search current project,
if begin with 'n ' then search org-directory. See `ic/rg-groups' for others.

Put .glob to set the filter of files. Glob should be the syntax like .gitignore.

For example:

  n.**/*.org sometext  or  n.org sometext

will search org files in `org-directory' for 'sometext'.

Use [C-c C-o] to dispatch the results to rg.el's Occur view, there you can do further edit.

Make sure package `rg' is installed."
  (interactive)
  (unless (executable-find "rg") (user-error "Ripgrep must be installed"))
  (require 'grep)
  (require 'xref)
  (require 'pcre2el)
  (require 'consult)
  (let* ((word (im:thing-at-region-or-point
                (lambda ()
                  (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                    (if (and sym (> 50 (length symn) 3)) symn nil)))))
         (current (minibuffer-with-setup-hook
                      (lambda ()
                        (setq im:rg-candicates nil)
                        (let ((map (make-composed-keymap nil (current-local-map))))
                          (define-key map (kbd "M-w") (lambda ()
                                                        (interactive)
                                                        (when-let (c (ignore-errors (cadr (split-string (im:completion-compat :current) "[0-9]:"))))
                                                          (kill-new c)
                                                          (throw 'miniquit 'Copy))))
                          (define-key map (kbd "C-c C-o") #'im:rg-dispatch-to-rg.el)
                          (use-local-map map)))
                    (let ((completion-styles '(orderless))
                          (prompt (format "Candidates [%s]: " (string-replace " " "" (mapconcat #'car ic/rg-groups "")))))
                      (catch 'miniquit
                        (completing-read prompt #'im:rg-completion-table nil nil word 'im:rg-search-history)))))
         (input (cl-fourth im:rg-candicates)))
    (if (symbolp current) (user-error "%s done" current))
    (when-let (h (plist-get input :origin)) ; history item
      (im:history-replace 'im:rg-search-history h))
    (setq current (cl-subseq current (plist-get input :beg))) ; remove prefix 'grp.glob ' in the result line
    (if (string-match (format "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'") current)
        (let ((file-name (match-string-no-properties 1 current))
              (line-number (match-string-no-properties 2 current)))
          (xref-push-marker-stack) ; use M-, to go back!
          (setq isearch-string (plist-get input :input))
          (isearch-update-ring isearch-string t)
          (find-file (expand-file-name file-name (cl-third im:rg-candicates)))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward isearch-string (point-at-eol) t)
          (recenter)
          ;; notify line
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'face '(:background "#aa3344"))
            (run-with-timer 0.3 nil (lambda () (delete-overlay ov)))))
      (message "Bad candidate?"))))

(provide 'imod-search)

;;; imod-search.el ends here
