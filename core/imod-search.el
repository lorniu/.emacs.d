;;; imod-search.el --- Search and replace -*- lexical-binding: t -*-

;;; Code:

(x isearch
   :config
   (setq isearch-lazy-count t)
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


;;; C-c /  [input]  [n input]  [.*.el input]  [n.el input]  [w.dotnet/ headline]

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

(defvar im:rg--candicates nil)
(defvar im:rg--last-input nil)
(defvar im:rg--last-result nil)
(defvar im:rg--search-history nil)

(defun im:rg-dispatch-to-rg.el ()
  "Use rg.el to show the results in Occur buffer."
  (interactive)
  (require 'rg)
  (require 'compile)
  (require 'pcre2el)
  (im:exit-minibuffer-with-message (c (im:completion-compat :current))
    (pcase-let*
        ((`(_ ,dir ,info) im:rg--candicates)
         (rg-required-command-line-flags
          (if-let (glob (plist-get info :glob))
              (cons (concat "--glob='" glob "'") rg-required-command-line-flags)
            rg-required-command-line-flags)))
      ;; dispatch query to rg.el
      (rg (rxt-elisp-to-pcre (plist-get info :input)) "*" dir)
      ;; jump to current candicate in the *rg* buffer.
      (with-current-buffer (pop-to-buffer (rg-buffer-name))
        (setq-local compilation-finish-functions
                    (list
                     (lambda (_a _b)
                       (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                         (let ((file-name (replace-regexp-in-string "^[\\./]*" "" (cl-subseq (match-string-no-properties 1 c) (plist-get info :beg))))
                               (line-number (match-string-no-properties 2 c)))
                           (if rg-group-result
                               (progn
                                 (search-forward (format "File: %s\n" file-name) nil t)
                                 (re-search-forward (format "^ *%s" line-number) nil t))
                             (search-forward (format "%s:%s:" file-name line-number) nil t))
                           (beginning-of-line)))))))
      ;; save input to history
      (when-let (item (plist-get info :origin))
        (im:history-replace 'im:rg--search-history item)))))

(defun im:rg-parse-input (string)
  "[input]  [n input]  [.*.el input]  [n.el input]  [w.dotnet/ headline]"
  (let* ((beg 0) group input glob)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (when (looking-at (format "\\([%s]\\)\\(?: +\\|\\. *\\)" (mapconcat #'car ic/rg-groups "")))
        (setq group (match-string 1))
        (forward-char 1)
        (if (equal (char-after) ? ) (skip-chars-forward " "))
        (setq beg (- (point) 1)))
      (when (looking-at "\\.\\([^ ]+\\) *")
        (setq beg (- (match-end 0) 1))
        (setq glob (match-string 1))
        (cond ; some sugars
         ((string-match-p "^[a-z][a-z0-9]+$" glob) (setq glob (concat "*." glob))) ; .ext
         ((string-match-p "^[a-zA-Z0-9_-]+/$" glob) (setq glob (concat "**/*" (substring glob 0 -1) "*/**"))))) ; .dir/
      (when (>= (- (length (encode-coding-string string 'utf-8)) beg) 3)
        (setq input (cl-subseq string beg)))
      (list :group group :beg beg :glob glob :input input :origin string))))

(defun im:rg-candicates (info &optional dir)
  "Query via rg.el. Syntax: (im:rg-candicates (list :input require.* :glob *.el)), Return: (lines dir info)."
  (let ((default-directory (file-truename (or dir default-directory))))
    ;; Defensive
    (when (seq-empty-p (plist-get info :input))
      (error "Please input..."))
    (when (or (string= "/" default-directory)
              (string-match-p"^.:[/\\]$" default-directory)
              (string= (file-name-as-directory default-directory) (file-name-as-directory (getenv "HOME"))))
      (error "Should not search from root or home directory!"))
    ;; Debounce
    (if (and (equal (minibuffer-contents) im:rg--last-input) im:rg--last-result)
        im:rg--last-result
      (setq im:rg--last-input (minibuffer-contents) im:rg--last-result nil)
      ;; Dispatch
      (let* ((command (with-temp-buffer
                        (insert (grep-expand-template
                                 "rg --no-heading --no-config --color=never [.^.] -n -S -z -e <R> ."
                                 (plist-get info :input)))
                        (search-backward "[.^.]")
                        (delete-region (point) (match-end 0))
                        (insert (if-let (g (plist-get info :glob)) (concat "-g '" g "'") ""))
                        (buffer-string)))
             (results (with-temp-buffer
                        (process-file-shell-command command nil (current-buffer))
                        (split-string (buffer-string) "\n")))
             (lines (cl-loop for line in results
                             if (string-match "\\`\\([^:]+\\):\\([^:]+\\):" line)
                             do (progn (add-face-text-property (match-beginning 1) (match-end 1)
                                                               'compilation-info nil line)
                                       (add-face-text-property (match-beginning 2) (match-end 2)
                                                               '(:underline t :inherit compilation-line-number) nil line))
                             and collect line)))
        (setq im:rg--last-result (list lines default-directory info))))))

(defun im/rg-smart-search ()
  "Search with ripgrep smartly.

Default search current directory, if input begin with 'p ' then search current project,
if begin with 'n ' then search org-directory. See `ic/rg-groups' for other groups.

Put .glob to set the filter of files. Glob should be the syntax like .gitignore.

For example:

  n.**/*.org sometext  or  n.org sometext

will search org files in `org-directory' for 'sometext'.

Use [C-c C-o] to display the results in rg.el's Occur view, there you can do further edit.

Make sure package `rg.el' is installed."
  (interactive)
  (require 'grep)
  (require 'xref)
  (unless (executable-find "rg") (user-error "Ripgrep must be installed"))
  (let* ((completion-styles '(orderless)) ; only support orderless style
         (prompt (format "Candidates [%s]: " (string-replace " " "" (mapconcat #'car ic/rg-groups ""))))
         (word (im:thing-at-region-or-point
                (lambda ()
                  (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                    (if (and sym (> 50 (length symn) 3)) symn nil)))))
         (table (lambda (_input _pred action)
                  (cond
                   ((eq action 'metadata)
                    '(metadata (category . ripgrep)
                               (cycle-sort-function . identity)
                               (display-sort-function . identity)))
                   ((eq (car-safe action) 'boundaries) nil)
                   (t (condition-case err
                          (let* ((parsed (im:rg-parse-input (minibuffer-contents-no-properties)))
                                 (group (assoc (plist-get parsed :group) ic/rg-groups))
                                 (completion-regexp-list nil))
                            (if group (add-face-text-property (minibuffer-prompt-end) (+ 1 (minibuffer-prompt-end)) 'warning))
                            (setq im:rg--candicates
                                  (if group
                                      (let ((dir (eval (cadr group))))
                                        (if (and dir (file-directory-p dir))
                                            (im:rg-candicates parsed dir)
                                          (or (caddr group) "Directory not existed")))
                                    (im:rg-candicates parsed)))
                            (if (stringp im:rg--candicates) (error im:rg--candicates))
                            (if (null (car im:rg--candicates)) (error "Nothing found..."))
                            (complete-with-action action (car im:rg--candicates) "" nil))
                        (error (setq im:rg--candicates nil) (cdr err)))))))
         (line (catch 'miniquit
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (setq im:rg--candicates nil)
                       (use-local-map (make-composed-keymap nil (current-local-map)))
                       (local-set-key (kbd "M-w")
                                      (lambda ()
                                        (interactive)
                                        (when-let (c (ignore-errors (cadr (split-string (im:completion-compat :current) "[0-9]:"))))
                                          (kill-new c)
                                          (throw 'miniquit 'Copy))))
                       (local-set-key (kbd "C-c C-o") #'im:rg-dispatch-to-rg.el))
                   (completing-read prompt table nil nil word 'im:rg--search-history))))
         (info (cl-third im:rg--candicates)))
    (if (symbolp line) (user-error "%s done" line))
    (when-let (h (plist-get info :origin)) ; history item
      (im:history-replace 'im:rg--search-history h))
    (setq line (cl-subseq line (plist-get info :beg))) ; remove prefix 'group.glob ' in the result line
    (if (string-match (format "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'") line)
        (let ((file-name (match-string-no-properties 1 line))
              (line-number (match-string-no-properties 2 line)))
          (xref-push-marker-stack) ; use M-, to go back!
          (setq isearch-string (plist-get info :input))
          (isearch-update-ring isearch-string t)
          (find-file (expand-file-name file-name (cl-second im:rg--candicates)))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward isearch-string (point-at-eol) t)
          (recenter)
          (pulse-momentary-highlight-one-line)) ; notify line
      (message "Bad candidate?"))))

(provide 'imod-search)

;;; imod-search.el ends here
