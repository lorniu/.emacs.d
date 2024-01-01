;;; -*- lexical-binding: t -*-

;;; Code:

(require 'grep)

(xzz isearch
  :config
  (setopt isearch-lazy-count t
          isearch-allow-motion t)
  (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
  (define-key isearch-mode-map "\C-e" 'isearch-edit-string))

(xzz replace
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
                    (if-let* ((it (get-buffer-window "*rg*"))) (select-window it))))
  (defun:after occur//follow (&rest _) (if-let* ((it (get-buffer-window "*Occur*"))) (select-window it))))

(defun im/grep-cursor (word)
  "Grep the current WORD in the files."
  (interactive (list (im:thing-at-region-or-point 'word)))
  (if (or (not word) (< (length word) 3))
      (message "word not available")
    (let* ((oldcmd grep-find-command)
           (ext (if-let* ((it (file-name-extension (buffer-name)))) (concat "." it) ""))
           (newcmd (format "find . -maxdepth 1 -type f -name '*%s' -exec grep -nH -e '%s' {} + " ext word)))
      (unwind-protect
          (progn
            (grep-apply-setting 'grep-find-command newcmd)
            (call-interactively 'grep-find))
        (grep-apply-setting 'grep-find-command oldcmd)))))

(defun im/isearch-smart ()
  "Isearch+, default with region word, then literal or regexp according last search."
  (interactive)
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-resume string nil nil t string nil))
    (call-interactively (if isearch-regexp 'isearch-forward-regexp 'isearch-forward))))


;;; C-c /  [input]  [n input]  [.*.el input]  [n.el input]  [w.dotnet/ headline]

(defcustom im.rg-groups
  `(("e"   (loce ""))
    ("a"   agenda-directory)
    ("n"   org-directory)
    ("s"   im.srcdir "im.srcdir not config")
    ("w"   im.workdir "im.workdir not config")
    ("p"   (project-root (project-current)) "No project found"))
  "Groups used by `im/rg-smart-search'. Format: (group-char search-dir prompt-when-non-exist*)+"
  :group 'imfine
  :type 'list)

(defvar im:rg-info nil)
(defvar im:rg-history nil)

(defun im:rg-build-command (glob input &optional opts)
  (let ((tpl (concat "rg --no-heading --no-config "
                     (concat (or opts "-S") " ")
                     (if glob (concat "-g '" glob "' "))
                     "-n -z -e <R> .")))
    (grep-expand-template tpl (rxt-elisp-to-pcre (or input "")))))

(defun im:rg-parse-input (string)
  "[input]  [n input]  [.*.el input]  [n.el input]  [w.dotnet/ headline]"
  (let* ((beg 0) group input glob dir opts command)
    (when-let* ((pos (cl-search " --- " string)))
      (setq opts (cl-subseq string (+ 5 pos)))
      (setq string (cl-subseq string 0 pos)))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (when (looking-at (format "\\([%s]\\)\\(?: +\\|\\. *\\)" (mapconcat #'car im.rg-groups "")))
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
      (if-let* ((dir-info (and group (cadr (assoc group im.rg-groups)))))
          (setq dir (eval dir-info))
        (setq dir default-directory)))
    (unless (and dir (file-exists-p dir))
      (error "Directory not exists"))
    (when (or (string= "/" dir)
              (string-match-p"^.:[/\\]$" dir)
              (string= (file-name-as-directory dir) (file-name-as-directory (getenv "HOME"))))
      (error "Should not search from root or home directory!"))
    (setq command (im:rg-build-command glob input opts))
    (list :origin string :input input :group group :beg beg :glob glob :dir dir :opts opts :command command)))

;; (im:rg-parse-input "n.org group")
;; (plist-get im:rg-info :dir)

(defun im:rg-candicates (info)
  "Query via ripgrep.
Syntax: (im:rg-candicates (list :input require.* :glob *.el :dir ...))
Return: (list :command command :items items)"
  (when (length= (plist-get info :input) 0)
    (error "Please input..."))
  (let* ((default-directory (plist-get info :dir))
         (lines (with-temp-buffer
                  (process-file-shell-command (plist-get info :command) nil (current-buffer))
                  (split-string (buffer-string) "\n"))))
    (cl-loop for line in lines
             if (string-match "\\`\\([^:]+\\):\\([^:]+\\):" line)
             do (progn (add-face-text-property (match-beginning 1) (match-end 1)
                                               'compilation-info nil line)
                       (add-face-text-property (match-beginning 2) (match-end 2)
                                               '(:underline t :inherit compilation-line-number) nil line))
             and collect line)))

(defun im:rg-dispatch-to-rg.el ()
  "Use rg.el to show the results in Occur buffer."
  (interactive)
  (im:exit-minibuffer-with-message (c (im:completion-compat :current))
    (let ((default-directory (plist-get im:rg-info :dir)))
      ;; search and display in dedicated buffer
      (grep (plist-get im:rg-info :command))
      ;; jump to current candicate
      (with-current-buffer (pop-to-buffer "*grep*")
        (setq-local compilation-finish-functions
                    (list
                     (lambda (_a _b)
                       (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                         (let ((file-name (replace-regexp-in-string
                                           "^[\\./]*" ""
                                           (cl-subseq (match-string-no-properties 1 c) (plist-get im:rg-info :beg))))
                               (line-number (match-string-no-properties 2 c)))
                           (search-forward (format "%s:%s:" file-name line-number) nil t)
                           (beginning-of-line)
                           (when (re-search-forward (plist-get im:rg-info :input) (line-end-position) t)
                             (goto-char (match-beginning 0)))))))))
      ;; save input to history
      (when-let* ((item (plist-get im:rg-info :origin)))
        (im:history-replace 'im:rg-history item)))))

(defun im/rg-smart-search ()
  "Search with ripgrep smartly.

Default search current directory, if input begin with 'p ' then search current project,
if begin with 'n ' then search org-directory. See `im.rg-groups' for other groups.

Put .glob to set the filter of files. Glob should be the syntax like .gitignore.

For example:

  n.**/*.org sometext  or  n.org sometext

will search org files in `org-directory' for \\='sometext'.

Use [C-c C-o] to display the results in rg.el's Occur view, there you can do further edit.

Make sure package `rg.el' is installed."
  (interactive)
  (unless (executable-find "rg")
    (user-error "Ripgrep must be installed"))
  (let* ((completion-styles '(orderless)) ; only support orderless style
         (prompt (format "Candidates [%s]: " (string-replace " " "" (mapconcat #'car im.rg-groups ""))))
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
                          (let* ((text (minibuffer-contents-no-properties)))
                            (if (and (equal text (plist-get im:rg-info :origin)) (plist-get im:rg-info :items))
                                (complete-with-action action (plist-get im:rg-info :items) "" nil)
                              (let* ((completion-regexp-list nil)
                                     (info (im:rg-parse-input text))
                                     (items (progn
                                              (when (plist-get info :group)
                                                (add-face-text-property (minibuffer-prompt-end) (+ 1 (minibuffer-prompt-end)) 'warning))
                                              (im:rg-candicates info))))
                                (setq im:rg-info (plist-put info :items items))
                                (unless (plist-get info :items) (error "Nothing found..."))
                                (complete-with-action action (plist-get im:rg-info :items) "" nil))))
                        (error (setq im:rg-info nil) (cdr err)))))))
         (line (catch 'miniquit
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (let ((text (minibuffer-contents-no-properties)))
                         (unless (equal text (plist-get im:rg-info :origin))
                           (setq im:rg-info (im:rg-parse-input text))))
                       (use-local-map (make-composed-keymap nil (current-local-map)))
                       (local-set-key (kbd "M-w")
                                      (lambda ()
                                        (interactive)
                                        (when-let* ((c (ignore-errors (cadr (split-string (im:completion-compat :current) "[0-9]:")))))
                                          (kill-new c)
                                          (throw 'miniquit 'Copy))))
                       (local-set-key (kbd "C-c o") #'im:rg-dispatch-to-rg.el)
                       (local-set-key (kbd "C-c C-o") #'im:rg-dispatch-to-rg.el))
                   (completing-read prompt table nil nil word 'im:rg-history)))))
    (if (symbolp line) (user-error "%s done" line))
    (when-let* ((h (plist-get im:rg-info :origin))) ; history item
      (im:history-replace 'im:rg-history h))
    (if (string-match (format "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'") line)
        (let ((file-name (match-string-no-properties 1 line))
              (line-number (match-string-no-properties 2 line)))
          (require 'xref)
          (xref-push-marker-stack) ; use M-, to go back!
          (setq isearch-string (plist-get im:rg-info :input))
          (isearch-update-ring isearch-string t)
          (find-file (expand-file-name file-name (plist-get im:rg-info :dir)))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward isearch-string (line-end-position) t)
          (recenter)
          (pulse-momentary-highlight-one-line)) ; notify line
      (message "Bad candidate?"))))
