;;; ikmd-keys-and-commands.el --- Keys and Commands -*- lexical-binding: t -*-

;;; Code:

(global-unset-key (kbd "C-x i"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-k"))

(leaf-keys
 (( "C-x i"         .  imtt/transient-goto                         )
  ( "C-c i"         .  imtt/transient-act                          )
  ( "C-c p"         .  imtt/transient-tpl                          )
  ( "C-c e"         .  imtt/transient-custom                       )
  ( "C-x C-o"       .  imtt/transient-window                       )

  ( "C-c a"         .  org-agenda                                  )
  ( "C-c c"         .  imtt/transient-gtd                          )
  ( "C-c g"         .  magit-status                                )
  ( "C-c m"         .  im/welcome-to-mode                          )
  ( "C-c '"         .  edit-indirect-region                        )

  ( [f2]            .  name-last-kbd-macro                         )
  ( [(control f2)]  .  name-and-insert-last-kbd-macro              )
  ( [f5]            .  gts-do-translate                            )
  ( [f6]            .  toggle-truncate-lines                       )
  ( [f7]            .  quick-calc                                  )
  ( [f8]            .  calendar                                    )
  ( [f9]            .  compile                                     )
  ( [f10]           .  im/silly                                    )
  ( [f12]           .  im/popup-eshell                             )
  ( [M-f12]         .  im/popup-xshell                             )

  ( "%"             .  his-match-paren                             )
  ( "C-#"           .  cua-rectangle-mark-mode                     )
  ( "C-s"           .  im/isearch-regexp                           )
  ( "M-w"           .  im/yank-more                                )
  ( "C-o"           .  im/smart-open-line                          )
  ( "M-q"           .  im/tiny-code                                )
  ( "M-Q"           .  im/tiny-code-buffer                         )
  ( "<C-backspace>" .  im/backward-delete-word                     )

  ( "ESC <down>"    .  im/copy-lines                               )
  ( "ESC <right>"   .  im/kill-lines                               )
  ( "<insertchar>"  .  undo                                        )
  ( "C-{"           .  shrink-window                               )
  ( "C-}"           .  enlarge-window                              )

  ( "C-x f"         .  project-find-file                           )
  ( "C-x F"         .  consult-file-externally                     )
  ( "C-x d"         .  im/project-find-directory                   )
  ( "C-x C-b"       .  ibuffer                                     )
  ( "C-x b"         .  consult-buffer                              )
  ( "C-x 4 b"       .  consult-buffer-other-window                 )
  ( "C-x 5 b"       .  consult-buffer-other-frame                  )
  ( "C-x C-j"       .  ffap                                        )
  ( "C-x C-k"       .  kill-this-buffer                            )

  ( "C-c `"         .  im/toggle-diagnostics-buffer                )
  ( "C-c /"         .  im/search-ripgrep+                          )
  ( "C-c RET"       .  im/dispatch-macroexpand                     )

  ( "C-x C-r"       .  im/view-favorites                           )

  ( "M-g g"         .  consult-goto-line                           )
  ( "M-g M-g"       .  consult-goto-line                           )

  ( "M-s m"         .  consult-multi-occur                         )

  ( "M-."           .  xref-find-definitions+citre                 )

  ( "C-."           .  imtt/transient-interact                     )
  ( "C-,"           .  er/expand-region                            )
  ( "C-M-,"         .  er/contract-region                          )
  ( "C-x n"         .  imtt/transient-narrow                       )

  ( "C-h h"         .  imtt/transient-help                         )
  ( "C-h C-h"       .  imtt/transient-help                         )
  ( "C-h c"         .  quick-calc                                  )
  ( "C-h C-c"       .  calc                                        )
  ( "C-h t"         .  im/popup-system-terminal                    )
  ( "C-h C-t"       .  im/popup-system-terminal                    )
  ( "C-h g"         .  im/toggle-gnus                              )
  ( "C-h C-g"       .  im/toggle-gnus                              )
  ( "C-h e"         .  im/toggle-eshell-buffer                     )
  ( "C-h C-e"       .  im/toggle-eshell-buffer                     )
  ( "C-h s"         .  im/toggle-scratch-or-ielm                   )
  ( "C-h C-s"       .  im/toggle-scratch-or-ielm                   )
  ( "C-h w"         .  im/toggle-messages-buffer                   )
  ( "C-h C-w"       .  im/toggle-messages-buffer                   )
  ( "C-h l"         .  im/toggle-common-lisp-dev-buffer            )
  ( "C-h C-l"       .  im/toggle-common-lisp-dev-buffer            )))

(global-set-key [mouse-2] 'mouse-drag-region)
(global-set-key [C-down-mouse-1] 'mouse-drag-region)



(x keycast
   :ref "tarsius/keycast")

(x which-key/d
   :init
   (setq which-key-show-early-on-C-h t
         which-key-idle-delay 1.5
         which-key-idle-secondary-delay 0.05)
   (which-key-mode 1)

   :defer-config
   (setq which-key-compute-remaps t)
   (setq which-key-allow-multiple-replacements t)
   (setq which-key-sort-order #'my/which-key-local-then-prefix-then-key-order)

   (which-key-add-key-based-replacements
     "C-x 8"   "unicode"
     "C-x a"   "custom-act"
     "C-x i"   "custom-goto"
     "C-x p"   "project"
     "C-x r"   "rect/register/bookmark"
     "C-x v"   "version-control")

   (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
   (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("߅" . nil)))
   (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⚔" . nil)))
   (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . (" " . nil)))
   (add-to-list 'which-key-replacement-alist '(("ESC" . nil) . ("ℇ" . nil)))

   (defun my/which-key-local-then-prefix-then-key-order (acons bcons)
     (let ((aloc? (which-key--local-binding-p acons))
           (bloc? (which-key--local-binding-p bcons)))
       (if (not (eq aloc? bloc?))
           (and aloc? (not bloc?))
         (let ((apref? (which-key--group-p (cdr acons)))
               (bpref? (which-key--group-p (cdr bcons))))
           (if (not (eq apref? bpref?))
               (and (not apref?) bpref?)
             (which-key-key-order-alpha acons bcons)))))))


;;; Enhanced

(defun im/yank-more ()
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

(defun im/bind-key-to-command ()
  (interactive)
  (let ((fun (read-string "Command: " (symbol-name (symbol-at-point)))))
    (if (< (length fun) 4)
        (user-error "Maybe the command is not right")
      (let ((key (read-key-sequence (format "Key: " fun))))
        (if (< (length key) 2)
            (user-error "Maybe the key is not right")
          (when (y-or-n-p (format "Bind '%s' to command '%s' ?" (key-description key) fun))
            (global-set-key (kbd key) (intern fun))))))))

(defun im/backward-word ()
  "Don't back too much."
  (if (string-match-p "^\\W*$" (buffer-substring-no-properties (line-beginning-position) (point)))
      (forward-same-syntax -1)
    (call-interactively 'backward-word)))

(defun im/backward-delete-word ()
  "Don't put into kill ring, don't delete too much."
  (interactive "*")
  (push-mark)
  (im/backward-word)
  (delete-region (point) (mark)))

(defun im/isearch-regexp ()
  "Isearch+, default with region word, enable regexp."
  (interactive)
  (if (use-region-p)
      (progn
        (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (isearch-resume string nil nil t string nil)))
    (call-interactively 'isearch-forward-regexp)))


;;; Toggle Display (C-h-*)

(defmacro im/hide-or-show-buffer (buffer-predicate buffer-show-form &rest buffer-init-form)
  "Helper macro: hide or show buffer window."
  (declare (indent 1))
  `(let* ((it (cl-find-if ,(cond ((and (consp buffer-predicate) (equal (car buffer-predicate) 'lambda)) buffer-predicate)
                                 ((symbolp buffer-predicate) buffer-predicate)
                                 (t `(lambda (b) (string-equal (buffer-name b) ,buffer-predicate))))
                          (buffer-list))))
     (cond ((and ,(not (null buffer-init-form)) (null it))
            ,@buffer-init-form)
           ((and it (get-buffer-window it))
            (save-excursion (ignore-errors (delete-window (get-buffer-window it)))))
           (t ,buffer-show-form))))

(defun im/toggle-messages-buffer()
  "Toggle show the *Messages* buffer."
  (interactive)
  (im/hide-or-show-buffer "*Messages*" (call-interactively 'view-echo-area-messages)))

(defun im/toggle-scratch-or-ielm (&optional ielmp)
  "Toggle show the *Scratch* buffer or ielm."
  (interactive "P")
  (im/hide-or-show-buffer (if ielmp "*ielm*" "*scratch*")
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (if ielmp (ielm) (resume-scratch)))))

(defun im/toggle-gnus ()
  "Toggle show the *Group* buffer."
  (interactive)
  (im/hide-or-show-buffer "*Group*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window display-buffer-in-direction) (direction . right)))))
      (display-buffer it))
    (user-error "You should start GNUS first.")))

(defun im/toggle-common-lisp-dev-buffer ()
  "Toggle show SLIME/SLY buffer."
  (interactive)
  (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-below-selected) (direction . right)))))
    (im/hide-or-show-buffer (lambda (b) (string-match-p "^\\*sl.*-m?repl.*" (buffer-name b)))
      (pop-to-buffer it)
      (condition-case nil
          (call-interactively ic/lisp-dev)
        (error (%lisp/devenv-nofound))))))

(defun im/toggle-eshell-buffer ()
  "Toggle show the *EShell* buffer."
  (interactive)
  (im/hide-or-show-buffer "*eshell*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (call-interactively 'eshell))))

(defun im/toggle-diagnostics-buffer()
  "Show the error messages for flycheck or flymake."
  (interactive)
  (cond ((and (boundp 'flycheck-mode) flycheck-mode)
         (let ((buf flycheck-error-list-buffer))
           (call-interactively 'flycheck-list-errors)
           (select-window (get-buffer-window buf))))
        ((and (boundp 'flymake-mode) flymake-mode)
         (let ((buf (flymake--diagnostics-buffer-name)))
           (call-interactively 'flymake-show-diagnostics-buffer)
           (select-window (get-buffer-window buf))))
        (t (message "Nothing to do, check flycheck or flymake toggled?"))))


;;; Open With

(defun im/open-externally (&optional @fname)
  "Open file in OS way."
  (interactive)
  (let (($file-list
         (if @fname (list @fname)
           (cond ((string-equal major-mode "dired-mode")
                  (dired-get-marked-files))
                 ((string-equal major-mode "eshell-mode")
                  (list (eshell/pwd)))
                 ((string-equal major-mode "shell-mode")
                  (list default-directory))
                 (t (list (or (buffer-file-name)
                              (if (y-or-n-p "No location, goto default dir or QUIT?" )
                                  default-directory
                                (message "Do Nothing.")))))))))
    (when (or (<= (length $file-list) 5) (y-or-n-p "Open more than 5 files? "))
      (cond
       (IS-WIN
        (mapc (lambda ($fpath) (w32-shell-execute "open" $fpath)) $file-list))
       (IS-MAC
        (mapc (lambda ($fpath) (shell-command (concat "open " (shell-quote-argument $fpath))))  $file-list))
       (IS-G
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(defun im/open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))


;;; Commands

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
      (user-error "URL not found.")
    (let ((eshell-buffer-name "*youtube-dl*")
          (directory (seq-find (lambda (dir)
                                 (and (file-directory-p dir) (expand-file-name dir)))
                               '("~/temp") ".")))
      (eshell)
      (when (eshell-interactive-process) (eshell t))
      (eshell-interrupt-process)
      (insert (format " cd '%s' && youtube-dl " directory) url)
      (eshell-send-input))))

(defun grep-cursor (word)
  "Grep the current WORD in the files."
  (interactive (list (im-thing-at-region-or-point)))
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
  (cond ((and (eq major-mode 'org-mode) ; org-src-block
              (eq (org-element-type (org-element-context)) 'src-block))
         (org-edit-special)
         (save-mark-and-excursion
           (indent-region (point-min) (point-max)))
         (org-edit-src-exit))
        (t (let (beg end whole)
             (cond ((use-region-p) ; region
                    (setq beg (region-beginning)
                          end (region-end)))
                   (current-prefix-arg ; whole buffer
                    (setq beg (point-min)
                          end (point-max)
                          whole t))
                   ((string-match-p ")\\|}" (char-to-string (preceding-char))) ; sexp end
                    (save-excursion
                      (setq end (point))
                      (backward-sexp 1)
                      (setq beg (point)))))
             (cond ((eq major-mode 'lsp-mode) ; lsp-mode
                    (cond (whole (lsp-format-buffer))
                          ((and beg end) (lsp-format-region beg end))
                          (t (call-interactively #'lsp-format-region))))
                   (t (if (and beg end) ; default
                          (indent-region beg end)
                        (indent-according-to-mode))))))))

(defun im/tiny-code-buffer ()
  "Indent codes according mode for whole buffer."
  (interactive)
  (save-excursion
    (call-interactively 'mark-whole-buffer)
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
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        ((or (derived-mode-p 'web-mode)
             (derived-mode-p 'sgml-mode))
         (require 'sgml-mode)
         (cond ((looking-at "<")
                (sgml-skip-tag-forward 1))
               ((looking-back ">")
                (sgml-skip-tag-backward 1))
               (t (self-insert-command (or n 1)))))
        (t
         (self-insert-command (or n 1)))))

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
    (im-make-buffer-buriable)
    (local-set-key "Q" 'kill-this-buffer)
    (read-only-mode 1)))

(defun resume-scratch ()
  "This sends you to the *Scratch* buffer."
  (interactive)
  (let ((name "*scratch*"))
    (unless (get-buffer name)
      (progn
        (get-buffer-create name)
        (with-current-buffer name
          (insert ";; scratch buffer\n\n")
          (funcall initial-major-mode))))
    (pop-to-buffer name '(display-buffer-pop-up-window) t)))

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

(defvar my-wrap-current nil)

(defun im/wrap-current (chars)
  "Wrap current word with some CHARS, x_y format."
  (interactive (list
                (read-string (format "Wrap %s with%s: "
                                     (if (use-region-p) "region"
                                       (propertize (or (word-at-point) (user-error "No region or no word at point."))
                                                   'face 'font-lock-warning-face))
                                     (if my-wrap-current (format " (%s)" (car my-wrap-current)) ""))
                             nil 'my-wrap-current (car my-wrap-current))))
  (save-excursion
    (let* ((fill (split-string chars " +"))
           (left (car fill))
           (right (or (cadr fill)
                      (pcase left
                        ("(" ")")
                        ("[" "]")
                        ("{" "}")
                        ("<" ">")
                        (_ left))))
           beg end)
      (if (use-region-p)
          (setq beg (region-beginning) end (region-end))
        (if (looking-at-p "\\sw") (forward-word))
        (setq end (point))
        (backward-word) (setq beg (point)))
      (goto-char end) (insert right)
      (goto-char beg) (insert left)
      (deactivate-mark))))

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

(defun im/insert-date (&optional timep)
  (interactive "P")
  (insert (format-time-string (if timep "%F %T" "%F"))))

(defun im/yank-current-directory ()
  (interactive)
  (if-let ((d default-directory))
      (progn
        (kill-new d)
        (message "Yanked: %s" d))
    (user-error "Nothing to copy.")))

(defun im/yank-current-buffer-name ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffname (buffer-name)))
    (if filename (kill-new filename))
    (kill-new buffname)
    (message "Yanked: %s" buffname)))

(defun im/set-lisp-indent-function-buffer-local ()
  "Toggle the current indent-function."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (user-error "Should be invoked in emacs-lisp-mode."))
  (cl-loop for item in '("common-lisp-indent-function" "lisp-indent-function")
           unless (string= item (symbol-name lisp-indent-function))
           collect item into cs
           finally do
           (let ((exp (completing-read "Change to: " cs nil t)))
             (setq-local lisp-indent-function (intern exp))
             (message "Changed to `%s' locally." lisp-indent-function))))

(defun trailing-whitespace-mode ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show-Trailing-Whitespace: %S" show-trailing-whitespace))

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

(defun im/ipinfo (ip)
  "Return ip info from ipinfo.io for IP."
  (interactive "sEnter IP to query (blank for own IP): ")
  (require 'request)
  (request
   (concat "https://ipinfo.io/" ip)
   :headers '(("User-Agent" . "Emacs ipinfo.io Client")
              ("Accept" . "application/json")
              ("Content-Type" . "application/json;charset=utf-8"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message
                (mapconcat
                 (lambda (e)
                   (format "%10s: %s" (capitalize (symbol-name (car e))) (cdr e)))
                 data "\n"))))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Can't receive ipinfo. Error %S " error-thrown)))))

(defun im/change-window-split-layout ()
  "Switch between vertical and horizontal split. It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun im/shengciben-add (word)
  (interactive (list (read-string "Save to shengciben: " (im-thing-at-region-or-point))))
  (deactivate-mark)
  (when (zerop (length word))
    (user-error "Word is empty, nothing done."))
  (save-window-excursion
    (let* ((f-default ic/shengciben)
           (f (if (file-exists-p f-default) f-default (locc "words.org"))))
      (find-file f)
      (goto-char (point-max))
      (skip-chars-backward " \n")
      (insert (format "\n- %-35s (%s)" word (time-str)))
      (save-buffer)))
  (message "[%s] saved." (propertize word 'face 'font-lock-string-face)))

(defun im/set-minibuffer-height (n)
  (interactive "nMinibuffer height set to: ")
  (set-face-attribute 'mode-line nil :height n)
  (set-face-attribute 'mode-line-inactive nil :height n)
  (message "Set current height to %d" n))

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

(provide 'ikmd-keys-and-commands)

;;; ikmd-keys-and-commands.el ends here
