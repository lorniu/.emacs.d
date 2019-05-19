;;; imkeyc.el --- Keys and Commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'bind-key)



;;; Keybinds

(bind-keys
 ( [f2]            . name-last-kbd-macro  )
 ( [(control f2)]  . name-and-insert-last-kbd-macro )
 ( [f6]            . toggle-truncate-lines)
 ( [f8]            . calendar             )
 ( [f10]           . im/silly             )
 ( [f11]           . ivy-push-view        )
 ( [C-f11]         . ivy-switch-view      )
 ( [f12]           . im/popup-eshell      )
 ( [C-f12]         . im/popup-shell       )
 ( [M-f12]         . im/popup-xshell      )

 ( "%"             . his-match-paren      )
 ( "C-#"           . cua-rectangle-mark-mode )
 ( "C-s"           . im/isearch-regexp    )
 ( "C-o"           . im/open-line         )
 ( "C-c C-j"       . ffap                 )
 ( "M-w"           . im/yank-more         )
 ( "M-q"           . im/tiny-code         )
 ( "M-Q"           . im/tiny-code-buffer  )
 ( "ESC <down>"    . im/copy-lines        )
 ( "ESC <up>"      . (lambda () (interactive) (im/copy-lines 1)))
 ( "ESC <right>"   . im/kill-lines        )
 ( "<C-backspace>" . im/backward-kill-word )
 ( "<insertchar>"  . undo                 )
 ( "<select>"      . im/toggle-dedicated  )
 ( "C-{"           . shrink-window        )
 ( "C-}"           . enlarge-window       )

 ( "M-x"           . counsel-M-x          )
 ( "C-r"           . counsel-grep-or-swiper )
 ( "C-x b"         . ivy-switch-buffer    )
 ( "C-x C-b"       . ibuffer              )
 ( "C-x d"         . counsel-projectile-find-dir )
 ( "C-x C-d"       . dired                )
 ( "C-x f"         . counsel-projectile-find-file )
 ( "C-x C-f"       . counsel-find-file    )
 ( "C-x i"         . counsel-imenu        )
 ( "C-x p"         . ivy-pages            )
 ( "C-h b"         . counsel-descbinds    )
 ( "C-h v"         . counsel-describe-variable )
 ( "C-h f"         . counsel-describe-function )
 ( "C-h S"         . counsel-info-lookup-symbol)
 ( "C-x C-r"       . im/file-viewer       ))

(global-unset-key (kbd "C-x C-z"))



;;; Key-Chord

(x key-chord
   :init
   (setq key-chord-one-key-delay 0.3 key-chord-two-keys-delay 0.2)
   (key-chord-mode 1)
   (defun key-chord-define (keymap keys command)
     "Override, make keys sequencely."
     (if (/= 2 (length keys)) (error "Key-chord keys must have two elements"))
     (let ((key1 (logand 255 (aref keys 0)))
	       (key2 (logand 255 (aref keys 1))))
       (if (eq key1 key2)
	       (define-key keymap (vector 'key-chord key1 key2) command)
         (define-key keymap (vector 'key-chord key1 key2) command)))))

(key-chord-define-global ",h" 'im/host-viewer)
(key-chord-define-global ",f" 'im/go-to-char)
(key-chord-define-global ",t" (lambda () (interactive) (im/go-to-char t)))



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
   ((looking-back "^[ \t]*" nil)
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



;;; Popups

(defun im/popup-eshell (&optional arg)
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (bury-buffer)
    (let ((last-file (or dired-directory (buffer-file-name))))
      (eshell arg)
      (if last-file (eshell-add-to-dir-ring (file-name-directory last-file))))))

(defun im/popup-shell (&optional _)
  (interactive "P")
  (if (eq major-mode 'shell-mode)
      (bury-buffer)
    (let ((curr-dir default-directory))
      (shell "*+shell+*")
      (ring-insert comint-input-ring
                   (concat "cd " (shell-quote-argument curr-dir))))))

(defun im/popup-xshell ()
  (interactive)
  (ivy-read "Shell: " '(powershell system-default)
            :action (lambda (type)
                      (if (string= type "powershell")
                          (powershell)
                        (let ((explicit-shell-file-name (getenv "SHELL")))
                          (shell "*-shell-*"))))))



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
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(]" next-char) (forward-sexp 1))
          ((string-match "[\]})]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or n 1))))))

(defun ascii-table-show ()
  "Print ASCII table."
  (interactive)
  (with-current-buffer
      (switch-to-buffer "*ASCII table*")
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

(defun im/wrap-current (chars)
  "Wrap current word with some CHARS, xx__yy format."
  (interactive (list (read-string "Wrap with: ")))
  (save-excursion
    (let* ((fill (split-string chars "  +"))
           (left (car fill)) (right (or (cadr fill) left))
           beg end)
      (if (use-region-p)
          (setq beg (region-beginning) end (region-end))
        (forward-word) (setq end (point))
        (backward-word) (setq beg (point)))
      (goto-char end) (insert right)
      (goto-char beg) (insert left)
      (deactivate-mark))))

(defun im/youtube-dl-url (&optional url)
  "Run 'youtube-dl' over the URL. If URL is nil, use URL at point."
  (interactive)
  (setq url (or url (thing-at-point-url-at-point)))
  (let ((eshell-buffer-name "*youtube-dl*")
        (directory (seq-find (lambda (dir)
                               (and (file-directory-p dir) (expand-file-name dir)))
                             '("~/temp") ".")))
    (eshell)
    (when (eshell-interactive-process) (eshell t))
    (eshell-interrupt-process)
    (insert (format " cd '%s' && youtube-dl " directory) url)
    (eshell-send-input)))

(defun trailing-whitespace-mode ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show-Trailing-Whitespace: %S" show-trailing-whitespace))

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


(provide 'imkeyc)

;;; imkeyc.el ends here
