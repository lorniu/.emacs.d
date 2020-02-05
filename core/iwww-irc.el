;;; iwww-irc.el --- IRC/ERC -*- lexical-binding: t -*-

;; From rcirc (minimal) to ERC (complex).

;;; Code:

(x erc
   :bind
   ((erc-mode-map
     ("M-m" . %erc-go-to-begin)))
   :init
   (setq erc-server "irc.libera.chat")
   ;; (erc-prompt "ERC>")
   ;; (erc-fill-prefix nil)
   (setq erc-nick "tzlm454")
   (setq erc-fill-column 120)
   (setq erc-fill-static-center 14)
   (setq erc-fill-function 'erc-fill-static--skip-system-message)
   (setq erc-insert-timestamp-function 'erc-insert-timestamp-above)

   (setq erc-server-reconnect-attempts 5)
   (setq erc-server-reconnect-timeout 3)

   (setq erc-keywords '("money" "bill"  "accelerate" "Accelerate"))
   (setq erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477"))

   (setq erc-log-insert-log-on-open nil)
   (setq erc-log-channels-directory "~/.erc-logs/")
   (setq erc-log-enable 'erc-log-all-but-server-buffers)
   (setq erc-log-filter-function 'erc-my-filter)

   (setq erc-prompt-for-password nil)

   (defun %erc-go-to-begin ()
     (interactive)
     (beginning-of-line)
     (cond ((looking-at "\\s-*<")
            (re-search-forward "> +" (line-end-position) t 1))
           (t (call-interactively #'back-to-indentation))))

   (defun:hook erc-mode-hook/setup ()
     (setq erc-hide-timestamps t)
     (erc-hl-nicks-mode 1))

   (defvar erc-hide-list-default '("JOIN" "PART" "QUIT"))
   (setq erc-hide-list erc-hide-list-default)

   (defun erc-my-filter (topic)
     "Filter out system messges from the logs"
     (if (equal (string-match-p "^\\[[:digit:]: ]+\\*\\*\\*[[:space:]]" topic) nil)
         topic
       ""))

   (defun erc-toggle-hide-list ()
     (interactive)
     (setq erc-hide-list (if erc-hide-list nil erc-hide-list-default)))

   (defun erc-insert-timestamp-above (_string)
     "Insert timestamps above the line."
     (goto-char (point-min))
     (let* ((now (current-time))
            (utc (format-time-string "%H:%M" now t))
            (est (format-time-string "%H:%M" now "EST"))
            (utc8 (format-time-string "%H:%M" now "UTC-8"))
            (ignore-p (and
                       erc-timestamp-last-inserted
                       (< (float-time (time-subtract now erc-timestamp-last-inserted))
                          60))) ;; interval
	        (time-show (format "[%s/%s/%s]\n" utc est utc8))
            (len (length time-show)))
       (unless ignore-p
         (setq erc-timestamp-last-inserted now)
         (erc-put-text-property 0 len 'face 'erc-timestamp-face time-show)
         (erc-put-text-property 0 len 'field 'erc-timestamp time-show)
         (erc-put-text-property 0 len 'invisible 'timestamp time-show)
         (insert time-show))))

   (defun erc-fill-static--skip-system-message ()
     "Fills a text such that messages start at column `erc-fill-static-center'."
     (save-match-data
       (goto-char (point-min))
       (looking-at "^\\(\\S-+\\)")
       (let ((nick (match-string 1)))
         (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
               (fill-prefix (make-string erc-fill-static-center 32)))
           (when (and (looking-at "^<")
                      (> erc-fill-static-center (length nick)))
             (let* ((right (/ (- erc-fill-static-center (length nick)) 2))
                    (left (- erc-fill-static-center (length nick) right)))
               (insert (make-string left 32))
               (goto-char (+ (point) (length nick)))
               (insert (make-string (max 0 (1- right)) 32))))
           (erc-fill-regarding-timestamp))
         (erc-restore-text-properties))))

   :defer-config
   (erc-log-mode)
   (with-eval-after-load 'which-func
     (add-to-list 'which-func-non-auto-modes 'erc-mode)))



(transient-define-prefix imtt/transient-erc-mode ()
  [[("-t" (lambda ()
            (if erc-hide-timestamps
                (propertize "Timestamp" 'face 'font-lock-doc-face)
              "Timestamp"))
     erc-toggle-timestamps)
    ]
   [("-o" (lambda ()
            (if erc-hide-list
                (propertize "Sys Msg" 'face 'font-lock-doc-face)
              "Sys Msg"))
     erc-toggle-hide-list)
    ]
   [("o" "Occur.." erc-occur)
    ]
   [("s" "Save Buffer in Logs" erc-save-buffer-in-logs)
    ]
   [("m" "Join.." erc-join-my-groups)
    ]
   ]
  (interactive)
  (if (eq major-mode 'erc-mode)
      (transient-setup 'imtt/transient-erc-mode)
    (user-error "You should invoke this in erc-mode.")))



(defun erc-cmd-HOWMANY (&rest _ignore)
  "Display how many users (and ops) the current channel has."
  (erc-display-message
   nil 'notice (current-buffer)
   (let ((hash-table (with-current-buffer
                         (erc-server-buffer)
                       erc-server-users))
         (users 0)
         (ops 0))
     (maphash (lambda (k v)
                (when (member (current-buffer)
                              (erc-server-user-buffers v))
                  (cl-incf users))
                (when (erc-channel-user-op-p k)
                  (cl-incf ops)))
              hash-table)
     (format
      "There are %s users (%s ops) on the current channel"
      users ops))))

(defun erc-cmd-JJ (&optional amount)
  (erc-join-my-groups amount))

(defun erc-join-my-groups (&optional arg)
  (interactive)
  (when (not (equal major-mode 'erc-mode))
    (user-error "Should called from erc-mode."))
  (let* ((vertico-sort-function)
         (candidates '("#emacs" "#commonlisp" "#c_lang_cn" "#linuxba"
                       "#archlinux-cn" "#archlinux"
                       "#c" "#java" "#clojure"))
         (groups (cond ((null arg)
                        (completing-read-multiple "Groups: " candidates))
                       ((or (numberp arg)
                            (string-match-p "^[0-9]+$" arg))
                        (cl-subseq candidates 0
                                   (if (numberp arg) arg (string-to-number arg))))
                       (t (list arg)))))
    (if (null groups) (user-error "No group selected."))
    (cl-loop for group in groups do (erc-cmd-JOIN group))))

(provide 'iwww-irc)

;;; iwww-irc.el ends here
