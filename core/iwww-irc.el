;;; -*- lexical-binding: t -*-

;; From rcirc (minimal) to ERC (complex).

;;; Code:

(defvar erc-hide-list-default '("JOIN" "PART" "QUIT"))

(xzz erc
  :bind (:map erc-mode-map ("M-m" . im:erc-go-to-begin))
  :config
  (setopt erc-server "irc.libera.chat"
          erc-nick "tzlm454"
          erc-fill-column 120
          erc-fill-static-center 14
          erc-fill-function 'im:erc-fill-static--skip-system-message
          erc-insert-timestamp-function 'im:erc-insert-timestamp-above

          erc-server-reconnect-attempts 5
          erc-server-reconnect-timeout 3

          erc-keywords '("money" "bill"  "accelerate" "Accelerate")
          erc-hide-list erc-hide-list-default
          erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477")

          erc-log-insert-log-on-open nil
          erc-log-channels-directory (locc ".erc-logs/")
          erc-log-enable 'erc-log-all-but-server-buffers
          erc-log-filter-function 'im:erc-filter

          erc-hide-timestamps t
          erc-prompt-for-password nil)

  (defun:hook erc-mode-hook/setup ()
    (which-function-mode 1)
    (erc-hl-nicks-mode 1))

  (erc-log-mode))

(defun im:erc-filter (topic)
  "Filter out system messges from the logs"
  (if (equal (string-match-p "^\\[[:digit:]: ]+\\*\\*\\*[[:space:]]" topic) nil) topic ""))

(defun im:erc-insert-timestamp-above (_string)
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

(defun im:erc-fill-static--skip-system-message ()
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

(defun im:erc-go-to-begin ()
  (interactive)
  (beginning-of-line)
  (cond ((looking-at "\\s-*<")
         (re-search-forward "> +" (line-end-position) t 1))
        (t (call-interactively #'back-to-indentation))))

(defun im:erc-toggle-hide-list ()
  (interactive)
  (setq erc-hide-list (if erc-hide-list nil erc-hide-list-default)))

(defun im/erc-join-groups (&optional arg)
  (interactive)
  (when (not (equal major-mode 'erc-mode))
    (user-error "Should called from erc-mode"))
  (let* ((candidates '("#emacs" "#commonlisp" "#c_lang_cn" "#linuxba"
                       "#archlinux-cn" "#archlinux"
                       "#c" "#java" "#clojure"))
         (groups (cond ((null arg)
                        (completing-read-multiple "Groups: " (im:completion-table candidates)))
                       ((or (numberp arg)
                            (string-match-p "^[0-9]+$" arg))
                        (cl-subseq candidates 0
                                   (if (numberp arg) arg (string-to-number arg))))
                       (t (list arg)))))
    (if (null groups) (user-error "No group selected"))
    (cl-loop for group in groups do (erc-cmd-JOIN group))))



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
  (im/erc-join-groups amount))



(transient-define-prefix im/assist-erc-mode ()
  ["Message begin with slash is directive:\n
  /nickname,list,kick,mode  /join,names,whois,quit  /msg,query,say,me..\n"
   [("-t" (lambda ()
            (if erc-hide-timestamps
                (propertize "Timestamp" 'face 'font-lock-doc-face)
              "Timestamp"))
     erc-toggle-timestamps)
    ]
   [("-o" (lambda ()
            (if erc-hide-list
                (propertize "Sys Msg" 'face 'font-lock-doc-face)
              "Sys Msg"))
     im:erc-toggle-hide-list)
    ]
   [("o" "Occur.." erc-occur)
    ]
   [("s" "Save Buffer in Logs" erc-save-buffer-in-logs)
    ]
   [("m" "Join.." im/erc-join-groups)
    ]
   ]
  (interactive)
  (if (eq major-mode 'erc-mode)
      (transient-setup 'im/assist-erc-mode)
    (user-error "You should invoke this in erc-mode")))
