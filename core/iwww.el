;;; iwww.el --- internet. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ic/gnus-nntps (list
                          "nntp.aioe.org"
                          "free.xsusenet.com"
                          "freenews.netfront.net"
                          "news.eternal-september.org")
  "NNTP Servers."
  :type 'list)

(defcustom ic/gnus-rss-feeds (list
                              "https://vdaily.iu.vc/weekly.xml"
                              "http://feeds.feedburner.com/zhihu-daily"
                              "http://feed.williamlong.info"
                              "http://www.matrix67.com/blog/feed")
  "RSS feeds."
  :type 'list)

(defcustom ic/gnus-mails-reciever
  '((nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)
            (nnir-search-engine imap)
            (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
            (nnmail-expiry-wait 'immediate))

    (nnimap "tmail"
            (nnimap-address "imap.qq.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)
            (nnir-search-engine imap)
            (nnmail-expiry-target "nnimap+tmail:Deleted Messages")
            (nnmail-expiry-wait 'immediate)))

  "Mails opened by GNUS."
  :type 'list)

(defcustom ic/gnus-mails-sender
  `((".*"
     (name ,user-full-name)
     (address user-mail-address)
     (signature "Mess.\n"))

    ("nnimap\\+tmail:"
     (name "nch")
     (address "lorniu@qq.com")
     ("X-Message-SMTP-Method" "smtp smtp.qq.com 465") ; explicit server
     (signature ""))

    ((header "Reply-To" ".*@replay.github.com")
     (name ,user-full-name)
     (address ,user-mail-address)
     (signature nil)
     (mail-citation-hook)
     (organization nil)
     (eval (set (make-local-variable 'message-cite-style) nil))))

  "Send mail strategy in GNUS."
  :type 'list)


;;; Read

(x gnus-start
   "For Email, can use mu4e/notmuch and so on.
   "
   "For RSS, can use elfeed/newsticker and so on.
   "
   :ref ("https://www.gnu.org/software/emacs/manual/html_node/gnus/index.html")

   :custom
   ;; default/secondary/foreign
   (gnus-select-method '(nnnil nil))
   (gnus-secondary-select-methods (append ic/gnus-mails-reciever     ; mails
                                          '((nntp "news.gmane.io")   ; emacs.devel
                                            (nntp "news.gwene.org")  ; rss
                                            )))
   (gnus-secondary-servers ic/gnus-nntps)

   ;; send mail
   (send-mail-function 'smtpmail-send-it)
   (smtpmail-smtp-server "smtp.gmail.com") (smtpmail-smtp-service 465) (smtpmail-stream-type  'ssl) ; default
   (gnus-posting-styles ic/gnus-mails-sender)

   ;; gnus
   (gnus-use-cache t)
   (gnus-asynchronous t)
   (gnus-use-full-window nil) ; don't take the entire window every time

   ;; group-buffer
   (gnus-thread-hide-subtree t)
   (gnus-permanently-visible-groups "")
   (gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n")
   (gnus-activate-level 3) ; group can refresh when level < this

   ;; summary-buffer
   (gnus-thread-indent-level 0)
   (gnus-summary-same-subject "")
   (gnus-sum-thread-tree-root nil)
   (gnus-sum-thread-tree-indent "  ")
   (gnus-sum-thread-tree-false-root nil)
   (gnus-sum-thread-tree-single-indent nil)
   (gnus-sum-thread-tree-single-leaf "    `-> ")
   (gnus-sum-thread-tree-vertical "   |")
   (gnus-sum-thread-tree-leaf-with-other "   |-> ")
   ;;
   (gnus-ancient-mark 32)
   (gnus-unread-mark 42)
   (gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date)
   (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
   (gnus-fetch-old-headers nil) ; be careful
   ;;
   (gnus-user-date-format-alist '(((gnus-seconds-today) . "      %H:%M") ((gnus-seconds-year)  . "%m/%d %H:%M") (t . " %Y.%m/%d")))
   (gnus-summary-line-format "%U%R%z%O %&user-date; | [%1{%N%}] %I%B (%3{%f%})\n")

   ;; article-buffer
   (mm-text-html-renderer 'shr) ; simple html render, part of eww
   (shr-bullet "  ")            ; unordered list
   (gnus-visible-headers (mapconcat 'regexp-quote
                                    '("From:" "To:" "Newsgroups:" "Subject:" "Date:" "Cc:" "Followup-To" "Gnus-Warnings:" "Organization:"
                                      "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:" "X-Mailer:" "Reply-To:"
                                      "X-Spam:" "X-Spam-Status:" "X-Now-Playing" "X-Attachments" "X-Diagnostic")
                                    "\\|"))

   ;; encoding
   (gnus-default-charset 'utf-8)
   (gnus-summary-show-article-charset-alist '((1 . gb2312) (2 . gb18030) (3 . gbk) (4 . big5) (5 . utf-8)))
   (gnus-group-name-charset-group-alist '(("\\.zhihu\\.com:" . utf-8) ("\\.weixin" . utf-8) (".*" . utf-8)))
   ;;(gnus-group-charset-alist '(("\\(^\\|:\\)hk\\|\\(^\\|:\\)tw" big5) ("\\(^\\|:\\)cn" gbk)))
   (gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown iso-8859-1 unknown-8bit x-unknown))

   :config
   ;; Scan news every 30 Minutes (invoke 'gnus-demon-init to active it)
   (gnus-demon-add-handler 'gnus-demon-scan-news 30 30)

   ;; display inline first
   (add-to-list 'mm-attachment-override-types "image/*")

   ;; hooks
   (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
   (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ; use local-time
   (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ; refresh timestamp
   (defun-hook gnus-summary-mode-hook () (setq line-spacing 5))

   ;; keybinds
   (transient-my-bind-service gnus-group-mode)
   (transient-my-bind-service gnus-summary-mode)
   (transient-my-bind-service gnus-article-mode)
   (define-key gnus-group-mode-map (kbd "G R") 'im/gnus-make-rss-group))

(defun im/gnus-make-rss-group ()
  (interactive nil gnus-group-mode)
  (let ((url (completing-read "URL to Search for RSS: " ic/gnus-rss-feeds)))
    (gnus-group-make-rss-group (and (> (length url) 0) url))))

(defun im/gnus-group-toggle-show-level ()
  (interactive)
  (let ((fmt "%M%S%p%P%5y:%B%(%g%)\n"))
    (setq gnus-group-line-format
          (if (string= gnus-group-line-format fmt)
              (concat "%L " fmt)
            fmt))
    (call-interactively 'gnus-group-list-groups)))

(defun im/gnus-set-activate-level ()
  (interactive)
  (let ((n (read-number "Set activate-level to: " gnus-activate-level)))
    (if (or (> n 9) (< n 1) (= n gnus-activate-level))
        (user-error "Nothing to do.")
      (setq gnus-activate-level n)
      (message "Activate-Level to %s, only groups with level < %s are actived." n n))))

(defun im/gnus-article-toggle-show-image ()
  (interactive)
  (let ((regexp "\\.gif-or-what"))
    (setq gnus-blocked-images
          (if (functionp gnus-blocked-images)
              (progn
                (message "Show images in all groups.")
                regexp)
            (message "Allow images only in newsgroups.")
            #'gnus-block-private-groups))))

(transient-define-prefix imtt/transient-gnus-group-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("" (lambda () (transient-my-description "t | T..   " "Topic..")) ignore :format " %d")
    ("" (lambda () (transient-my-description "^ | B     " "Server List")) ignore :format " %d")
    ("" (lambda () (transient-my-description "l | L | A." "Server View")) ignore :format " %d")
    ]
   [
    ("" (lambda () (transient-my-description "c/C    " "Clean Topics")) ignore :format " %d")
    ("" (lambda () (transient-my-description "C-u RET" "Show Topics..")) ignore :format " %d")
    ("" (lambda () (transient-my-description "G GSp.." "Topics Search/Sort/Edit..")) ignore :format " %d")
    ]
   [("v v    " "Show Levels" im/gnus-group-toggle-show-level)
    ("v l    " "Active level" im/gnus-set-activate-level)
    ("" (lambda () (transient-my-description "Al | Sl" "Show/Change Level")) ignore :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'gnus-group-mode)
      (transient-setup 'imtt/transient-gnus-group-mode)
    (user-error "You should invoke this in gnus-group-mode.")))

(transient-define-prefix imtt/transient-gnus-summary-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("" (lambda () (transient-my-description "A T | ^" "All/Parent Article")) ignore :format " %d")
    ("" (lambda () (transient-my-description "C-u M-g" "Show Topics..")) ignore :format " %d")
    ("" (lambda () (transient-my-description "/ owN.." "Limit Locally")) ignore :format " %d")
    ]
   [("" (lambda () (transient-my-description "T E      " "Mark Expired")) ignore :format " %d")
    ("" (lambda () (transient-my-description "! | MMdd " "Ticked/Uncache")) ignore :format " %d")
    ("" (lambda () (transient-my-description "T i | C-M-l | I | L | V." "Score..")) ignore :format " %d")
    ]
   [("v i" "Toggle Show Image"  im/gnus-article-toggle-show-image)
    ]
   ]
  (interactive)
  (if (eq major-mode 'gnus-summary-mode)
      (transient-setup 'imtt/transient-gnus-summary-mode)
    (user-error "You should invoke this in gnus-summary-mode.")))

(transient-define-prefix imtt/transient-gnus-article-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("s    "   "Show Summary"  ignore)
    ("T E  "   "Mark Expired"  ignore)
    ]
   [("" (lambda () (transient-my-description "t | C-u g " "Show Header/Raw")) ignore :format " %d")
    ("" (lambda () (transient-my-description "m|a|f|r|Sw" "Compose/Reply..")) ignore :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'gnus-article-mode)
      (transient-setup 'imtt/transient-gnus-article-mode)
    (user-error "You should invoke this in gnus-article-mode.")))


;;; Chat

(x erc
   "From rcirc (minimal) to ERC (complex)."
   :custom
   (erc-server "irc.libera.chat")
   ;; (erc-prompt "ERC>")
   ;; (erc-fill-prefix nil)
   (erc-nick "tzlm454")
   (erc-fill-column 120)
   (erc-fill-static-center 14)
   (erc-fill-function 'erc-fill-static--skip-system-message)
   (erc-insert-timestamp-function 'erc-insert-timestamp-above)

   (erc-server-reconnect-attempts 5)
   (erc-server-reconnect-timeout 3)

   (erc-keywords '("money" "bill"  "accelerate" "Accelerate"))
   (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477"))

   (erc-log-insert-log-on-open nil)
   (erc-log-channels-directory "~/.erc-logs/")
   (erc-log-enable 'erc-log-all-but-server-buffers)
   (erc-log-filter-function 'erc-my-filter)

   (erc-prompt-for-password nil)

   :bind
   (:map
    erc-mode-map
    ("M-m"    . (lambda ()
                  (interactive)
                  (beginning-of-line)
                  (cond ((looking-at "\\s-*<")
                         (re-search-forward "> +" (line-end-position) t 1))
                        (t (call-interactively #'back-to-indentation)))))
    ("C-c m"  . imtt/transient-erc-mode))

   :init
   (defun-hook erc-mode-hook/setup ()
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

   :config
   (erc-log-mode)
   (with-eval-after-load 'which-func
     (add-to-list 'which-func-non-auto-modes 'erc-mode)))

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
                  (incf users))
                (when (erc-channel-user-op-p k)
                  (incf ops)))
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
  (let* ((selectrum-should-sort)
         (candidates '("#emacs" "#commonlisp" "#c_lang_cn" "#linuxba"
                       "#archlinux-cn" "#archlinux"
                       "#c" "#java" "#clojure"))
         (groups (cond ((null arg)
                        (completing-read-multiple "Groups: " candidates))
                       ((or (numberp arg)
                            (string-match-p "^[0-9]+$" arg))
                        (subseq candidates 0
                                (if (numberp arg) arg (string-to-number arg))))
                       (t (list arg)))))
    (if (null groups) (user-error "No group selected."))
    (cl-loop for group in groups do (erc-cmd-JOIN group))))


(provide 'iwww)

;;; iwww.el ends here
