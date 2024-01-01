;;; -*- lexical-binding: t -*-

;; For Email, can use gnus/rmail/mu4e/notmuch and so on.
;; For RSS, can use gnus/elfeed/newsticker and so on.

;; Rmail:
;;
;;  pacman -S mailutils (for movemail)
;;  movemail -v 'imaps://lorniu:pass@imap.qq.com:993' ~/RMAIL

;; GNUS:
;;
;;   - nntp.aioe.org
;;   - free.xsusenet.com
;;   - freenews.netfront.net
;;   - news.eternal-september.org

;; [Servers - Groups - Articles]
;;
;;  - In Group Buffer, use `^' to Open Servers Buffer, where list all servers
;;  - Add or config servers via `gnus-select-method' or `gnus-secondary-select-methods'
;;  - Subscribe Groups in Servers Buffer; Add foreign groups via `G_mRu..' or `B' in Group Buffer
;;
;;  - Set group level with `Sl'. Use `gnus-activate-level' to judge which groups refresh at startup
;;  - Change group behavior via parameter by `Gc' or `Gp', recommended to make some groups always visiable by add `(visiable . t) to the group parameter
;;  - Use `t' to toggle topic-mode, use `l' and 'L' to toggle show active level, use `c' to clean unread
;;  - Use `SPC' to select group and read the first unread article.
;;  - Use `Enter' to select group and show all unread articles, with C-u prefix to submit how many to display
;;
;;  - Use `SPC', `nN.j..' to nav next article
;;  - `g' to show article, with `C-u' prefix to show raw, `t' to toggle show header
;;  - `h' to switch between Summary and Article buffer
;;  - `^' to goto parent article, `A_RT' to show all ancestor

;;; Code:

(defvar im.gnus-rss-urls
  (list "https://vdaily.iu.vc/weekly.xml"
        "http://feeds.feedburner.com/zhihu-daily"
        "http://feed.williamlong.info"
        "http://www.matrix67.com/blog/feed"))

(setq user-full-name "lorniu"
      user-mail-address "lorniu@gmail.com")

;; https://myaccount.google.com/apppasswords
;; machine smtp.gmail.com port 587 login [EMAIL] password [APPPASS]
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      smtpmail-smtp-user user-mail-address
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(setq gnus-select-method '(nnnil "") ; legacy, not use
      gnus-secondary-select-methods
      `((nntp "news.gmane.io")
        (nntp "news.gwene.org")
        (nnatom "https://www.reddit.com/r/emacs/.rss") ; reddit
        (nnimap "gmail"
                ;; machine gmail port imaps login [EMAIL] password [APPPASS]
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
      gnus-posting-styles
      `((".*"
         (name ,user-full-name)
         (address ,user-mail-address)
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
      ;; groups
      gnus-inhibit-startup-message t
      gnus-use-dribble-file nil
      gnus-use-full-window nil ; don't take the entire window every time
      gnus-check-new-newsgroups nil
      gnus-asynchronous t
      gnus-activate-level 2 ; when startup, only fetch group leval <= 2, Use S l to change group level
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      ;; summary
      gnus-thread-hide-subtree nil
      gnus-user-date-format-alist '(((gnus-seconds-today) . "Today %H:%M") ((gnus-seconds-year)  . "%m/%d %H:%M") (t . " %Y.%m/%d"))
      gnus-summary-line-format "%U%R%z%I%(%[ %&user-date;: %-21,21f %]%) %s\n"
      ;; article
      gnus-visible-headers (mapconcat 'regexp-quote
                                      '("From:" "To:" "Newsgroups:" "Subject:" "Date:" "Cc:" "Followup-To" "Gnus-Warnings:" "Organization:"
                                        "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:" "X-Mailer:" "Reply-To:"
                                        "X-Spam:" "X-Spam-Status:" "X-Now-Playing" "X-Attachments" "X-Diagnostic")
                                      "\\|")
      ;; encoding
      gnus-default-charset 'utf-8
      gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown iso-8859-1 unknown-8bit x-unknown)
      gnus-summary-show-article-charset-alist '((1 . gb2312) (2 . gb18030) (3 . gbk) (4 . big5) (5 . utf-8))
      gnus-group-name-charset-group-alist '(("\\.weixin" . utf-8)
                                            ("\\.zhihu\\.com:" . utf-8)
                                            (".*" . utf-8)))

(xzz gnus-start
  :ref (gnus "https://www.gnu.org/software/emacs/manual/html_node/gnus/index.html")
  :config

  ;; filter select methods
  (setopt gnus-secondary-select-methods
          (cl-remove-if (lambda (method) (or (assoc 'disable method) (and (eq (car method) 'nnimap) (null (lookup-password :host (cadr method))))))
                        gnus-secondary-select-methods))

  ;; display inline first
  (add-to-list 'mm-attachment-override-types "image/*")

  ;; Hooks
  (defun:hook gnus-group-mode-hook ()
    ;; enable topic mode
    (gnus-topic-mode 1)
    ;; After some idle time, check for new groups & activate groups to level 3
    (run-with-idle-timer 60 nil (lambda ()
                                  (gnus-find-new-newsgroups)
                                  (gnus-activate-all-groups 3))))
  (defun:hook gnus-sum-load-hook/keys-overrides ()
    (define-key gnus-summary-mode-map "t"
                (lambda ()
                  (interactive)
                  (call-interactively
                   (if (cl-find-if (lambda (w) (string-match-p "^\\*Article" (buffer-name (window-buffer w))))
                                   (window-list))
                       'gnus-summary-toggle-header
                     'gnus-summary-toggle-threads))))
    (define-key gnus-summary-mode-map "q"
                (lambda ()
                  (interactive)
                  (call-interactively
                   (if (cl-find-if (lambda (w) (string-match-p "^\\*Article" (buffer-name (window-buffer w))))
                                   (window-list))
                       'gnus-summary-expand-window
                     'gnus-summary-exit)))))
  (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ; use local-time
  (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ; refresh timestamp

  ;; Keybinds
  (define-key gnus-group-mode-map (kbd "C-c m") 'im/assist-gnus-group-mode)
  (define-key gnus-summary-mode-map (kbd "C-c m") 'im/assist-gnus-summary-mode)
  (define-key gnus-article-mode-map (kbd "C-c m") 'im/assist-gnus-article-mode)
  (define-key gnus-group-mode-map (kbd "G R") 'im/gnus-make-rss-group))


;;; Helpers

(defun im/gnus-group-toggle-level-number ()
  "Toggle show level number in the group line."
  (interactive nil gnus-group-mode)
  (let ((fmt "%M%S%p%P%5y:%B%(%g%)\n"))
    (setopt gnus-group-line-format
            (if (string= gnus-group-line-format fmt) (concat "%L " fmt) fmt))
    (call-interactively 'gnus-group-list-groups)))

(defun im/gnus-threads-setup ()
  (interactive nil gnus-summary-mode gnus-group-mode)
  (let* ((subtree (completing-read "Thread hide subtree: " '("yes" "no") nil t nil nil
                                   (if gnus-thread-hide-subtree "yes" "no")))
         (gathering (completing-read "Threads gather by: " '("subject" "references") nil t nil nil
                                     (if (eq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
                                         "subject" "references")))
         (sorts '(gnus-thread-sort-by-number
                  gnus-thread-sort-by-author
                  gnus-thread-sort-by-recipient
                  gnus-thread-sort-by-subject
                  gnus-thread-sort-by-date
                  gnus-thread-sort-by-score
                  gnus-thread-sort-by-most-recent-number
                  gnus-thread-sort-by-most-recent-date
                  gnus-thread-sort-by-newsgroups
                  gnus-thread-sort-by-random
                  gnus-thread-sort-by-total-score))
         (sort (completing-read-multiple "Threads sort by (main at last):"
                                         sorts nil t nil nil
                                         (mapcar #'symbol-name (ensure-list gnus-thread-sort-functions)))))
    (setopt gnus-thread-hide-subtree (equal subtree "yes"))
    (setopt gnus-summary-thread-gathering-function (if (equal gathering "references")
                                                       #'gnus-gather-threads-by-references
                                                     #'gnus-gather-threads-by-subject))
    (setopt gnus-thread-sort-functions (mapcar #'intern-soft sort))
    (message "gnus-thread-hide-subtree: %s\ngnus-summary-thread-gathering-function: %s\ngnus-thread-sort-functions: %s"
             gnus-thread-hide-subtree gnus-summary-thread-gathering-function gnus-thread-sort-functions)))

(defun im/gnus-make-rss-group ()
  "Subscribe RSS group from URL."
  (interactive nil gnus-group-mode)
  (let ((url (completing-read "URL to Search for RSS: " im.gnus-rss-urls)))
    (gnus-group-make-rss-group (and (> (length url) 0) url))))

(defun im/gnus-article-toggle-show-image ()
  (interactive nil gnus-group-mode)
  (let ((regexp "\\.gif-or-what"))
    (setopt gnus-blocked-images
            (if (functionp gnus-blocked-images)
                (progn
                  (message "Show images in all groups.")
                  regexp)
              (message "Allow images only in newsgroups.")
              #'gnus-block-private-groups))))

(transient-define-prefix im/assist-gnus-group-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("" (lambda () (!tdesc "^/B" "Server..")) ignore :format " %d")
    ("G" "  Group.." (lambda ()
                       (interactive)
                       (im:execute-command-matching "Groups: "
                         '("^gnus-group.*\\(group$\\|edit-group\\|virtual$\\|directory$\\)"
                           im/gnus-make-rss-group
                           im/gnus-group-toggle-level-number))))
    ("t" "  Topic.." (lambda ()
                       (interactive)
                       (im:execute-command-matching "Topic: " "^gnus-topic" "sort")))]
   [("l" "  List.." (lambda ()
                      (interactive)
                      (im:execute-command-matching "View list: "
                        '("^gnus-group.*\\(list\\|apropos\\)" gnus-topic-mode))))
    ("s" "  Sort.." (lambda ()
                      (interactive)
                      (im:execute-command-matching "Sort: " "^gnus-.*sort")))
    ("L" "  Level.." (lambda ()
                       (interactive)
                       (im:execute-command-matching "Group level: " "gnus-group.*level")))]
   [("" (lambda () (!tdesc "RET/SPC" "Read..")) ignore :format " %d")
    ("" (lambda () (!tdesc "g/M-g  " "Refresh..")) ignore :format " %d")
    ("" (lambda () (!tdesc "c/C/M-c" "Clean..")) ignore :format " %d")
    ]
   [("" (lambda () (!tdesc "G G" "Query..")) ignore :format " %d")
    ("" (lambda () (!tdesc "M-e/G epc/W e" "Edit..")) ignore :format " %d")
    ("" (lambda () (!tdesc "s/u/#/F/R/ami, im/gnus-make-rss-group, im/gnus-threads-setup" "")) ignore :format " %d")
    ]]
  (interactive)
  (if (eq major-mode 'gnus-group-mode)
      (transient-setup 'im/assist-gnus-group-mode)
    (user-error "You should invoke this in gnus-group-mode")))

(transient-define-prefix im/assist-gnus-summary-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("m" "Mark.." (lambda ()
                    (interactive)
                    (im:execute-command-matching "Mark: "
                      "^gnus-summary.*\\(mark\\|tick\\|expire\\|kill\\)\\|gnus-cache.*art"
                      "put-mark\\|limit-")))
    ("/" "Limit.." (lambda ()
                     (interactive)
                     (im:execute-command-matching "Limit to: " "^gnus-summary.*limit")))]
   [("s  " "Score.." (lambda ()
                       (interactive)
                       (im:execute-command-matching "Score: "
                         '("^gnus-summary.*score"
                           gnus-score-edit-all-score
                           gnus-score-edit-current-scores
                           gnus-summary-raise-thread
                           gnus-summary-lower-thread))))
    ("v i" "Toggle Show Image"  im/gnus-article-toggle-show-image)]
   [("" (lambda () (!tdesc "M-g | / N" "Refresh Topics.."))   ignore :format " %d")
    ("" (lambda () (!tdesc "A T | ^  " "All/Parent Article")) ignore :format " %d")]]
  (interactive)
  (if (eq major-mode 'gnus-summary-mode)
      (transient-setup 'im/assist-gnus-summary-mode)
    (user-error "You should invoke this in gnus-summary-mode")))

(transient-define-prefix im/assist-gnus-article-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("r" "Reply.." (lambda ()
                     (interactive)
                     (im:execute-command-matching "Send: "
                       '(gnus-summary-reply
                         gnus-summary-followup
                         gnus-summary-post-news
                         gnus-article-reply-with-original
                         gnus-article-wide-reply-with-original
                         gnus-article-followup-with-original))))]]
  (interactive)
  (if (eq major-mode 'gnus-article-mode)
      (transient-setup 'im/assist-gnus-article-mode)
    (user-error "You should invoke this in gnus-article-mode.")))
