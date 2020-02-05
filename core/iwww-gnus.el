;;; iwww-gnus.el --- GNUS -*- lexical-binding: t -*-

;;
;; Main config demo:
;;
;;   (setq ic/gnus-mails-reciever
;;         '((nnimap "tmail"
;;                   (nnimap-address "imap.qq.com")
;;                   (nnimap-server-port 993)
;;                   (nnimap-stream ssl)
;;                   (nnir-search-engine imap)
;;                   (nnmail-expiry-target "nnimap+tmail:Deleted Messages")
;;                   (nnmail-expiry-wait 'immediate))))
;;
;;   (setq ic/gnus-mails-sender
;;         `((".*"
;;            (name "yaoliuyao")
;;            (address "161616161@qq.com"))))
;;

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

(setq gnus-inhibit-startup-message t)

(x gnus-start
   "For Email, can use mu4e/notmuch and so on.
   "
   "For RSS, can use elfeed/newsticker and so on.
   "
   :ref ("https://www.gnu.org/software/emacs/manual/html_node/gnus/index.html")
   :init
   ;; default/secondary/foreign
   (setq gnus-select-method '(nnnil nil))
   (setq gnus-secondary-select-methods (append ic/gnus-mails-reciever     ; mails
                                               '((nntp "news.gmane.io")   ; emacs.devel
                                                 (nntp "news.gwene.org")  ; rss
                                                 )))
   (setq gnus-secondary-servers ic/gnus-nntps)

   ;; send mail
   (setq send-mail-function 'smtpmail-send-it)
   (setq smtpmail-smtp-server "smtp.gmail.com"
         smtpmail-smtp-service 465
         smtpmail-stream-type  'ssl) ; default
   (setq gnus-posting-styles ic/gnus-mails-sender)

   ;; gnus
   (setq gnus-use-cache t)
   (setq gnus-asynchronous t)
   (setq gnus-use-full-window nil) ; don't take the entire window every time

   ;; group-buffer
   (setq gnus-thread-hide-subtree t)
   (setq gnus-permanently-visible-groups "")
   (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n")
   (setq gnus-activate-level 3) ; group can refresh when level < this

   ;; summary-buffer
   (setq gnus-thread-indent-level 0)
   (setq gnus-summary-same-subject "")
   (setq gnus-sum-thread-tree-root nil)
   (setq gnus-sum-thread-tree-indent "  ")
   (setq gnus-sum-thread-tree-false-root nil)
   (setq gnus-sum-thread-tree-single-indent nil)
   (setq gnus-sum-thread-tree-single-leaf "    `-> ")
   (setq gnus-sum-thread-tree-vertical "   |")
   (setq gnus-sum-thread-tree-leaf-with-other "   |-> ")
   ;;
   (setq gnus-ancient-mark 32)
   (setq gnus-unread-mark 42)
   (setq gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date)
   (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
   (setq gnus-fetch-old-headers nil) ; be careful
   ;;
   (setq gnus-user-date-format-alist '(((gnus-seconds-today) . "      %H:%M") ((gnus-seconds-year)  . "%m/%d %H:%M") (t . " %Y.%m/%d")))
   (setq gnus-summary-line-format "%U%R%z%O %&user-date; | [%1{%N%}] %I%B (%3{%f%})\n")

   ;; article-buffer
   (setq mm-text-html-renderer 'shr) ; simple html render, part of eww
   (setq shr-bullet "  ")            ; unordered list
   (setq gnus-visible-headers (mapconcat 'regexp-quote
                                         '("From:" "To:" "Newsgroups:" "Subject:" "Date:" "Cc:" "Followup-To" "Gnus-Warnings:" "Organization:"
                                           "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:" "X-Mailer:" "Reply-To:"
                                           "X-Spam:" "X-Spam-Status:" "X-Now-Playing" "X-Attachments" "X-Diagnostic")
                                         "\\|"))

   ;; encoding
   (setq gnus-default-charset 'utf-8)
   (setq gnus-summary-show-article-charset-alist '((1 . gb2312) (2 . gb18030) (3 . gbk) (4 . big5) (5 . utf-8)))
   (setq gnus-group-name-charset-group-alist '(("\\.zhihu\\.com:" . utf-8) ("\\.weixin" . utf-8) (".*" . utf-8)))
   ;;(gnus-group-charset-alist '(("\\(^\\|:\\)hk\\|\\(^\\|:\\)tw" big5) ("\\(^\\|:\\)cn" gbk)))
   (setq gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown iso-8859-1 unknown-8bit x-unknown))

   :defer-config
   ;; Scan news every 30 Minutes (invoke 'gnus-demon-init to active it)
   (gnus-demon-add-handler 'gnus-demon-scan-news 30 30)

   ;; display inline first
   (add-to-list 'mm-attachment-override-types "image/*")

   ;; hooks
   (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
   (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ; use local-time
   (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ; refresh timestamp
   (defun:hook gnus-summary-mode-hook () (setq line-spacing 5))

   ;; keybinds
   (transient-my-bind-service gnus-group-mode)
   (transient-my-bind-service gnus-summary-mode)
   (transient-my-bind-service gnus-article-mode)
   (define-key gnus-group-mode-map (kbd "G R") 'im/gnus-make-rss-group)
   (defun:hook gnus-sum-load-hook/keys-overrides ()
     (define-key gnus-summary-mode-map "q"  'im/gnus-summary-smart-quit)))

(defun im/gnus-summary-smart-quit ()
  (interactive)
  (if (cl-find-if (lambda (w) (string-match-p "^\\*Article" (buffer-name (window-buffer w))))
                  (window-list))
      (call-interactively 'gnus-summary-expand-window)
    (call-interactively 'gnus-summary-exit)))

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
  [[("" (lambda () (!tdesc "t | T..   " "Topic.."))     ignore :format " %d")
    ("" (lambda () (!tdesc "^ | B     " "Server List")) ignore :format " %d")
    ("" (lambda () (!tdesc "l | L | A." "Server View")) ignore :format " %d")
    ]
   [
    ("" (lambda () (!tdesc "c/C    " "Clean Topics"))   ignore :format " %d")
    ("" (lambda () (!tdesc "C-u RET" "Show Topics.."))  ignore :format " %d")
    ("" (lambda () (!tdesc "G GSp.." "Topics Search/Sort/Edit..")) ignore :format " %d")
    ]
   [("v v    " "Show Levels"  im/gnus-group-toggle-show-level)
    ("v l    " "Active level" im/gnus-set-activate-level)
    ("" (lambda () (!tdesc "Al | Sl" "Show/Change Level")) ignore :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'gnus-group-mode)
      (transient-setup 'imtt/transient-gnus-group-mode)
    (user-error "You should invoke this in gnus-group-mode.")))

(transient-define-prefix imtt/transient-gnus-summary-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("" (lambda () (!tdesc "A T | ^" "All/Parent Article")) ignore :format " %d")
    ("" (lambda () (!tdesc "C-u M-g" "Show Topics.."))      ignore :format " %d")
    ("" (lambda () (!tdesc "/ owN.." "Limit Locally"))      ignore :format " %d")
    ]
   [("" (lambda () (!tdesc "T E      " "Mark Expired"))     ignore :format " %d")
    ("" (lambda () (!tdesc "! | MMdd " "Ticked/Uncache"))   ignore :format " %d")
    ("" (lambda () (!tdesc "T i | C-M-l | I | L | V." "Score..")) ignore :format " %d")
    ]
   [("v i" "Toggle Show Image"  im/gnus-article-toggle-show-image)]
   ]
  (interactive)
  (if (eq major-mode 'gnus-summary-mode)
      (transient-setup 'imtt/transient-gnus-summary-mode)
    (user-error "You should invoke this in gnus-summary-mode.")))

(transient-define-prefix imtt/transient-gnus-article-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("s    "   "Show Summary"  ignore)
    ("T E  "   "Mark Expired"  ignore)]
   [("" (lambda () (!tdesc "t | C-u g " "Show Header/Raw")) ignore :format " %d")
    ("" (lambda () (!tdesc "m|a|f|r|Sw" "Compose/Reply..")) ignore :format " %d")]
   ]
  (interactive)
  (if (eq major-mode 'gnus-article-mode)
      (transient-setup 'imtt/transient-gnus-article-mode)
    (user-error "You should invoke this in gnus-article-mode.")))

(provide 'iwww-gnus)

;;; iwww-gnus.el ends here
