;;; imnet.el --- Gnus/Irc/ETC Setting
;;; Commentary:

;;; Code:

;; (env-windows
;;  (setq url-gateway-method 'socks)
;;  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))



;;; EWW

(x eww
   :when (env-linux-vps)
   :init (setq browse-url-browser-function 'eww-browse-url))


;;; Gnus

(x gnus
   :init
   (setq gnus-select-method  ; Mail
         `(nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port ,(if (env-windows) 993 "imaps"))
                  (nnimap-stream ssl))

         gnus-secondary-select-methods  ; Usenet
         '((nntp "nntp.aioe.org")
           ;; (nntp "news.eternal-september.org")
           ;; (nntp "freenews.netfront.net")
           ))

   (setq smtpmail-smtp-server "smtp.gmail.com"
         smtpmail-smtp-service 465
         smtpmail-stream-type  'ssl
         gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

   :config
   (setq gnus-novice-user nil)
   (setq gnus-expert-user t)
   (setq gnus-show-threads t)
   (setq gnus-interactive-exit t)
   (setq gnus-use-dribble-file nil)
   (setq gnus-always-read-dribble-file nil)
   (setq gnus-asynchronous t)
   (setq gnus-large-newsgroup 200)
   (setq mm-inline-large-images t)
   (setq send-mail-function 'smtpmail-send-it)
   (setq gnus-fetch-old-headers 'some)

   (setq gnus-thread-sort-functions
         '(gnus-thread-sort-by-most-recent-date
           (not gnus-thread-sort-by-number)))

   (setq gnus-summary-same-subject ""
         gnus-sum-thread-tree-indent "  "
         gnus-sum-thread-tree-single-indent "◎ "
         gnus-sum-thread-tree-root "● "
         gnus-sum-thread-tree-false-root "☆"
         gnus-sum-thread-tree-vertical "│"
         gnus-sum-thread-tree-leaf-with-other "├─► "
         gnus-sum-thread-tree-single-leaf "╰─► "
         gnus-summary-gather-subject-limit 'fuzzy)

   (setq gnus-topic-line-format "%4{[ %n -- %A ]%v%}\n"
         gnus-group-line-format "%{%M%S%p%} %0{%1y%} %P%4{%G%}\n"
         gnus-summary-line-format "%U%R%z%O %{%-5&user-date;%} %{%ua%} %B %(%I%-50,50s%)\n")

   (defun gnus-user-format-function-a (header)
     (let ((myself (concat "lorniu"))
           (references (mail-header-references header))
           (message-id (mail-header-id header)))
       (if (or (and (stringp references) (string-match-p myself references))
               (and (stringp message-id) (string-match-p myself message-id)))
           "X" "│")))

   (setq gnus-visible-headers
         (mapconcat 'regexp-quote
                    '("From:" "Newsgroups:" "Subject:" "Date:"
                      "Organization:" "To:" "Cc:"
                      "Followup-To" "Gnus-Warnings:"
                      "X-Sent:" "X-URL:" "User-Agent:"
                      "X-Newsreader:"
                      "X-Mailer:" "Reply-To:" "X-Spam:"
                      "X-Spam-Status:" "X-Now-Playing"
                      "X-Attachments" "X-Diagnostic")
                    "\\|"))

   (setq gnus-message-archive-group
         '((if (message-news-p) "miscellaneous-News" "miscellaneous-Gmail")))

   (setq gnus-posting-styles
         '((".*"
            (name "lorxiu")
            (address "loxnix@gmail.com")
            (signature "Life is busy.\n"))
           ((message-mail-p)
            (name "lorniu")
            (address "lorniu@gmail.com"))))

   (setq gnus-default-charset 'utf-8
         gnus-group-charset-alist
         '(("newsgroup\\.la" big5)
           ("\\(^\\|:\\)hk\\|\\(^\\|:\\)tw" big5)
           ("\\(^\\|:\\)cn" gbk))
         gnus-summary-show-article-charset-alist
         '((1 . gb2312)
           (2 . gb18030)
           (3 . gbk)
           (4 . big5)
           (5 . utf-8))
         gnus-group-name-charset-group-alist
         '(("newsgroup\\.la" . big5)
           ("\\.com\\.cn:" . gbk)
           ("news\\.newsfan\\.net" . gbk))
         gnus-newsgroup-ignored-charsets
         '(unknown-8bit
           x-unknown iso-8859-1
           unknown-8bit x-unknown gb18030))

   (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
   (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
   (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))



;;; Rcirc

(x rcirc
   :bind (:map rcirc-mode-map ("C-c SPC" . rcirc-next-active-buffer))
   :init
   (defun rcirc-reconnect (host)
     (interactive (list (completing-read "Reconnect the Host: " rcirc-server-alist)))
     (let (cs rcirc-server-alist)
       (dolist (buf (buffer-list) (princ cs))
         (with-current-buffer buf
           (if (and (eq major-mode 'rcirc-mode)
                    (or (null (rcirc-buffer-process))
                        (string= host (process-contact (rcirc-buffer-process) :name))))
               (if (string-match-p "^#.+$" (buffer-name))
                   (push (buffer-name) cs)))))
       (ignore-errors (delete-process (get-process host)))
       (setq rcirc-server-alist `((,host :channels ,cs)))
       (rcirc nil)))

   :config

   (require 'rcirc-styles)

   (setq rcirc-default-user-name "loofee"
         rcirc-default-nick      "loofee"
         rcirc-server-alist      '(("irc.freenode.net" :channels ("#lisp" "#linuxba")))
         rcirc-authinfo          '(("freenode" nickserv "loofee" "ooo"))
         rcirc-align-width       10
         rcirc-prompt            " %t << "
         rcirc-always-use-server-buffer-flag t
         rcirc-fill-column       'frame-width
         rcirc-fill-prefix        (make-string (+ 7 rcirc-align-width) ? )
         rcirc-keywords          '("money" "lisp")
         rcirc-omit-responses    '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

   (add-hook-lambda 'rcirc-mode-hook
     (rcirc-track-minor-mode 1)
     (unless rcirc-omit-mode
       (rcirc-omit-mode)
       (diminish 'rcirc-omit-mode ".")))

   (defun rcirc-generate-new-buffer-name (process target)
     (substring-no-properties
      (or target (concat "[" (process-name process) "]"))))

   (defun rcirc-short-buffer-name (buffer)
     (let ((bn (buffer-name buffer)) sn)
       (cond ((string-match "^\\[irc\\.\\([^ .]+\\)" bn)
              (setq sn (match-string 1 bn)))
             (t (with-current-buffer buffer
                  (setq sn (or rcirc-short-buffer-name (buffer-name))))))
       (substring sn 0 (min (length sn) 4))))

   ;; Align message according to nick.
   (add-to-list 'rcirc-markup-text-functions
                (lambda (sender response)
                  (goto-char (point-min))
                  (string-match "^..:.. \\(<[^>]+>\\|\\[[a-z]+ \\)" (buffer-string))
                  (let* ((nick (match-string 1 (buffer-string)))
                         (nick* (format (format "%%%ds" rcirc-align-width) nick)))
                    (while (and nick (search-forward nick nil t))
                      (replace-match nick* nil t)))))

   ;; inhibit showing user-list when join
   (defvar rcirc-hide-userlist t)

   (defadvice rcirc-handler-353 (around ad--353 activate)
     (if (not rcirc-hide-userlist) ad-do-it))

   (defadvice rcirc-handler-366 (around ad--366 activate)
     (if (not rcirc-hide-userlist) ad-do-it))

   (defadvice rcirc-handler-JOIN (before ad--join activate)
     (setq rcirc-hide-userlist t))

   (defadvice rcirc-cmd-names (before ad--cmd-names activate)
     (setq rcirc-hide-userlist nil))

   (defun rcirc-handler-301 (process cmd sender args) "/away message handler.")
   (defun rcirc-handler-372 (process cmd sender args) "/welcome handler."))


(provide 'imnet)

;;; imnet.el ends here
