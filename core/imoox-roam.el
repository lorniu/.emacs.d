;;; imoox-roam.el --- Roam Research -*- lexical-binding: t -*-
;;; Commentary:

;; https://roamresearch.com/

;;; Code:

(x org-roam
   :ref ("org-roam/org-roam"
         "org-roam/org-roam-server"
         "jethrokuan/company-org-roam"
         "nobiot/Zero-to-Emacs-and-Org-roam")
   :hook (org-load . org-roam-mode)
   :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
   :delight " [R]"
   :commands (org-roam-buffer-toggle-display
              org-roam-dailies-date
              org-roam-dailies-today
              org-roam-dailies-tomorrow
              org-roam-dailies-yesterday)
   :preface
   (defvar org-roam-directory (if (file-exists-p (loco "roam/")) (loco "roam/")
                                (locc "roam/")))
   (defvar org-roam-file-exclude-regexp "sitemap.org")
   :bind (:map org-roam-mode-map
               (("C-c n l" . org-roam)
                ("C-c n f" . org-roam-find-file)
                ("C-c n g" . org-roam-graph-show))
               :map org-mode-map
               (("C-c n i" . org-roam-insert))
               (("C-c n I" . org-roam-insert-immediate)))
   :config
   (setq org-roam-verbose nil)
   (setq org-roam-buffer-window-parameters '((no-delete-other-windows . t)))

   (cond (IS-WIN
          (setq org-roam-graph-viewer
                (or (executable-find "chrome")
                    (executable-find "edge")
                    (let ((f "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"))
                      (if (file-executable-p f) f)))))
         (IS-LINUX-G
          (setq org-roam-graph-viewer
                (or (executable-find "google-chrome")
                    (executable-find "firefox")
                    (executable-find "chrome")))))

   (setq org-roam-capture-templates
         '(("d" "default" plain (function org-roam-capture--get-point)
            "%?"
            :file-name "roam/%<%Y%m%d%H%M%S>-${slug}"
            :head "#+title: ${title}\n#+roam_alias:\n"
            :unnarrowed t)))

   (setq org-roam-capture-immediate-template
         (append (car org-roam-capture-templates) '(:immediate-finish t)))

   (add-to-list 'org-roam-capture-ref-templates
                '("a" "Annotation" plain (function org-roam-capture--get-point)
                  "%U ${body}\n"
                  :file-name "roam/${slug}"
                  :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n\n[[抓取内容]]\n\n"
                  :immediate-finish t
                  :unnarrowed t))

   (require 'org-roam-protocol))

(x org-roam-server
   :commands (liveroam-load liveroam-view)
   :config
   (setq org-roam-server-host "127.0.0.1"
         org-roam-server-port 8171
         org-roam-server-authenticate nil
         org-roam-server-export-inline-images t
         org-roam-server-serve-files nil
         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
         org-roam-server-network-poll t
         org-roam-server-network-arrows nil
         org-roam-server-network-label-truncate t
         org-roam-server-network-label-truncate-length 60
         org-roam-server-network-label-wrap-length 20)
   (defun liveroam-load ()
     (interactive)
     (org-roam-server-mode +1)
     (message "Roam Serving at %s:%s" org-roam-server-host org-roam-server-port))
   (defun liveroam-view ()
     (interactive)
     (liveroam-load)
     (browse-url (format "http://%s:%s" org-roam-server-host org-roam-server-port))))

(x company-org-roam)



(defvar org-roam-default-directory org-roam-directory)

(defun roam-set-directory (&optional arg)
  (interactive "P")
  (setq org-roam-directory
        (read-directory-name "Directory: "
                             (if arg org-roam-default-directory default-directory)
                             nil t))
  (message "org-roam-directory now is: %s" org-roam-directory))



;; should be removed after org-9.4

(defun:override org-protocol-check-filename-for-protocol$temp (fname restoffiles _client)
  "Check if `org-protocol-the-protocol' and a valid protocol are used in FNAME.
Sub-protocols are registered in `org-protocol-protocol-alist' and
`org-protocol-protocol-alist-default'.  This is how the matching is done:
  (string-match \"protocol:/+sub-protocol\\\\(://\\\\|\\\\?\\\\)\" ...)

protocol and sub-protocol are regexp-quoted.
Old-style links such as \"protocol://sub-protocol://param1/param2\" are
also recognized.
If a matching protocol is found, the protocol is stripped from
fname and the result is passed to the protocol function as the
first parameter.  The second parameter will be non-nil if FNAME
uses key=val&key2=val2-type arguments, or nil if FNAME uses
val/val2-type arguments.  If the function returns nil, the
filename is removed from the list of filenames passed from
emacsclient to the server.  If the function returns a non-nil
value, that value is passed to the server as filename.
If the handler function is greedy, RESTOFFILES will also be passed to it.
CLIENT is ignored."
  (let ((sub-protocols (append org-protocol-protocol-alist
			                   org-protocol-protocol-alist-default)))
    (catch 'fname
      (let ((the-protocol (concat (regexp-quote org-protocol-the-protocol)
				                  ":/+")))
        (when (string-match the-protocol fname)
          (dolist (prolist sub-protocols)
            (let ((proto
		           (concat the-protocol
			               (regexp-quote (plist-get (cdr prolist) :protocol))
			               "\\(:/+\\|/*\\?\\)")))
              (when (string-match proto fname)
                (let* ((func (plist-get (cdr prolist) :function))
                       (greedy (plist-get (cdr prolist) :greedy))
                       (split (split-string fname proto))
                       (result (if greedy restoffiles (cadr split)))
		               (new-style (string-match "/*?" (match-string 1 fname))))
                  (when (plist-get (cdr prolist) :kill-client)
		            (message "Greedy org-protocol handler.  Killing client.")
		            (server-edit))
                  (when (fboundp func)
                    (unless greedy
                      (throw 'fname
			                 (if new-style
				                 (funcall func (org-protocol-parse-parameters
						                        result new-style))
			                   (warn "Please update your Org Protocol handler \
to deal with new-style links.")
			                   (funcall func result))))
		            ;; Greedy protocol handlers are responsible for
		            ;; parsing their own filenames.
		            (funcall func result)
                    (throw 'fname t))))))))
      fname)))


(provide 'imoox-roam)

;;; imoox-roam.el ends here
