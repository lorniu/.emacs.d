;;; ioox-roam.el --- Roam Research -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(x org-roam
   :ref ("org-roam/org-roam"
         "org-roam/org-roam-server"
         "jethrokuan/company-org-roam"
         "nobiot/Zero-to-Emacs-and-Org-roam"
         "Roam Official: https://roamresearch.com/")
   :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
   :delight " ❇"
   :commands (org-roam-buffer-toggle-display
              org-roam-dailies-date
              org-roam-dailies-today
              org-roam-dailies-tomorrow
              org-roam-dailies-yesterday)
   :preface
   (defvar org-roam-directory (if (file-exists-p (loco "roam/")) (loco "roam/")
                                (locc "roam/")))
   (defvar org-roam-file-exclude-regexp "sitemap.org")
   (defvar org-roam-tag-sources '(prop last-directory))
   (defvar org-roam-completion-everywhere t)
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



(defvar org-roam-default-directory org-roam-directory)

(defun roam-set-directory (&optional arg)
  (interactive "P")
  (setq org-roam-directory
        (read-directory-name "Directory: "
                             (if arg org-roam-default-directory default-directory)
                             nil t))
  (message "org-roam-directory now is: %s" org-roam-directory))


(provide 'ioox-roam)

;;; ioox-roam.el ends here
