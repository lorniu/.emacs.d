;;; -*- lexical-binding: t -*-

;;; Code:

(defvar org-roam-directory
  (let ((d (loco "Roam/"))) (if (file-exists-p d) (file-truename d))))

(defvar org-roam-file-exclude-regexp "sitemap.org")

(defvar origin-org-roam-directory org-roam-directory)

(defun im/set-roam-directory (&optional arg)
  (interactive "P")
  (setq org-roam-directory
        (read-directory-name "Directory: "
                             (if arg origin-org-roam-directory default-directory)
                             nil t))
  (message "org-roam-directory now is: %s" org-roam-directory))

(x org-roam
   :ref ("org-roam/org-roam"
         "org-roam/org-roam-ui"
         "https://roamresearch.com/")
   :diminish (org-roam-mode " ❇")
   :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture)
          ("C-c n j" . org-roam-dailies-capture-today))
   :config
   (setopt org-roam-capture-templates
           '(("d" "default"
              plain "%?"
              :if-new (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_aliases:\n")
              :unnarrowed t))

           org-roam-capture-ref-templates
           ;; 1) add `xdg-org-protocol.desktop`
           ;; 2) xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
           ;; 3) bookmark: javascript:location.href = 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title) + '&body=' + encodeURIComponent(window.getSelection())
           '(("r" "ref"
              plain "* ${slug}\n:PROPERTIES:\n:KEY: ${title}\n:REF: ${ref}\n:END:\n\n${body}%?"
              :if-new (file+head "roam/capture-from-web.org" "#+title: Contents Captured from Web")
              :immediate-finish t
              :empty-lines-before 2
              :empty-lines-after 1
              :unnarrowed t))

           org-roam-graph-viewer
           (cond (IS-WIN
                  (or (executable-find "chrome")
                      (executable-find "edge")
                      (let ((f "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"))
                        (if (file-executable-p f) f))))
                 (IS-LINUX-G
                  (or (executable-find "microsoft-edge-beta")
                      (executable-find "google-chrome")
                      (executable-find "firefox")
                      (executable-find "chrome"))))))

(x org-roam-ui
  :bind ("C-c n u" . org-roam-ui-mode))
