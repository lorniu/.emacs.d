;;; ioox.el --- Org-Mode -*- lexical-binding: t -*-
;;; Commentary:

;; http://orgmode.org/manual/index.html

;;; Code:

(require 'ioox-note)
(require 'ioox-gtd)
(require 'ioox-roam)
(require 'ioox-TeX)

(x org
   :commands
   (org-time-stamp org-time-stamp-inactive)

   :bind
   (:map org-mode-map
         ("×"       . "*")
         ("C-c m"   . imtt/transient-org-mode)
         ("C-c \""  . im/org-edit-special-with-wc-setup))

   :custom
   (org-startup-indented t)
   (org-startup-folded t)
   (org-hide-leading-stars nil)
   (org-hide-block-startup nil)
   (org-startup-with-inline-images t)
   (org-image-actual-width nil)
   (org-cycle-separator-lines 0)
   (org-pretty-entities t)
   (org-list-allow-alphabetical t)

   (org-use-sub-superscripts '{})
   (org-export-with-sub-superscripts '{})
   (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

   (org-id-link-to-org-use-id t)
   (org-catch-invisible-edits 'show)
   (org-attach-store-link-p t)

   (org-html-html5-fancy t)
   (org-html-doctype "html5")
   (org-html-container-element "section")
   (org-html-validation-link "Go ahead, never stop.")
   (org-html-htmlize-output-type 'inline-css)
   (org-html-head-include-scripts nil)
   (org-publish-list-skipped-files nil)
   (org-publish-timestamp-directory (locc "org-publish-timestamp/"))

   (org-id-locations-file (locc ".org-id-locations"))
   (org-link-abbrev-alist `(("google"      . "https://www.google.com/search?q=")
                            ("gmap"        . "https://maps.google.com/maps?q=%s")
                            ("github"      . "https://github.com/")
                            ("notes"       . ,org-directory)
                            ("wolfram"     . "https://wolframalpha.com/input/?i=%s")))

   :init
   (defun-hook org-mode-hook ()
     (delight 'org-indent-mode)
     (setq-local system-time-locale "C")

     (setq-local company-idle-delay 0.2)
     (company-local-set '(company-capf company-files company-dabbrev-code))

     (electric-pair-local-mode 1)
     (org-special-block-extras-mode 1)

     (modify-syntax-entry ?< "w")
     (modify-syntax-entry ?> "w")

     (org-link-set-parameters "file" ; highlight broken file link
                              :face (lambda (path)
                                      (if (or (file-remote-p path)
                                              (file-exists-p path))
                                          'org-link
                                        'font-lock-warning-face))))

   :config
   (require 'org-img)
   (require 'org-tempo) ; org-insert-structure-template
   (require 'org-special-block-extras)

   (im/org-configuration-mathjax)
   (im/org-configuration-src-block)
   (im/org-configuration-faces)
   (im/org-configuration-misc)

   (unbind-key "C-," org-mode-map)
   (unbind-key "M-h" org-mode-map)
   (unbind-key "C-x n" org-mode-map)
   (unbind-key "C-c /" org-mode-map))

(defun im/org-configuration-mathjax ()
  (setf (cadr (assoc 'path org-html-mathjax-options))
        "https://cdn.bootcdn.net/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML")
  (setq org-html-mathjax-template "
<script type=\"text/x-mathjax-config\">
    MathJax.Hub.Config({
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",
        \"HTML-CSS\": { scale: %SCALE, linebreaks: { automatic: \"%LINEBREAKS\" }, webFont: \"%FONT\" },
        SVG: {scale: %SCALE, linebreaks: { automatic: \"%LINEBREAKS\" }, font: \"%FONT\"},
        NativeMML: {scale: %SCALE},
        TeX: { equationNumbers: {autoNumber: \"%AUTONUMBER\"}, MultLineWidth: \"%MULTLINEWIDTH\", TagSide: \"%TAGSIDE\", TagIndent: \"%TAGINDENT\" }
    });
</script>
<script src=\"%PATH\" async></script>"))

(defun im/org-configuration-src-block ()
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((lisp       . t)
                                 (shell      . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (haskell    . t)
                                 (java       . t)
                                 (restclient . t)
                                 (sql        . t)
                                 (latex      . t)
                                 (gnuplot    . t)
                                 (ditaa      . t)
                                 (dot        . t)
                                 (plantuml   . t)
                                 (js         . t)
                                 (calc       . t)
                                 (csx        . t)
                                 (csharp     . t)))

  ;; alias
  (cl-loop with setups = '(("xml" . sgml) ("html" . mhtml) ("cs" . csharp) ("csx" . csharp))
           for s in setups do (pushnew s org-src-lang-modes))

  ;; fixup
  (defvar org-babel-default-header-args:cs '())
  (defun org-babel-execute:cs (body params) (org-babel-execute:csharp body params)))

(defun im/org-configuration-faces ()
  ;; Mono
  (when IS-G
    (let ((ff/font-face-family (ff/get :font-mono)))
      (ff/font-face 'org-table)
      (ff/font-face 'org-column)))

  ;; Emphasis
  (org-set-emph-re 'org-emphasis-regexp-components
                   '("-[:multibyte:][:space:]('\"{"           ; pre
                     "-[:multibyte:][:space:].,:!?;'\")}\\["  ; post
                     "[:space:]"                              ; border
                     "."                                      ; body
                     1))                                      ; newline

  ;; Keywords
  (defun-hook org-mode-hook/keyword ()
    (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'org-meta-line)
                                  ("\\<\\(FIXME\\|NOTE\\|AIA\\|TODO\\):" 1 'font-lock-warning-face prepend)))))

(defun im/org-configuration-misc ()
  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; Remove zero-width chars when publishing
  (add-to-list 'org-export-filter-final-output-functions
               (defun my/org-publish-remove-zero-width-space (text _backend _info)
                 (unless (org-export-derived-backend-p 'org)
                   (replace-regexp-in-string "\u200b" "" text)))))

(defun:before-until org-eldoc-documentation-function$ (&rest _)
  "Display full link in minibuffer when cursor/mouse is over it."
  (when-let (link (org-element-property :raw-link (org-element-context)))
    (format "Link: %s" link)))


;;; Utils and Helpers

(defun im/org-wrap-src ()
  (interactive)
  (let ((beg "#+begin_src") (end "#+end_src") (lang (read-from-minibuffer "Please input your type: ")))
    (if (use-region-p)
        (let ((regin-beg (region-beginning)) (regin-end (region-end)))
          (save-excursion
            (goto-char regin-end) (end-of-line)
            (insert (format "\n%s\n" end))
            (goto-char regin-beg) (beginning-of-line) (open-line 1)
            (insert (concat beg " " lang))
            (ignore-errors (org-edit-special) (org-edit-src-exit) (deactivate-mark))))
      (insert (format "%s %s\n\n%s\n" beg lang end))
      (forward-line -2))))

(defun im/org-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun im/org-publish-clean-extra-spaces (text backend _)
  "When export to HTML, delete blanks between Chinese Char."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]"))
      (setq text (replace-regexp-in-string (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp) "\\1\\2" text))             ;; Avoid `Newline' to `SPACE'
      (setq text (replace-regexp-in-string (format "\\(%s\\) +\\(<[a-z]>%s\\)" regexp regexp) "\\1\\2" text))          ;; Trim blanks before `BOLD'
      (setq text (replace-regexp-in-string (format "\\(%s</[a-z]>\\) +\\(%s\\)" regexp regexp) "\\1\\2" text)) text))) ;; Trim blanks after `BOLD'

(defun im/org-clear-timestamp-directory ()
  (interactive)
  (if (file-exists-p org-publish-timestamp-directory)
      (delete-directory org-publish-timestamp-directory t)))

(defun im/flush-lines-in-org-cache ()
  (interactive)
  (let ((file (read-file-name "File: " org-publish-timestamp-directory)))
    (if (and (file-regular-p file)
             (file-writable-p file)
             (string= (file-name-extension file) "cache"))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (call-interactively 'flush-lines)
          (save-buffer)
          (kill-buffer))
      (message "Cache file not available."))))

(defun im/org-edit-special-with-wc-setup ()
  (interactive)
  (let* ((options '(current-window split-window-below split-window-right other-window reorganize-frame other-frame))
         (option (intern (completing-read "Edit special with: " options nil t))))
    (setq org-src-window-setup option)
    (call-interactively 'org-edit-special)))

(defun im/search-notes ()
  "Input like: xxx [/svg]; C-Ret force search TAG."
  (interactive)
  (unless (file-exists-p org-directory)
    (error "ORG-DIRECTORY '%s' not detected." org-directory))
  (let* ((word (im/thing-at-region-or-point 'symbol))
         (selectrum-should-sort nil)
         (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                     (define-key map [C-return] (selectrum-make-action (cand _ input)
                                                                  (if (zerop (length input)) (setq input cand))
                                                                  (when (string-match-p "^[^:]\\w+[^:]$" input)
                                                                    (setq input (concat ":" input ":")))
                                                                  (rg input "org" org-directory)
                                                                  (pop-to-buffer (rg-buffer-name))))
                                     map))
         (candidates (let ((ret))
                       (setq ret (mapcar (lambda (x) (format ":%s:" (car x))) org-tag-alist))
                       (if word (push word ret))
                       ret))
         (cand (completing-read "Tag or query (xxx /*): " candidates
                                nil nil nil
                                'org-tags-history word))
         (ext "org")
         (arr (split-string cand)))
    (when (and (plusp (length arr))
               (string-prefix-p "/" (car (last arr))))
      (setq ext (cl-subseq (car (last arr)) 1))
      (setq cand (mapconcat #'identity (butlast arr) " ")))
    (rg cand ext org-directory)
    (pop-to-buffer (rg-buffer-name))))

(defun im/org-agenda-files+ ()
  (interactive)
  (let* ((cands (lambda (input)
                  (let ((files org-agenda-files))
                    (cond ((string-prefix-p "n " input)
                           (setq input (cl-subseq input 2))
                           (setq files org-agenda-notes)))
                    `((candidates . ,files)
                      (input . ,input)))))
         (cand (selectrum--read "Agenda file: " cands
                                :require-match t)))
    (find-file cand)))


;;; Extension

(define-minor-mode org-starless-mode
  "Starless org-mode, need to track.
https://github.com/TonCherAmi/org-starless"
  :body
  (let* ((keyword
          `(("^\\(\\*+ \\).+"
             (1 (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
                nil)))))
    (if org-starless-mode
        (progn
          (font-lock-add-keywords nil keyword)
          (font-lock-fontify-buffer))
      (save-excursion
        (goto-char (point-min))
        (font-lock-remove-keywords nil keyword)
        (font-lock-fontify-buffer)))))


;;; Wonderful PlugIns

(x ob-dot
   :ref ("http://graphviz.org/")
   :if (executable-find "dot") ;; choco install graphviz
   :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :ref ("http://ditaa.sourceforge.net/")
   :if (executable-find "java")
   :config (setq org-ditaa-jar-path (loce "share/ditaa.jar")))

(x ob-plantuml
   :ref ("Homepage: https://plantuml.com/zh/")
   :init
   (setq plantuml-jar-path (loce "plantuml.jar") ; plantuml-mode.el
         plantuml-default-exec-mode 'jar)
   (setq org-plantuml-jar-path plantuml-jar-path)
   (defun:around plantuml-init-once$ (f &rest args)
     "Don't raise error when jar not found! This can distrub org-publish."
     (ignore-errors (apply f args)))
   (defun im/download-plantuml-jar ()
     (interactive)
     (if (null (file-exists-p org-plantuml-jar-path))
         (let ((org-plantuml-jar-download-script (loce "plantuml.sh")))
           (unless (file-exists-p org-plantuml-jar-download-script)
             (if (yes-or-no-p "Download plantuml.jar Now? \nOr please manually execute the script `~/.emacs.d/plantuml.sh'.\n")
                 (url-copy-file "https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar" org-plantuml-jar-path)
               (with-temp-file org-plantuml-jar-download-script
                 (insert "wget https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar -O ~/.emacs.d/plantuml.jar")))))
       (if (called-interactively-p 'any)
           (message "plantuml.jar already exists."))))
   :config
   (if (and (executable-find "java") (executable-find "dot"))
       (im/download-plantuml-jar)))

(x gnuplot
   ;; TODO: Make it work on Windows.
   :if (executable-find "gnuplot")
   :init (setq gnuplot-program "gnuplot"))

(x org-img
   :init
   (setq org-img-backend
         (cond ((executable-find "wget")          "wget \"%s\" -O \"%s\"")
               ((executable-find "curl")          "curl \"%s\" -o \"%s\"")))
   (setq org-img-edit-method
         (cond ((executable-find "gimp")          "gimp '%s'")
               ((executable-find "mogrify")       "mogrify -quality 100 '%s'")))
   (setq org-img-clipboard-method
         (cond ((executable-find "xclip")         "xclip -selection clipboard -t image/png -o > '%s'")
               ((executable-find "powershell")    "powershell -Command (Get-Clipboard -Format Image).save('%s')")))
   (setq org-img-screenshot-method
         (cond ((executable-find "screencapture") "screencapture -i '%s'")
               ((executable-find "deepin-screen-recorder") "deepin-screen-recorder -s '%s'")
               ((executable-find "scrot")         "scrot -s '%s'")
               ((executable-find "import")        "import '%s'"))))

(x org-present
   :init
   (defun-hook org-present-mode-hook ()
     (org-present-big)
     (org-display-inline-images)
     (org-present-hide-cursor)
     (org-present-read-only))
   (defun-hook org-present-mode-quit-hook ()
     (org-present-small)
     (org-remove-inline-images)
     (org-present-show-cursor)
     (org-present-read-write)))

(x org-crypt
   :commands
   (org-encrypt-entry org-encrypt-entries org-decrypt-entry org-decrypt-entries)
   :config
   (setq org-tags-exclude-from-inheritance '("crypt")))

(x org-noter
   :ref "weirdNox/org-noter")


(provide 'ioox)

;;; ioox.el ends here
