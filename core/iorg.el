;;; iorg.el --- Org-Mode -*- lexical-binding: t -*-

;; http://orgmode.org/manual/index.html

;; try and uninstall `org-transclusion', IMO, it's a enhanced #+INCLUDE. not perfect now, maybe try another day. 2022-07-10

;;; Code:

(defcustom agenda-directory (or (loco "000/" t) (loco "111/" t) (loco "./"))
  "Main directory of agenda. Save notes/tasks.org and so on."
  :type 'directory
  :group 'imfine)

(defcustom org-agenda-files (file-expand-wildcards (expand-file-name "*.org" agenda-directory))
  "The files to be used for agenda display."
  :type 'list
  :group 'imfine)



(setq org-startup-folded t)
(setq org-startup-indented t)
(setq org-hide-leading-stars nil)
(setq org-hide-block-startup nil)
(setq org-startup-with-inline-images t)
(setq org-pretty-entities t)
(setq org-list-allow-alphabetical t)
(setq org-image-actual-width nil)
(setq org-use-sub-superscripts '{})
(setq org-cycle-separator-lines 0)
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
(setq org-fold-catch-invisible-edits 'show)
(setq org-imenu-depth 4)

(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-id-method 'ts)
(setq org-id-locations-file (locc ".org-id-locations"))
(setq org-attach-store-link-p t)
(setq org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format org-attach-id-uuid-folder-format))

(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-container-element "section")
(setq org-html-validation-link "Go ahead, never stop.")
(setq org-html-htmlize-output-type 'inline-css)
(setq org-html-head-include-scripts nil)
(setq org-export-with-sub-superscripts '{})
(setq org-publish-list-skipped-files nil)
(setq org-publish-timestamp-directory (locc "org-publish-timestamp/"))
(setq org-export-global-macros '(("details" . (lambda (&optional summary props)
                                                (if (and (string= summary "/") (null props))
                                                    "#+HTML: </details>"
                                                  (concat "#+HTML: <details>"
                                                          (when summary
                                                            (format "<summary%s>%s</summary>"
                                                                    (cond ((null props) "")
                                                                          ((string-match-p ":" props) (format " style='%s'" props))
                                                                          (t (format " class='%s'" props)))
                                                                    summary))))))))

(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)
(setq org-link-abbrev-alist `(("google"  . "https://www.google.com/search?q=")
                              ("github"  . "https://github.com/")
                              ("notes"   . ,org-directory)
                              ("wolfram" . "https://wolframalpha.com/input/?i=%s")))

(defun im:loc-agenda-file (name &optional nocheck)
  (let ((f (expand-file-name name agenda-directory)))
    (if (or nocheck (file-exists-p f)) f
      (expand-file-name "notes.org" agenda-directory))))

(defvar org-default-notes-file   (im:loc-agenda-file "notes.org"))
(defvar org-default-tasks-file   (im:loc-agenda-file "tasks.org"))
(defvar org-default-favors-file  (im:loc-agenda-file "favors.org"))
(defvar org-default-words-file   (im:loc-agenda-file "words.org"))
(defvar org-default-working-file (im:loc-agenda-file "working.org"))
(defvar org-default-journal-file (im:loc-agenda-file "z-journal.org"))
(defvar bbdb-file                (im:loc-agenda-file ".bbdb" t))
(defvar diary-file               (im:loc-agenda-file ".diary" t))

(setq org-capture-templates
      `(("j" "Journal" plain (file+function ,org-default-journal-file org-reverse-datetree-goto-date-in-file) "--- %<%T> ---\n\n%i%?" :empty-lines 1 :trim t)
        ("t" "Task" entry (file+function ,org-default-tasks-file im:org-capture-interactive) "* TODO %u %i%?" :prepend t :jump-to-captured t)
        ("w" "New Word" item (file+function ,org-default-words-file im:org-capture-interactive) "%i%?" :prepend t :jump-to-captured t)
        ("l" "Favor Link" plain (file+function ,org-default-favors-file im:org-capture-interactive) "%i%?" :prepend t :jump-to-captured t)
        ("n" "Draft/Tips" entry (file ,org-default-notes-file) "* %U\n\n%i%?" :prepend t :empty-lines 1)
        ("N" "Draft/Tips*" plain (file+function ,org-default-notes-file im:org-capture-interactive) "%i%?" :jump-to-captured t)
        ("a" "Agenda..." entry (function im:org-capture-interactive) "* %i%?" :prepend t :jump-to-captured t)))
(put 'org-capture-templates 'origin org-capture-templates)
(setq org-capture-bookmark nil)

(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-window-setup 'current-window
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo   . " %i %-16:c")
                                 (tags   . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)
(setq org-deadline-warning-days 5)
(setq org-clock-idle-time 30
      org-clock-x11idle-program-name "xprintidle" ; pacman -S xprintidle
      org-clock-persist t
      org-clock-persist-file (locc "org-clock-save.el")
      org-clock-sound t)
(setq org-tag-alist
      '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Teaching")
        ("Dodo" . ?d) ("Yiyi" . ?e)
        ("Scope") ("Object") ("Function") ("Closure") ("Module") ("Concurrent") ("Network") ("IO") ("DateTime"))
      org-todo-keywords
      '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCEL(c!)")))
(setq org-refile-targets `((org-agenda-files . (:level . 1))))
(setq org-show-notification-handler (lambda (m)
                                      (let ((ring-bell-function nil))
                                        (org-clock-play-sound org-clock-sound)
                                        (alert m :timeout 1200 :title "Org Clock Notify" :severity 'high))))

(x org
   :commands (org-time-stamp org-time-stamp-inactive)
   :bind ( :map org-mode-map
           ("×"       . "*")
           ("C-c \""  . im/org-edit-special-with-wc-setup)
           ("M-."     . org-open-at-point)
           ("M-,"     . org-mark-ring-goto)
           ("C-,"     . nil)
           ("M-h"     . nil)
           ("C-x n"   . nil)
           ("C-c /"   . nil))
   :config
   (let ((inhibit-message t))
     (require 'org-img)
     (require 'org-tempo) ; org-insert-structure-template
     (require 'org-special-block-extras)
     (require 'org-clock)
     (require 'org-timer)
     ;;(require 'mpvi)
     (require 'ox-spectacle)
     (require 'edraw-org)
     (edraw-org-setup-default))

   ;; babel switcher
   (cl-loop for l in '(shell powershell typescript
                             lisp python ruby haskell java js csx csharp fsharp
                             sql sqlite
                             gnuplot ditaa dot plantuml calc
                             restclient latex awk)
            unless (eq (elt (symbol-name l) 0) ?-) collect `(,l . t) into lst
            finally (org-babel-do-load-languages 'org-babel-load-languages lst))

   ;; assoc with which mode
   (cl-loop with setups = '(("xml" . sgml)
                            ("html" . mhtml)
                            ("cs" . csharp)
                            ("csx" . csharp)
                            ("tikz" . latex)
                            ("ps" . powershell))
            for s in setups do (cl-pushnew s org-src-lang-modes))

   ;; use cs as shortcut of csharp
   (defvar org-babel-default-header-args:cs '())
   (defun org-babel-execute:cs (body params) (org-babel-execute:csharp body params))

   ;; use ps as shortcut of powershell
   (defvar org-babel-default-header-args:ps '())
   (defun org-babel-execute:ps (body params) (org-babel-execute:powershell body params))

   ;; fixup image display
   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

   ;; execute only the selection
   (defun:around org-babel-execute-src-block//region (f &rest params)
     "If region exists, only execute the selected code."
     (cl-flet ((in-src-block-p (beg end)
                 (org-with-wide-buffer
                  (goto-char beg)
                  (let ((element (org-element-at-point)))
                    (and (eq (org-element-type element) 'src-block)
                         (> (line-beginning-position)
                            (org-element-property :post-affiliated element))
                         (> (progn (goto-char (org-element-property :end element))
                                   (skip-chars-backward " \t\n")
                                   (line-beginning-position))
                            end))))))
       (let (b e)
         (when (use-region-p)
           (if (in-src-block-p (setq b (region-beginning))
                               (setq e (region-end)))
               (if (cadr params)
                   (setf (nth 1 (cadr params)) (buffer-substring b e))
                 (user-error "INFO empty?"))
             (user-error "Region must inner src-block")))
         (apply f params))))

   ;; lighter
   (delight 'org-indent-mode nil)

   ;; mono-font
   (when IS-G
     (let ((f/font-face-family (f/get :font-mono)))
       (f/font-of-face 'org-table)
       (f/font-of-face 'org-column)))

   ;; keywords
   (font-lock-add-keywords 'org-mode
                           '(("\\\\\\\\$" 0 'org-meta-line)
                             ("\\<\\(FIXME\\|NOTE\\|AIA\\|TODO\\):" 1 'font-lock-warning-face prepend)))

   ;; Display full link in minibuffer when cursor/mouse is over it
   (defun:before-until org-eldoc-documentation-function (&rest _)
     (when-let (link (org-element-property :raw-link (org-element-context)))
       (format "Link: %s" link)))

   ;; Highlight broken file link
   (org-link-set-parameters "file"
                            :face (lambda (path)
                                    (if (or (file-remote-p path) (file-exists-p path))
                                        'org-link
                                      'font-lock-warning-face)))

   ;; Open file links in current window, rather than new ones
   (setf (alist-get 'file org-link-frame-setup) #'find-file)

   ;; Open directory links in dired
   (add-to-list 'org-file-apps '(directory . emacs))

   ;; Emphasis Hack for Chinese (ToBeImproved)
   (setq org-emphasis-regexp-components
         (list
          "-[:space:]('\"{"                                    ;; prematch
          (concat "-[:space:].,:!?;'\")}\\[" "[:nonascii:]")   ;; postmatch
          "[:space:]"                                          ;; border(forbidden)
          "."                                                  ;; body-regexp
          1))
   (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
   (org-element-update-syntax)
   (add-to-list 'org-export-filter-final-output-functions
                (defun im:org-publish-remove-zero-width-spaces-maybe (text _backend _info)
                  (unless (org-export-derived-backend-p 'org)
                    (replace-regexp-in-string "\u200b" "" text))))

   ;; Supress recentf
   (defun:around org-get-agenda-file-buffer (f &rest args)
     (with-supress-recentf (apply f args)))

   ;; fixup clock
   (run-with-idle-timer 10 nil (lambda () (im:with-message nil (org-clock-load)))))

(defun:hook org-mode-hook ()
  (setq-local system-time-locale "C")

  (electric-pair-local-mode 1)
  (org-special-block-extras-mode 1)

  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))

(defun:hook org-after-todo-statistics-hook (&rest _)
  "Switch entry to DONE when all subentries are done"
  (let (org-log-done org-todo-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun:hook kill-emacs-hook/clock ()
  (ignore-errors
    (when (featurep 'org)
      (org-clock-save))))

(defun:around org-table-sort-lines//better-string-to-number (fn &rest args)
  "Take 123,345 as number, make strings convert failed as tail."
  (let ((string-to-number-ori (symbol-function 'string-to-number)))
    (unwind-protect
        (progn
          (fset 'string-to-number
                (lambda (str)
                  (when (string-match-p "[_,]" str)
                    (setq str (replace-regexp-in-string "[_,]" "" str)))
                  (if (string-match-p "^[ \t]*[0-9]+[ \t]*" str)
                      (funcall string-to-number-ori str)
                    (if current-prefix-arg
                        0
                      (- most-positive-fixnum (if (> (length str) 0) (elt str 0) 0))))))
          (apply fn args))
      (fset 'string-to-number string-to-number-ori))))


;;; Utils

(defun im:org-completing-read-headline (&optional org-file)
  "If ORG-FILE is nil then read from current buffer."
  (let* ((completion-ignore-case t)
         (heads (org-map-entries (lambda ()
                                   (let* ((comp (org-heading-components))
                                          (padding (string-pad " " (* 4 (- (car comp) 1)))))
                                     (format "%s%s" padding (nth 4 comp))))
                                 "+LEVEL<3" (if org-file (list org-file)))))
    (string-trim (completing-read "Headline: " (im:completion-table-with-sort-fn heads)))))

(defun im:org-capture-interactive ()
  "Used as capture target function."
  (let ((file (car-safe (cdr-safe (org-capture-get :target)))))
    (if (and (stringp file) (file-exists-p file)) ; file+function
        (progn
          (set-buffer (find-file-noselect file))
          (let ((headline (im:org-completing-read-headline)))
            (org-capture-put-target-region-and-position)
	        (widen)
	        (goto-char (point-min))
            (unless (re-search-forward (format org-complex-heading-regexp-format (regexp-quote headline)) nil t)
              (goto-char (point-max))
              (or (bolp) (insert "\n"))
              (insert "* " headline "\n"))
            (line-end-position)))
      (let ((org-agenda-files (directory-files agenda-directory t "\\.org$"))) ; function
        (consult-org-agenda "+LEVEL<3+ITEM<>{^[<\\[]}-Archived-elfeed-Config")
        (im:org-show-plain-text-and-children-headlines)))))

(defun im:org-show-plain-text-and-children-headlines ()
  (org-fold-show-entry)
  (org-with-limited-levels (org-fold-show-children))
  (org-fold-show-set-visibility 'tree))

(defun im:org-in-drawer-p ()
  "If current point is in a drawer."
  (ignore-errors
    (cl-member (save-excursion
                 (org-up-element)
                 (car (org-element-at-point)))
               '(drawer property-drawer))))

(defun im/org-babel-tangle-current-block ()
  "Tangle the block at the point only."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-babel-tangle)))

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

(defun im/ox-clean-extra-spaces-when-publish (text backend _)
  "When export to HTML, delete blanks between Chinese Char."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]"))
      (setq text (replace-regexp-in-string (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp) "\\1\\2" text))             ;; Avoid `Newline' to `SPACE'
      (setq text (replace-regexp-in-string (format "\\(%s\\) +\\(<[a-z]>%s\\)" regexp regexp) "\\1\\2" text))          ;; Trim blanks before `BOLD'
      (setq text (replace-regexp-in-string (format "\\(%s</[a-z]>\\) +\\(%s\\)" regexp regexp) "\\1\\2" text)) text))) ;; Trim blanks after `BOLD'

(defun im/org-edit-special-with-wc-setup ()
  (interactive)
  (let* ((options '(current-window split-window-below split-window-right other-window reorganize-frame other-frame))
         (option (intern (completing-read "Edit special with: " options nil t))))
    (setq org-src-window-setup option)
    (call-interactively 'org-edit-special)))

(defun im/diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (when year
    (setq year (let* ((d-date (diary-make-date lunar-month lunar-day year))
                      (a-date (calendar-absolute-from-gregorian d-date))
                      (c-date (calendar-chinese-from-absolute a-date)))
                 (+ (* 100 (car c-date)) (cadr c-date)))))
  (diary-chinese-anniversary lunar-month lunar-day year mark))

(defun im/org-update-id-locations ()
  "Hack `org-id-update-id-locations', origin one not working on all org files"
  (interactive)
  (let ((dir (read-directory-name
              "Update org files under: "
              (file-name-as-directory org-directory) nil t)))
    (org-id-update-id-locations (directory-files-recursively dir ".org"))))

;; publish

(defun im/ox-clear-timestamp-directory ()
  (interactive)
  (when (file-exists-p org-publish-timestamp-directory)
    (delete-directory org-publish-timestamp-directory t)))

(defun im/ox-flush-lines-in-timestamp-file ()
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

;; agenda

(defun im/org-agenda-lines ()
  "Read and goto headline in `org-agenda-files'."
  (interactive)
  (consult-org-agenda "+LEVEL<3+ITEM<>{^[<\\[]}-Archived-elfeed-Config")
  (im:org-show-plain-text-and-children-headlines))

(defun im/org-agenda-files ()
  "Fast open file in `org-agenda-files'."
  (interactive)
  (let ((f (completing-read "Agenda files: "
                            (lambda (input pred action)
                              (if (eq action 'metadata)
                                  `(metadata (category . "file") (display-sort-function . ,#'identity))
                                (complete-with-action action org-agenda-files input pred)))
                            nil t)))
    (find-file f)
    (when (= (point) 1)
      (re-search-forward "^\\*+ " nil t)
      (beginning-of-line))))

;; fresh word

(defun im/pick-freshword (word)
  (interactive (list (read-string (format "Save to %s: "
                                          (file-name-nondirectory org-default-words-file))
                                  (im:thing-at-region-or-point))))
  (deactivate-mark)
  (when (zerop (length word))
    (user-error "Word is empty, nothing done"))
  (with-current-buffer (find-file-noselect org-default-words-file)
    (goto-char (point-min))
    (re-search-forward (format org-complex-heading-regexp-format "Picker") nil t)
    (skip-chars-forward " \n")
    (insert (format "- %-35s (%s)\n" word (time-str)))
    (save-buffer))
  (message "[%s] saved." (propertize word 'face 'font-lock-string-face)))

(defun im/view-freshwords ()
  (interactive)
  (with-current-buffer (find-file-noselect org-default-words-file)
    (goto-char (point-min))
    (re-search-forward (format org-complex-heading-regexp-format "Picker") nil t)
    (beginning-of-line)
    (outline-show-subtree)
    (pop-to-buffer (current-buffer))))

;; org protocol

(defun is/enable-org-protocol-for-linux ()
  (interactive)
  (cl-assert IS-LINUX)
  (let ((path
         "~/.local/share/applications/org-protocol.desktop")
        (content
         "[Desktop Entry]
          Name=Org-Protocol
          Exec=emacsclient %u
          Icon=emacs-icon
          Type=Application
          Terminal=false
          MimeType=x-scheme-handler/org-protocol")
        (xdg-command
         "xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol"))
    (if (file-exists-p path) (message "%s already exists." path)
      (with-current-buffer (find-file path)
        (insert (im:string-indent content)))
      (shell-command xdg-command))))


;;; PlugIns

(x ob-dot
   :ref (org "ob-dot: https://graphviz.org/")
   :if (executable-find "dot") ;; choco install graphviz
   :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :ref (org "ob-ditta: https://ditaa.sourceforge.net/")
   :if (executable-find "java")
   :config (setq org-ditaa-jar-path (loce "share/ditaa.jar")))

(x ob-plantuml
   :ref (org "ob-plantuml: https://plantuml.com/zh/")
   :init
   (setq plantuml-jar-path (locc "plantuml.jar") ; plantuml-mode.el
         plantuml-default-exec-mode 'jar)
   (setq org-plantuml-jar-path plantuml-jar-path)
   (defun:around plantuml-init-once (f &rest args)
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
   :ref (org "org-present: rlister/org-present")
   :init
   (defun:hook org-present-mode-hook ()
     (org-present-big)
     (org-display-inline-images)
     (org-present-hide-cursor)
     (org-present-read-only))
   (defun:hook org-present-mode-quit-hook ()
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
   :ref (org "org-noter: weirdNox/org-noter"))

(x calendar
   :config
   (require 'cal-china-x)
   (setq calendar-week-start-day 1
         mark-holidays-in-calendar t
         cal-china-x-important-holidays cal-china-x-chinese-holidays
         calendar-holidays (append cal-china-x-important-holidays
                                   cal-china-x-general-holidays)))

(x org-reverse-datetree
   :ref (org "org-reverse-datetree: akirak/org-reverse-datetree")
   :init
   (setq-default org-reverse-datetree-level-formats
                 '("%Y"                    ; year
                   ;; (lambda (time) (format-time-string "%Y-%m" (org-reverse-datetree-monday time))) ; month
                   "%Y-%m-%d %a"           ; date
                   )))


;;; Interface

(transient-define-prefix im/transient-org-mode ()
  [:hide
   (lambda () t)
   ("s"    "1"   org-schedule)
   ("v T"  "2"   (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'org-babel-tangle))))
   ("v f"  "3"   org-babel-tangle-file)
   ("v C"  "4"   org-babel-tangle-clean)
   ("v D"  "5"   org-babel-detangle)
   ("'"    "6"   (lambda () (interactive) (poly-org-mode)))
   ("w"    "7"   mpvi-seek)]
  [["Schedule/TimeIt"
    ("d" (lambda () (!tdesc "s/d" "schedule/deadline")) org-deadline :format " %d")
    ""
    ("c i"   "Clock In"               org-clock-in     :if-not (lambda () (org-clocking-p)))
    ("c o"   "Clock Out"              org-clock-out    :if (lambda () (org-clocking-p)))
    ("c u"   "Clock Cancel"           org-clock-cancel :if (lambda () (org-clocking-p)))
    ("c d"   "Clock Display"          org-clock-display)
    ("c r"   "Clock Report"           org-clock-report)
    ("c e"   "Effort Estimate"        org-clock-modify-effort-estimate)
    ("C-y"   "Evaluate Timespan "     org-evaluate-time-range)
    ]
   ["Prop/Drawer"
    ("t  "   "Org Todo"               org-todo)
    ("q  "   "Org Tag"                org-set-tags-command)
    (",  "   "Org Priority"           org-priority)
    ("x p"   "Set Property"           org-set-property)
    ("x x"   "Insert Dblock"          org-dynamic-block-insert-dblock)
    ("x a"   "Toggle Archive Tag"     org-toggle-archive-tag)
    ]
   ["Refactor"
    ("C-w"   "Org Refile"             org-refile)
    ("r W"   "Org Copy"               org-copy)
    ("r $"   "Archive Subtree"        org-archive-subtree)
    ""
    ("l s"   "Store Link"             org-store-link)
    ("l i"   "Insert Link"            org-insert-link)
    ("l o"   "Open Current"           org-open-at-point)
    ]
   ["View"
    ("v c"   "Org Columns"            org-columns)
    ("v ;"   "Toggle Comment"         org-toggle-comment)
    ("v :"   "Toggle Fixed-Width"     org-toggle-fixed-width)
    ("v l"   "Toggle Link-Display"    org-toggle-link-display)
    ("v v"   "Toggle Inline-Images"   org-toggle-inline-images)
    ("v \\"  "Toggle pretty-entities" org-toggle-pretty-entities)
    ]
   ["Misc"
    ("C-z"   "Add Note"               org-add-note)
    ("C-a"   "Add Attach"             org-attach)
    ("\\"    "Org Sparse Tree"        org-sparse-tree)
    ""
    ("m"   "Demarcate Block"          org-babel-demarcate-block)
    ("g"   "Goto Named Block"         org-babel-goto-named-src-block)
    ("v t" (lambda () (!tdesc "v t/T/f/C/D" "Tangle")) org-babel-tangle :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'org-mode)
      (transient-setup 'im/transient-org-mode)
    (user-error "Sorry, but this is not org-mode")))

(provide 'iorg)

;;; iorg.el ends here
