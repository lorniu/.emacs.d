;;; -*- lexical-binding: t -*-

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

(defun im:loc-agenda-file (name &optional nocheck)
  (let ((f (expand-file-name name agenda-directory)))
    (if (or nocheck (file-exists-p f)) f
      (expand-file-name "notes.org" agenda-directory))))

(defvar bbdb-file                (im:loc-agenda-file ".bbdb" t))
(defvar diary-file               (im:loc-agenda-file ".diary" t))
(defvar org-default-notes-file   (im:loc-agenda-file "notes.org"))
(defvar org-default-tasks-file   (im:loc-agenda-file "tasks.org"))
(defvar org-default-links-file   (im:loc-agenda-file "links.org"))
(defvar org-default-words-file   (im:loc-agenda-file "words.org"))
(defvar org-default-working-file (im:loc-agenda-file "working.org"))
(defvar org-default-journal-file (im:loc-agenda-file "z-journal.org"))

(setq org-startup-folded 'fold
      org-startup-indented t
      org-hide-leading-stars nil
      org-hide-block-startup nil
      org-startup-with-inline-images t
      org-pretty-entities t
      org-image-actual-width nil
      org-cycle-separator-lines 0
      org-blank-before-new-entry '((heading . t) (plain-list-item . nil))

      org-list-allow-alphabetical t
      org-use-sub-superscripts '{}
      org-fold-catch-invisible-edits 'show

      org-id-method 'ts
      org-id-link-to-org-use-id 'create-if-interactive
      org-id-locations-file (locc ".org-id-locations")
      org-attach-store-link-p t
      org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format org-attach-id-uuid-folder-format)

      org-bookmark-names-plist nil
      org-refile-targets `((org-agenda-files . (:level . 1)))

      org-confirm-babel-evaluate nil
      org-link-elisp-confirm-function nil
      org-src-window-setup 'current-window)

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-container-element "section"
      org-html-validation-link "Go ahead, never stop."
      org-html-htmlize-output-type 'inline-css
      org-html-head-include-scripts nil

      org-export-with-sub-superscripts '{}

      org-publish-list-skipped-files nil
      org-publish-timestamp-directory (locc "org-publish-timestamp/"))

(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-window-setup 'current-window
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s") (todo   . " %i %-16:c") (tags   . " %i %-12:c") (search . " %i %-12:c"))

      org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCEL(c!)"))
      org-tag-alist '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Teaching") ("Dodo" . ?d) ("Yiyi" . ?e)
                      ("Scope") ("Object") ("Function") ("Closure") ("Module") ("Concurrent") ("Network") ("IO") ("DateTime"))

      org-log-into-drawer t
      org-deadline-warning-days 5

      org-clock-into-drawer t
      org-clock-idle-time 30
      org-clock-x11idle-program-name "xprintidle" ; pacman -S xprintidle
      org-clock-persist t
      org-clock-persist-file (locc "org-clock-save.el")
      org-clock-sound t

      org-show-notification-handler #'im:org-notify-with-alert)

(setq org-link-abbrev-alist
      `(("google"  . "https://www.google.com/search?q=")
        ("github"  . "https://github.com/")
        ("notes"   . ,org-directory)
        ("wolfram" . "https://wolframalpha.com/input/?i=%s")))

(setq org-export-global-macros
      '(("details" . (lambda (&optional summary props)
                       (if (and (string= summary "/") (null props))
                           "#+HTML: </details>"
                         (concat "#+HTML: <details>"
                                 (when summary
                                   (format "<summary%s>%s</summary>"
                                           (cond ((null props) "")
                                                 ((string-match-p ":" props) (format " style='%s'" props))
                                                 (t (format " class='%s'" props)))
                                           summary))))))))

(setq org-capture-templates
      `(("j" "Journal" plain (file+function ,org-default-journal-file org-reverse-datetree-goto-date-in-file) "--- %<%T> ---\n\n%i%?" :empty-lines 1 :trim t)
        ("t" "Task" entry (file+function ,org-default-tasks-file im:org-capture-interactive) "* TODO %u %i%?" :prepend t :jump-to-captured t)
        ("w" "New Word" item (file+function ,org-default-words-file im:org-capture-interactive) "%i%?" :prepend t :jump-to-captured t)
        ("l" "Favor Link" plain (file+function ,org-default-links-file im:org-capture-interactive) "%i%?" :prepend t :jump-to-captured t)
        ("n" "Draft/Tips" entry (file ,org-default-notes-file) "* %U\n\n%i%?" :prepend t :empty-lines 1)
        ("N" "Draft/Tips*" plain (file+function ,org-default-notes-file im:org-capture-interactive) "%i%?" :jump-to-captured t)
        ("a" "Agenda..." entry (function im:org-capture-interactive) "* %i%?" :prepend t :jump-to-captured t)))



(xzz org
  :bind (:map org-mode-map
              ("×"       . "*")
              ("C-c \""  . im/org-edit-special-with-wc-setup)
              ("M-."     . org-open-at-point)
              ("M-,"     . org-mark-ring-goto)
              ("C-,"     . nil)
              ("M-h"     . nil)
              ("C-x n"   . nil)
              ("C-c /"   . nil))
  :hook ((org-mode . (lambda ()
                       (setq-local system-time-locale "C")
                       (electric-pair-local-mode 1)
                       (modify-syntax-entry ?< "w")
                       (modify-syntax-entry ?> "w")))
         ((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-after-todo-statistics . (lambda () ; Switch entry to DONE when all subentries are done
                                        (let (org-log-done org-todo-log-states)
                                          (org-todo (if (= n-not-done 0) "DONE" "TODO")))))
         (kill-emacs . (lambda () (ignore-errors (org-clock-save)))))
  :commands (org-time-stamp org-time-stamp-inactive)
  :preface (setq org-modules '(org-tempo ol))
  :init (run-with-idle-timer 30 nil (lambda () (unless (featurep 'org) (require 'org) (message "Loading org.el... done."))))
  :config
  ;; Dim
  (with-eval-after-load 'org-indent (diminish 'org-indent-mode))

  ;; Highlight broken file link
  (org-link-set-parameters "file" :face (lambda (path)
                                          (when (or (file-remote-p path)
                                                    (file-exists-p path))
                                            'org-link 'warning)))

  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; src
  (setq org-src-lang-modes
        (append '(("xml"  . sgml)
                  ("html" . mhtml)
                  ("cs"   . csharp)
                  ("csx"  . csharp)
                  ("tikz" . latex)
                  ("ps"   . powershell))
                org-src-lang-modes))

  ;; babel
  (set-default-toplevel-value
   'org-babel-load-languages
   (cl-loop with langs =
            '((awk) calc
              (powershell) shell
              (sqlite . sqlite3) sql
              (typescript . tsc) (js . node)
              (python) (ruby) (haskell . ghc) lisp
              (java) (csx . dotnet) (csharp . dotnet) (fsharp . dotnet)
              (gnuplot) (ditaa . java) (dot) (plantuml . java)
              restclient latex)
            for lang in langs
            if (and (consp lang) (executable-find (format "%s" (or (cdr lang) (car lang)))))
            do (require (intern (format "ob-%s" lang)) nil t) and collect (cons (car lang) t)
            if (and (symbolp lang) (not (eq (elt (symbol-name lang) 0) ?-)))
            do (require (intern (format "ob-%s" lang)) nil t) and collect (cons lang t)))

  ;; use cs as shortcut of csharp
  (defvar org-babel-default-header-args:cs '())
  (defun org-babel-execute:cs (body params) (org-babel-execute:csharp body params))

  ;; use ps as shortcut of powershell
  (defvar org-babel-default-header-args:ps '())
  (defun org-babel-execute:ps (body params) (org-babel-execute:powershell body params))

  ;; fixup clock
  (run-with-idle-timer 10 nil (lambda () (im:with-message nil (require 'org-clock) (org-clock-load)))))

(xzz ox
  :config
  (require 'ox-spectacle)
  (add-to-list 'org-export-filter-final-output-functions
               (defun im:org-publish-remove-zero-width-spaces-maybe (text _backend _info)
                 (unless (org-export-derived-backend-p 'org)
                   (replace-regexp-in-string "\u200b" "" text)))))

(defun:before org-link-open//edraw-org-autoload (link &optional _arg)
  (let ((path (org-element-property :path link)))
    (when (and (not (featurep 'edraw-org)) (equal "edraw:" path))
      (org-element-put-property link :type "edraw")
      (require 'edraw-org)
      (edraw-org-setup-default))))

(defun:around org-babel-execute-src-block//prefer-eval-region (f &rest params)
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
        (if (in-src-block-p (setq b (region-beginning)) (setq e (region-end)))
            (if (cadr params)
                (setf (nth 1 (cadr params)) (buffer-substring b e))
              (user-error "INFO empty?"))
          (user-error "Region must inner src-block")))
      (apply f params))))

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

(defun:around org-get-agenda-file-buffer//supress-rencentf (f &rest args)
  (with-supress-recentf (apply f args)))

(defun:before-until org-eldoc-documentation-function//echo-link-in-minibuffer (&rest _)
  "Display full link in minibuffer when cursor/mouse is over it."
  (when-let* ((link (org-element-property :raw-link (org-element-context))))
    (format "Link: %s" link)))


;;; Emphasis markers

(with-eval-after-load 'org
  ;; Support NonASCII, see `org-set-emph-re' for detail (-pre*B BODY B*post-)
  (setq org-emphasis-regexp-components
        '("-[:nonascii:][:space:]('\"{"          ; pre
          "-[:nonascii:][:space:].,:!?;'\")}\\[" ; post
          "[:space:]"  ; border (reverse match)
          "[^​]"        ; body (zero-width space should not in BODY, used it as a fixtool)
          1))          ; max newlines allowed
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Auto hide emphasis markers
  (defun:hook org-mode-hook/auto-expose-hidden-markers ()
    (require 'org-expose-emphasis-markers)
    (org-expose-emphasis-markers 'paragraph)))

(defun:override org-element--parse-generic-emphasis//for-export (mark type)
  "Enhance for export *NonAscii*, correspond to value of `org-emphasis-regexp-components'."
  (save-excursion
    (let ((origin (point)))
      (unless (bolp) (forward-char -1))
      (let ((opening-re
             (rx-to-string
              `(seq (or line-start (any space ?- ?\( ?' ?\" ?\{ nonascii)) ; 1. add nonascii here
                    ,mark (not space)))))
        (when (looking-at-p opening-re)
          (goto-char (1+ origin))
          (let ((closing-re
                 (rx-to-string
                  `(seq (not space) (group ,mark)
                        (or (any space ?- ?. ?, ?\; ?: ?! ?? ?' ?\" ?\) ?\} ?\\ ?\[ nonascii) ; 2. add nonascii here
                            line-end)))))
            (when (re-search-forward closing-re nil t)
              (let ((closing (match-end 1)))
                (goto-char closing)
                (let* ((post-blank (skip-chars-forward " \t"))
                       (contents-begin (1+ origin))
                       (contents-end (1- closing)))
                  (org-element-create
                   type
                   (append
                    (list :begin origin :end (point) :post-blank post-blank)
                    (if (memq type '(code verbatim))
                        (list :value
                              (and (memq type '(code verbatim))
                                   (org-element-deferred-create
                                    t #'org-element--substring
                                    (- contents-begin origin)
                                    (- contents-end origin))))
                      (list :contents-begin contents-begin
                            :contents-end contents-end)))))))))))))


;;; Utils

(defun im:org-completing-read-headline (&optional org-file)
  "If ORG-FILE is nil then read from current buffer."
  (let* ((completion-ignore-case t)
         (heads (org-map-entries (lambda ()
                                   (let* ((comp (org-heading-components))
                                          (padding (string-pad " " (* 4 (- (car comp) 1)))))
                                     (format "%s%s" padding (nth 4 comp))))
                                 "+LEVEL<3" (if org-file (list org-file)))))
    (string-trim (completing-read "Headline: " (im:completion-table heads)))))

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

(defun im:org-notify-with-alert (message)
  (let ((ring-bell-function nil))
    (org-clock-play-sound org-clock-sound)
    (alert message :timeout 1200 :title "Org Clock Notify" :severity 'high)))

(defun im/org-babel-tangle-current-block ()
  "Tangle the block at the point only."
  (interactive nil org-mode)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-babel-tangle)))

(defun im/org-wrap-src ()
  (interactive nil org-mode)
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
  (interactive nil org-mode)
  (org-table-map-tables 'org-table-align 'quietly))

(defun im/org-edit-special-with-wc-setup ()
  (interactive nil org-mode)
  (let* ((options '(current-window split-window-below split-window-right other-window reorganize-frame other-frame))
         (option (intern (completing-read "Edit special with: " options nil t))))
    (setq org-src-window-setup option)
    (call-interactively 'org-edit-special)))

(defun im/org-update-id-locations ()
  "Hack `org-id-update-id-locations', origin one not working on all org files"
  (interactive nil org-mode)
  (let ((dir (read-directory-name
              "Update org files under: "
              (file-name-as-directory org-directory) nil t)))
    (org-id-update-id-locations (directory-files-recursively dir ".org"))))

(defun im/org-toggle-emphasis-markers ()
  (interactive nil org-mode)
  (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-flush)
  (message "Org emphasis markers have been %s." (if org-hide-emphasis-markers "hidden" "shown" )))

;; agenda

(defun im/org-agenda-lines ()
  "Read and goto headline in `org-agenda-files'."
  (interactive)
  (consult-org-agenda "+LEVEL<3+ITEM<>{^[<\\[]}-Archived-elfeed-Config")
  (im:org-show-plain-text-and-children-headlines))

(defun im/org-agenda-files ()
  "Fast open file in `org-agenda-files'."
  (interactive)
  (let ((f (completing-read "Agenda files: " (im:completion-table org-agenda-files nil "file") nil t)))
    (find-file f)
    (when (= (point) 1)
      (re-search-forward "^\\*+ " nil t)
      (beginning-of-line))))

;; publish

(defun im/org-publish-clear-timestamp-directory ()
  (interactive)
  (when (file-exists-p org-publish-timestamp-directory)
    (delete-directory org-publish-timestamp-directory t)))

;; org protocol

(defun im/org-protocol-enable-for-linux ()
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

(xzz org-img
  :config
  (setq org-img-backend
        (cond ((executable-find "wget")          "wget \"%s\" -O \"%s\"")
              ((executable-find "curl")          "curl \"%s\" -o \"%s\""))
        org-img-edit-method
        (cond ((executable-find "gimp")          "gimp '%s'")
              ((executable-find "mogrify")       "mogrify -quality 100 '%s'"))
        org-img-clipboard-method
        (cond ((executable-find "xclip")         "xclip -selection clipboard -t image/png -o > '%s'")
              ((executable-find "powershell")    "powershell -Command (Get-Clipboard -Format Image).save('%s')"))
        org-img-screenshot-method
        (cond ((executable-find "screencapture") "screencapture -i '%s'")
              ((executable-find "deepin-screen-recorder") "deepin-screen-recorder -s '%s'")
              ((executable-find "scrot")         "scrot -s '%s'")
              ((executable-find "import")        "import '%s'")))
  :commands (oi/url oi/url-yank oi/clipboard oi/screenshot oi/edit oi/rename oi/delete oi/open-dired))

(xzz org-make-toc
  :ref "alphapapa/org-make-toc")

(xzz org-reverse-datetree
  :ref (org "org-reverse-datetree: akirak/org-reverse-datetree")
  :init
  (setq org-reverse-datetree-level-formats
        '("%Y"                    ; year
          ;; (lambda (time) (format-time-string "%Y-%m" (org-reverse-datetree-monday time))) ; month
          "%Y-%m-%d %a"           ; date
          )))

(xzz ob-dot
  :ref (org "ob-dot: https://graphviz.org/")
  :if (executable-find "dot") ;; choco install graphviz
  :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(xzz ob-ditaa
  :ref (org "ob-ditta: https://ditaa.sourceforge.net/")
  :if (executable-find "java")
  :config (setopt org-ditaa-jar-path (loce "share/ditaa.jar")))

(xzz ob-plantuml
  :ref (org "ob-plantuml: https://plantuml.com/zh/")
  :init
  (setq plantuml-jar-path (locc "plantuml.jar") ; plantuml-mode.el
        plantuml-default-exec-mode 'jar
        org-plantuml-jar-path plantuml-jar-path)
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
  (when (and (executable-find "java") (executable-find "dot"))
    (im/download-plantuml-jar)))

(xzz org-present
  :ref (org "org-present: rlister/org-present")
  :config
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

(xzz org-crypt
  :commands (org-encrypt-entry org-encrypt-entries org-decrypt-entry org-decrypt-entries)
  :config
  (setopt org-tags-exclude-from-inheritance '("crypt")))


;;; Interface

(transient-define-prefix im/transient-agenda () ; C-c c
  [:hide
   (lambda () t)
   ("m"   "1"  im/note-publish-interactively)
   ("N"   "2"  (lambda () (interactive) (im/note-publish t)))
   ("M"   "3"  (lambda () (interactive) (im/note-publish-interactively t)))
   ("R"   "4"  im/note-publish-reset-directories)
   ("SPC" "5"  org-timer-pause-or-continue :if-non-nil org-timer-start-time)
   ("i"   "6"  (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer??")) (call-interactively #'org-timer-item))))
   ("W"   "7"  im/view-freshwords)]
  [["Agenda"
    ("l"  "Agenda Lines"   im/org-agenda-lines)
    ("f"  "Agenda Files"   im/org-agenda-files)]
   ["Notes"
    ("  p"  "   Capture"  org-capture)
    ("n" (lambda () (!tdesc "nN mMR"  " Publish..")) im/note-publish :format "%d")]
   [:description
    (lambda () (cond (org-timer-countdown-timer (format "%s/%s" (propertize "Countdown" 'face 'transient-heading) (propertize (if (eq org-timer-countdown-timer 'paused) "Paused" "Running") 'face 'warning)))
                     (org-timer-start-time (format "%s/%s" (propertize "Timer" 'face 'transient-heading) (propertize "Running" 'face 'warning)))
                     (t "Timer/Countdown")))
    ("t"
     (lambda () (if (and org-timer-start-time (null org-timer-countdown-timer)) "    Reset" "  New Timer"))
     (lambda () (interactive) (if (or (null org-timer-start-time) (y-or-n-p "Reset the Timer ?")) (org-timer-start) (message "Do Nothing.")))
     :if (lambda () (or (null org-timer-countdown-timer) (null org-timer-start-time))))
    ("d"
     (lambda () (if org-timer-countdown-timer "    Reset" "New Countdown"))
     org-timer-set-timer
     :if (lambda () (or org-timer-countdown-timer (null org-timer-start-time))))
    ("S"
     (lambda () (!tdesc "S/SPC" "Stop/Pause"))
     (lambda () (interactive) (if (y-or-n-p "Stop it ?") (org-timer-stop) (message "Do Nothing.")))
     :format "%d" :if-non-nil org-timer-start-time)]
   ["Clock"
    (" c " "Goto Clock" org-clock-goto)
    ("r  "
     (lambda () (!tdesc "i/r" "Record Timer"))
     (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer?")) (call-interactively #'org-timer)))
     :format " %d")]
   ["Miscellaneous"
    ("v" (lambda () (if evil-local-mode " -vil" " Evil")) evil-local-mode)
    ("w" (lambda () (!tdesc "wWp" "Words")) im/pick-freshword :format "%d")]
   ]
  (interactive)
  (require 'ox-publish)
  (transient-setup 'im/transient-agenda))

(transient-define-prefix im/assist-org-mode () ; C-c m
  [:hide
   (lambda () t)
   ("s"    "1"   org-schedule)
   ("v T"  "2"   (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'org-babel-tangle))))
   ("v f"  "3"   org-babel-tangle-file)
   ("v C"  "4"   org-babel-tangle-clean)
   ("v D"  "5"   org-babel-detangle)
   ("w"    "7"   mpvi-control)]
  [["Schedule/TimeIt"
    ("d" (lambda () (!tdesc "s/d" "schedule/deadline")) org-deadline :format " %d")
    ""
    ("c i"   "Clock In"                org-clock-in     :if-not (lambda () (org-clocking-p)))
    ("c o"   "Clock Out"               org-clock-out    :if (lambda () (org-clocking-p)))
    ("c u"   "Clock Cancel"            org-clock-cancel :if (lambda () (org-clocking-p)))
    ("c d"   "Clock Display"           org-clock-display)
    ("c r"   "Clock Report"            org-clock-report)
    ("c e"   "Effort Estimate"         org-clock-modify-effort-estimate)
    ("C-y"   "Evaluate Timespan "      org-evaluate-time-range)
    ]
   ["Prop/Drawer"
    ("t  "   "Org Todo"                org-todo)
    ("q  "   "Org Tag"                 org-set-tags-command)
    (",  "   "Org Priority"            org-priority)
    ("x p"   "Set Property"            org-set-property)
    ("x x"   "Insert Dblock"           org-dynamic-block-insert-dblock)
    ("x a"   "Toggle Archive Tag"      org-toggle-archive-tag)
    ]
   ["Refactor"
    ("C-w"   "Org Refile"              org-refile)
    ("r W"   "Org Copy"                org-copy)
    ("r $"   "Archive Subtree"         org-archive-subtree)
    ""
    ("l s"   "Store Link"              org-store-link)
    ("l i"   "Insert Link"             org-insert-link)
    ("l o"   "Open Current"            org-open-at-point)
    ]
   ["View"
    ("v c"   "Org Columns"             org-columns)
    ("v ;"   "Toggle Comment"          org-toggle-comment)
    ("v :"   "Toggle Fixed-Width"      org-toggle-fixed-width)
    ("v l"   "Toggle Link-Display"     org-toggle-link-display)
    ("v m"   "Toggle Emphasis-Markers" im/org-toggle-emphasis-markers)
    ("v v"   "Toggle Inline-Images"    org-toggle-inline-images)
    ("v \\"  "Toggle Pretty-Entities"  org-toggle-pretty-entities)
    ]
   ["Misc"
    ("C-z"   "Add Note"                org-add-note)
    ("C-a"   "Add Attach"              org-attach)
    ("\\"    "Org Sparse Tree"         org-sparse-tree)
    ""
    ("m"   "Demarcate Block"           org-babel-demarcate-block)
    ("g"   "Goto Named Block"          org-babel-goto-named-src-block)
    ("v t" (lambda () (!tdesc "v t/T/f/C/D" "Tangle")) org-babel-tangle :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'org-mode)
      (transient-setup 'im/assist-org-mode)
    (user-error "Sorry, but this is not org-mode")))
