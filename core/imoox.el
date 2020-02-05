;;; imoox.el --- Org-Mode -*- lexical-binding: t -*-
;;; Commentary:

;; http://orgmode.org/manual/index.html

;;; Code:

(setq org-startup-indented t
      org-hide-leading-stars t
      org-hide-block-startup t
      org-startup-with-inline-images t
      org-image-actual-width nil
      org-cycle-separator-lines 0
      org-pretty-entities t
      org-url-hexify-p nil
      org-list-allow-alphabetical t

      org-use-sub-superscripts '{}
      org-export-with-sub-superscripts '{}
      org-blank-before-new-entry '((heading . t) (plain-list-item . nil))

      org-attach-store-link-p t)

(setq org-html-html5-fancy t
      org-html-doctype "html5"
      org-html-container-element "section"
      org-html-validation-link "Go ahead, never stop."
      org-html-htmlize-output-type 'inline-css
      org-html-head-include-scripts nil
      org-publish-list-skipped-files nil
      org-publish-timestamp-directory (locc "org-publish-timestamp/")
      org-html-mathjax-template "
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
<script src=\"%PATH\" async></script>")

(setq org-link-abbrev-alist
      `(("google"      . "https://www.google.com/search?q=")
        ("gmap"        . "https://maps.google.com/maps?q=%s")
        ("github"      . "https://github.com/")
        ("notes"       . ,org-directory)
        ("wolfram"     . "https://wolframalpha.com/input/?i=%s")))

(setq org-id-locations-file (locc ".org-id-locations"))

(defun im/org-configuration-source-block ()
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)

  (pushnew (cons "xml" 'sgml) org-src-lang-modes)
  (pushnew (cons "html" 'mhtml) org-src-lang-modes)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-babel-default-header-args (cons '(:noweb . "no") (assq-delete-all :noweb org-babel-default-header-args)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((lisp       . t)
                                 (shell      . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (haskell    . t)
                                 (java       . t)
                                 (restclient . t) ;; GET :s/hello.json
                                 (sql        . t)
                                 (latex      . t)
                                 (gnuplot    . t)
                                 (ditaa      . t)
                                 (dot        . t)
                                 (plantuml   . t)
                                 (js         . t))))

(defun im/org-configuration-faces ()
  ;; Emphasis
  (org-set-emph-re 'org-emphasis-regexp-components
                   '("：，。！、  \t('\"{"          ; prematch
                     "- ：，。！、 \t.,:!?;'\")}\\" ; postmatch
                     " \t\r\n,\"'"                ; forbidden chars
                     "."                          ; body
                     1))                          ; newlines

  ;; Mono font for align
  (when IS-G
   (let ((f/face-font-family (f/get :font-mono)))
     (f/face-font 'org-table)
     (f/face-font 'org-column)))

  ;; Add Keywords!
  (defface hi-org-break `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee")))))
    "Face for org mode \\ break"
    :group 'imfine)
  (add-hook-fun org-mode-hook/keyword ()
    (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'hi-org-break) ("\\<\\(FIXME\\|NOTE\\|AIA\\|TODO\\):" 1 'font-lock-warning-face prepend)))))

(defun im/org-configuration-some-hacks ()
  ;; Remove extra space!
  (with-eval-after-load 'ox (add-to-list 'org-export-filter-paragraph-functions 'im/org-publish-clean-extra-spaces))

  ;; Remove extra blank line for example block!
  (defun:filter-return org-element-fixed-width-parser$ (ret)
    (list 'fixed-width (plist-put (cadr ret) :value (string-trim (plist-get (cadr ret) :value))))))

(x org
   :bind
   (:map org-mode-map
         ("×" . "*")
         ("C-x a a" . im/org-wrap-src)
         ("C-c \""  . im/org-edit-special-with-wc-setup))

   :init
   (require 'imoox-note)
   (require 'imoox-gtd)
   (require 'imoox-roam)

   (add-hook-fun org-mode-hook ()
     (delight 'org-indent-mode)
     (setq-local system-time-locale "C")

     (setq-local company-idle-delay 0.2)
     (company-local-set '(company-org-roam company-capf company-files company-dabbrev-code company-yasnippet))

     (org-special-block-extras-mode t)

     (org-link-set-parameters "file" ; highlight broken file link
                              :face (lambda (path)
                                      (if (or (file-remote-p path)
                                              (file-exists-p path))
                                          'org-link
                                        'font-lock-warning-face))))

   :config
   (im/org-configuration-source-block)
   (im/org-configuration-faces)

   ;; Open file links in current window, rather than new ones
   (setf (alist-get 'file org-link-frame-setup) #'find-file)
   ;; Open directory links in dired
   (add-to-list 'org-file-apps '(directory . emacs))

   (require 'org-img)
   (require 'org-special-block-extras)
   (require 'org-tempo) ; org-insert-structure-template

   (unbind-key "C-," org-mode-map)
   (unbind-key "M-h" org-mode-map)
   (unbind-key "C-x n" org-mode-map)
   (unbind-key "C-c /" org-mode-map))

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
  (let* ((word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (and (symbol-at-point) (symbol-name (symbol-at-point)))))
         (selectrum-should-sort-p nil)
         (selectrum-minibuffer-bindings (append selectrum-minibuffer-bindings
                                                `(([C-return] . ,(selectrum-make-action (cand _ input)
                                                                   (if (zerop (length input)) (setq input cand))
                                                                   (when (string-match-p "^[^:]\\w+[^:]$" input)
                                                                     (setq input (concat ":" input ":")))
                                                                   (rg input "org" org-directory)
                                                                   (pop-to-buffer (rg-buffer-name)))))))
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
         (cand (selectrum-read "Agenda file: " cands
                               :may-modify-candidates t
                               :require-match t)))
    (find-file cand)))


;;; Extension

(define-minor-mode org-starless-mode
  "Starless org-mode, need to track.
https://github.com/TonCherAmi/org-starless"
  nil nil nil
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

(x org-superstar
   :ref "integral-dw/org-superstar-mode"
   :hook ((org-mode . org-superstar-mode)))

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
   (add-hook-fun org-present-mode-hook ()
     (org-present-big)
     (org-display-inline-images)
     (org-present-hide-cursor)
     (org-present-read-only))
   (add-hook-fun org-present-mode-quit-hook ()
     (org-present-small)
     (org-remove-inline-images)
     (org-present-show-cursor)
     (org-present-read-write)))

(x org-crypt
   :commands
   (org-encrypt-entry org-encrypt-entries org-decrypt-entry org-decrypt-entries)
   :config
   (setq org-tags-exclude-from-inheritance '("crypt")))


;;; Keybinds and Hydras

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'imdra-org/body)

(defhydra imdra-org (:body-pre
                     (require 'ox-publish)
                     :exit t :foreign-keys warn :hint nil)
  "
   ^
   ^Org^                 ^GTD^                 ^Task^                 ^Misc^
   ^───^─────────────────^───^─────────────────^────^─────────────────^────^────────────────────────
   _C_ Capture           _s_ Schedule          _.t_ TODO              _./_ Sparse-Tree
   ^ ^                   _d_ Deadline          _.,_ Priod             ^ ^
   _ls_ Store link       ^ ^                   _.z_ Add Note          _.a_ Attachment
   _li_ Stored Link      _ci_ in               _.q_ Set Tag           _xc_ Column View
   _lo_ Open Link        _co_ out              _xa_ :Archive:         _.$_ Archive Subtree
   ^ ^                   _cu_ cancel           ^ ^                    _.w_ _.W_ Refile/Copy
   _n_ Publish           _ce_ effort           _.._ Timestamp         ^ ^
   _N_ Publish<f>        _cj_ jump             _.!_ Invalidate Tim    _xp_ _xx_ insert Prop/DBlock
   _m_ Publish<i>        _cd_ display          _.y_ Eval Time Rang    _xv_ _xl_ _x\\_ ✓ image/link/entity
   _M_ Publish<if>       _cr_ report           ^ ^                    _.:_ _.;_ ✓ fixed-width/comment
   _r_ Reconf NoteDir    _cc_ Timer/Countdown  _o_ Agenda Files       _/_ Search Notes
   "
  ("a" org-agenda)
  ("o" im/org-agenda-files+)
  ("C" org-capture :exit t)
  ("cC" org-capture :exit t)

  ("n" im/note-publish :exit t)
  ("N" (im/note-publish t) :exit t)
  ("m" im/note-publish-interactively :exit t)
  ("M" (im/note-publish-interactively t) :exit t)
  ("r" im/note-publish-reset-directories :exit t)

  ("ls" org-store-link)
  ("li" org-insert-link)
  ("lo" org-open-at-point)

  ("s" org-schedule)
  ("d" org-deadline)

  ("ci" org-clock-in)
  ("co" org-clock-out)
  ("cu" org-clock-cancel)
  ("ce" org-clock-modify-effort-estimate)
  ("cj" org-clock-goto)
  ("cd" org-clock-display)
  ("cr" org-clock-report)
  ("cc" (progn (if (or org-timer-start-time org-timer-countdown-timer)
                   (defhydra imdra-org-timer ()
                     ("c" (message "") nil)
                     ("<SPC>" org-timer-pause-or-continue (if org-timer-pause-time "Continue" "Pause"))
                     ("i" org-timer)
                     ("I" org-timer-item)
                     ("l" (progn (call-interactively 'org-timer-item)
                                 (insert (read-string "Memo: "))))
                     ("r" (if org-timer-countdown-timer
                              (let (org-timer-default-timer)
                                (org-timer-set-timer '(16)))
                            (if (y-or-n-p "Reset the Timer ?")
                                (org-timer-start)
                              (message "Do Nothing."))) "Reset" :exit t)
                     ("k" (if (y-or-n-p "Stop it ?")
                              (org-timer-stop)
                            (message "Do Nothing."))
                          (format "Stop this %s" (propertize (if org-timer-countdown-timer "Countdown Timer" "Timer") 'face 'font-lock-constant-face))
                          :exit t))
                 (defhydra imdra-org-timer (:exit t :foreign-keys warn)
                   "Action"
                   ("t" org-timer-start "Create Timer")
                   ("d" org-timer-set-timer "Create Countdown")))
               (call-interactively 'imdra-org-timer/body)))

  (".." org-time-stamp)
  (".!" org-time-stamp-inactive)
  (".y" org-evaluate-time-range)

  (".t" org-todo)
  (".q" org-tag)
  (".," org-priority)
  ("./" org-sparse-tree)
  ("xa" org-toggle-archive-tag)
  ("/"  im/search-notes)

  (".w" org-refile)
  (".W" org-copy)
  (".$" org-archive-subtree)
  (".z" org-add-note)
  (".a" org-attach)
  ("xc" org-columns)

  ("xp" org-set-property)
  ("xx" org-dynamic-block-insert-dblock)

  (".:" org-toggle-fixed-width)
  (".;" org-toggle-comment)

  ("xv" org-toggle-inline-images)
  ("xl" org-toggle-link-display)
  ("x\\" org-toggle-pretty-entities))


(provide 'imoox)

;;; imoox.el ends here
