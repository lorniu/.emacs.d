;;; ioox-org.el --- Org-Mode -*- lexical-binding: t -*-

;; http://orgmode.org/manual/index.html

;;; Code:

(defun im-org-basic ()
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-hide-leading-stars nil)
  (setq org-hide-block-startup nil)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (setq org-cycle-separator-lines 0)
  (setq org-pretty-entities t)
  (setq org-list-allow-alphabetical t)

  (setq org-use-sub-superscripts '{})
  (setq org-export-with-sub-superscripts '{})
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

  (setq org-id-link-to-org-use-id t)
  (setq org-catch-invisible-edits 'show)
  (setq org-attach-store-link-p t)

  (setq org-html-html5-fancy t)
  (setq org-html-doctype "html5")
  (setq org-html-container-element "section")
  (setq org-html-validation-link "Go ahead, never stop.")
  (setq org-html-htmlize-output-type 'inline-css)
  (setq org-html-head-include-scripts nil)
  (setq org-publish-list-skipped-files nil)
  (setq org-publish-timestamp-directory (locc "org-publish-timestamp/"))

  (setq org-id-locations-file (locc ".org-id-locations")))

(defun im-org-mathjax ()
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

(defun im-org-src-block ()
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  ;; babel
  (cl-loop for l in '(shell
                      lisp python ruby haskell java js csx csharp
                      sql sqlite
                      gnuplot ditaa dot plantuml calc
                      restclient latex)
           unless (eq (elt (symbol-name l) 0) ?-) collect `(,l . t) into lst
           finally (org-babel-do-load-languages 'org-babel-load-languages lst))
  ;; lang-modes
  (cl-loop with setups = '(("xml" . sgml)
                           ("html" . mhtml)
                           ("cs" . csharp)
                           ("csx" . csharp)
                           ("tikz" . latex))
           for s in setups do (cl-pushnew s org-src-lang-modes))
  ;; fixup
  (defvar org-babel-default-header-args:cs '())
  (defun org-babel-execute:cs (body params) (org-babel-execute:csharp body params))
  ;; image
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  ;; region-execute!
  (defun:around org-babel-execute-src-block$region (f arg info &rest params)
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
              (setf (nth 1 info) (buffer-substring b e))
            (user-error "Region must inner src-block")))
        (funcall f arg info params)))))

(defun im-org-display ()
  (blackout 'org-indent-mode nil)

  (when IS-G
    (let ((f/font-face-family (f/get :font-mono)))
      (f/font-face 'org-table)
      (f/font-face 'org-column)))

  ;; (org-set-emph-re 'org-emphasis-regexp-components ...)

  (defun:hook org-mode-hook/keyword ()
    (font-lock-add-keywords
     nil '(("\\\\\\\\$" 0 'org-meta-line)
           ("\\<\\(FIXME\\|NOTE\\|AIA\\|TODO\\):" 1 'font-lock-warning-face prepend)))))

(defun im-org-links ()
  ;; Abbrev
  (setq org-link-abbrev-alist `(("google"      . "https://www.google.com/search?q=")
                                ("github"      . "https://github.com/")
                                ("notes"       . ,org-directory)
                                ("wolfram"     . "https://wolframalpha.com/input/?i=%s")))

  ;; Highlight broken file link
  (org-link-set-parameters "file"
                           :face (lambda (path)
                                   (if (or (file-remote-p path) (file-exists-p path))
                                       'org-link
                                     'font-lock-warning-face)))

  ;; Display full link in minibuffer when cursor/mouse is over it
  (defun:before-until org-eldoc-documentation-function$ (&rest _)
    (when-let (link (org-element-property :raw-link (org-element-context)))
      (format "Link: %s" link)))

  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs)))

(defun im-org-pubish ()
  ;; Remove zero-width chars when publishing
  (add-to-list 'org-export-filter-final-output-functions
               (defun my/org-publish-remove-zero-width-space (text _backend _info)
                 (unless (org-export-derived-backend-p 'org)
                   (replace-regexp-in-string "\u200b" "" text)))))

(x org
   :bind ((org-mode-map
           ("×"       . "*")
           ("C-c \""  . im/org-edit-special-with-wc-setup)
           ("C-,"     . nil)
           ("M-h"     . nil)
           ("C-x n"   . nil)
           ("C-c /"   . nil)))
   :commands
   (org-time-stamp org-time-stamp-inactive)
   :init
   (im-org-basic)
   :defer-config
   (require 'org-img)
   (require 'org-tempo) ; org-insert-structure-template
   (require 'org-special-block-extras)
   (im-org-mathjax)
   (im-org-src-block)
   (im-org-display)
   (im-org-links)
   (im-org-pubish))

(defun:hook org-mode-hook ()
  (setq-local system-time-locale "C")

  (electric-pair-local-mode 1)
  (org-special-block-extras-mode 1)

  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))


;;; Transient

(transient-define-prefix imtt/transient-org-mode ()
  [:hide
   (lambda () t)
   ("s"    ""   org-schedule)
   ("v T"  ""   (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'org-babel-tangle))))
   ("v f"  ""   org-babel-tangle-file)
   ("v C"  ""   org-babel-tangle-clean)
   ("v D"  ""   org-babel-detangle)
   ("'"    ""   (lambda () (interactive) (poly-org-mode)))
   ]
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
      (transient-setup 'imtt/transient-org-mode)
    (user-error "Sorry, but this is not org-mode")))


;;; Utils

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
  (let ((word (im-thing-at-region-or-point 'symbol)))
    (consult--read
     (let ((ret))
       (setq ret (mapcar (lambda (x) (format ":%s:" (car x))) org-tag-alist))
       (if word (push word ret))
       ret)
     :prompt "Tag or query (xxx /*): "
     :keymap (let ((map (make-composed-keymap nil vertico-map)))
               (define-key map [C-return]
                           (myc-make-action (cand input)
                             (setq input (car input))
                             (if (zerop (length input)) (setq input cand))
                             (when (string-match-p "^[^:]\\w+[^:]$" input)
                               (setq input (concat ":" input ":")))
                             (rg input "org" org-directory)
                             (pop-to-buffer (rg-buffer-name))))
               map)
     :initial word
     :history 'org-tags-history
     :lookup (lambda (_input _cs cand)
               (let ((ext "org")
                     (arr (split-string cand)))
                 (when (and (cl-plusp (length arr))
                            (string-prefix-p "/" (car (last arr))))
                   (setq ext (cl-subseq (car (last arr)) 1))
                   (setq cand (mapconcat #'identity (butlast arr) " ")))
                 (rg cand ext org-directory)
                 (pop-to-buffer (rg-buffer-name)))))))

(defun im/org-agenda-files+ ()
  (interactive)
  (consult--read
   (append (cl-loop for f in org-agenda-files
                    collect (propertize f 'type 'file))
           (cl-loop for n in org-agenda-notes
                    collect (propertize n 'type 'note)))
   :prompt "Agenda files: "
   :require-match t
   :predicate (lambda (cand)
                (let ((type (get-text-property 0 'type cand)))
                  (unless consult--narrow (setq consult--narrow ?f))
                  (equal type (if (= consult--narrow ?f) 'file 'note))))
   :narrow '((?n . "Notes") (?f . "Agenda"))
   :sort nil
   :lookup #'consult--lookup-location))

(defun im-org-in-drawer-p ()
  "If current point is in a drawer."
  (ignore-errors
    (cl-member (save-excursion
                 (org-up-element)
                 (car (org-element-at-point)))
               '(drawer property-drawer))))


;;; PlugIns

(x ob-dot
   :ref "http://graphviz.org/"
   :if (executable-find "dot") ;; choco install graphviz
   :defer-config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :ref "http://ditaa.sourceforge.net/"
   :if (executable-find "java")
   :defer-config (setq org-ditaa-jar-path (loce "share/ditaa.jar")))

(x ob-plantuml
   :ref "Homepage: https://plantuml.com/zh/"
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
   :defer-config
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
   :defer-config
   (setq org-tags-exclude-from-inheritance '("crypt")))

(x org-noter
   :ref "weirdNox/org-noter")

(provide 'ioox-org)

;;; ioox-org.el ends here
