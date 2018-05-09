;;; iokeys.el --- Keybinds and Org-Mode
;;; Commentary:

;;; Code:

(bind-keys
 (  [f1]          . info                 )
 (  [f6]          . toggle-truncate-lines)
 (  [f7]          . (lambda () (interactive) (im/open-file-view "~/.emacs.d/core/immor.el")))
 (  [C-f7]        . (lambda () (interactive) (find-file "~/.notes/000/")))
 (  [f8]          . calendar             )
 (  [f10]         . shrink-window        )
 (  [f11]         . enlarge-window       )
 (  [f12]         . im/toggle-fullscreen )
 (  "<insertchar>". undo                 )
 (  "<select>"    . im/toggle-dedicated  )
 (  "%"           . his-match-paren      )
 (  "C-c C-j"     . ffap                 )
 (  "M-q"         . im/tiny-code         )
 (  "M-w"         . im/copy-current-line )
 (  "C-h t"       . im/trans-word        )
 (  "ESC <down>"  . im/copy-lines        )
 (  "ESC <up>"    . (lambda () (interactive) (im/copy-lines 1)))
 (  "ESC <right>" . im/kill-lines        )
 (  "C-x p"       . other-frame          ))


;;; Org-Mode
;; http://orgmode.org/manual/index.html

(defvar --im/org-pub-home "~/.notes/")

(defun -/ (path) (concat org-directory path))

(defun -im/html-mk-css (&rest styles)
  (format "<style>\n%s\n</style>" (padding-left-to-string styles)))

(defun -im/html-mk-js (script)
  (concat "<script>\n%s\n</script>" (padding-left-to-string script)))

(defun im/org-wrap-src ()
  (interactive)
  (let ((beg "#+BEGIN_SRC") (end "#+END_SRC") (lang (read-from-minibuffer "Please input your type: ")))
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

(defun -im/face-color (type)
  (if (eq type 'bg)
      (let ((bg (face-background 'default)))
        (if (and (env-linux-ng) (string= "unspecified-bg" bg)) "#333" bg))
    (let ((fg (face-foreground 'default)))
      (if (and (env-linux-ng) (string= "unspecified-fg" fg)) "#eee" fg))))

(defun im/org-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))


;;; Org-Mode - Configurations

(defun im/org-configurations ()

  (interactive)

;;; Basic

  (setq org-startup-indented                  t
        org-hide-leading-stars                t
        org-hide-block-startup                t
        org-startup-with-inline-images        t
        org-image-actual-width                nil
        org-cycle-separator-lines             0
        org-pretty-entities                   t
        org-src-fontify-natively              t
        org-src-tab-acts-natively             nil
        org-url-hexify-p                      nil

        org-log-into-drawer                   t
        org-clock-into-drawer                 t
        org-clock-out-remove-zero-time-clocks t

        org-blank-before-new-entry            '((heading . t) (plain-list-item . nil))
        org-use-sub-superscripts              '{}
        org-export-with-sub-superscripts      '{}
        org-export-copy-to-kill-ring          nil
        org-publish-list-skipped-files        nil

        org-emphasis-regexp-components '("：，。！、  \t('\"{" "- ：，。！、 \t.,:!?;'\")}\\" " \t\r\n,\"'"  "."  1)

        org-html-html5-fancy                  t
        org-html-doctype                      "html5"
        org-html-container-element            "section"
        org-html-validation-link              "Go ahead, never stop."
        org-html-htmlize-output-type          'inline-css
        org-html-head-include-scripts         nil
        org-html-head    (string-join-newline (-im/html-mk-css
                                               "body { padding:10px; font-size:13px; }"
                                               "a { text-decoration:none; }"
                                               ".org-src-container { position:relative }"
                                               (format "pre.src { position:static; overflow:auto; background: %s; color: %s; } "  (-im/face-color 'bg) (-im/face-color 'fg))
                                               (format "pre.src:before { color: %s; } " (-im/face-color 'bg))
                                               "blockquote { border-left: 3px solid #999; padding-left: 10px; margin-left: 10px; }"
                                               )))

;;; GTD

  (setq org-default-task-file     (-/ "000/task.org")
        org-default-notes-file    (-/ "000/journal.org")

        org-agenda-files          (file-expand-wildcards (-/ "000/*.org"))

        org-time-clocksum-format  '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        org-tag-alist             '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Dodo" . ?d))
        org-todo-keywords         '((sequence "TODO(t!)" "TING(i!)" "|" "DONE(d!)" "CANCEL(c!)"))
        org-refile-targets        `((org-agenda-files . (:level . 1)))

        org-capture-templates     `(("t" "新任务" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
                                    ("d" "Diary"  plain (file+datetree ,org-default-notes-file) "%U\n\n%i%?" :empty-lines 1)
                                    ("n" "草稿箱" entry (file ,(-/ "000/scratch.org")) "* %U\n\n%i%?" :prepend t :empty-lines 1))))

;;; Babel

(defun im/org-load-babel ()
  (setq org-confirm-babel-evaluate    nil)
  (setq org-babel-default-header-args (cons '(:noweb . "no") (assq-delete-all :noweb org-babel-default-header-args)))

  ;; (add-to-list 'org-src-lang-modes '("html" . web))

  (cl-flet ((-lbs (langs) (org-babel-do-load-languages 'org-babel-load-languages (mapcar (lambda (x) (cons x t)) langs))))
    (-lbs '( emacs-lisp
             sh shell
             lisp
             haskell
             python
             ruby
             java
             restclient  ;; GET :s/hello.json
             sql
             gnuplot
             ditaa
             dot
             plantuml ))))


;;; Org-Mode - Publishments

(defun im/org-publishments ()
  (let ((--im/org-pub-dest (concat --im/org-pub-home "__html__")))
    (setq
     org-publish-project-alist
     `(("org"
        :base-directory       ,--im/org-pub-home
        :publishing-directory ,--im/org-pub-dest
        :base-extension       "org"
        :exclude              "^\\..*"
        :publishing-function  org-html-publish-to-html
        :recursive            t
        :headline-levels      3
        :with-toc             3
        :html-preamble        t
        :auto-sitemap         t)

       ("res"
        :base-directory       ,--im/org-pub-home
        :publishing-directory ,--im/org-pub-dest
        :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg\\|pdf\\|zip"
        :exclude              "^\\(\\.\\|_\\).*"
        :publishing-function  org-publish-attachment
        :recursive            t)

       ("nnn" :components ("org" "res")))) ))

(defun im/org-publish-note (&optional force)
  (interactive)
  (let ((start (current-time)))
    (with-temp-buffer
      (find-file (concat --im/org-pub-home " processing.org"))
      (let ((org-startup-folded 'showeverything)
            (vc-handled-backends nil))
        (org-publish "nnn" force))
      (kill-buffer))
    (message "Publish Finished in %.2f seconds!" (time-subtract-seconds (current-time) start))))

(defun im/org-publish-note-force () (interactive) (im/org-publish-note 'force))


;;; Org-Mode - Initialization

(defun im/initial-org (&optional home)
  (interactive (list (read-directory-name "选择笔记目录: ")))
  (setq --im/org-pub-home  home)
  (unless (called-interactively-p t) (setq org-directory home))
  (im/org-configurations)
  (im/org-publishments)
  (message "Publishment Home: %s" --im/org-pub-home))

(cond
 ((env-classroom) (im/initial-org "e:/share/notes/"))
 (t (im/initial-org "~/.notes/")))

;; change color for html export
(advice-add 'load-theme :after (lambda (&rest _) (im/org-configurations)))
(advice-add 'disable-theme :after (lambda (&rest _) (im/org-configurations)))


;;; Org-Mode - Major-Mode

(x org/w :commands (org-publish)
   :bind*
   (( "C-c a"   . org-agenda )
    ( "C-c l"   . org-store-link )
    ( "C-c c"   . org-capture )
    ( "C-c n"   . im/org-publish-note )
    ( "C-c C-n" . im/org-publish-note-force )
    ( :map org-mode-map ( "C-x a a" . im/org-wrap-src )))

   :config

   ;; Hooks
   (add-hook-lambda 'org-mode-hook
     (diminish 'org-indent-mode)
     (set (make-local-variable 'system-time-locale) "C")
     (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'hi-org-break)
                                   ("\\<\\(FIXME\\|NOTE\\|AIA\\):" 1 'font-lock-warning-face prepend))))

   ;; Faces
   (defface hi-org-break `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee"))))) "for org mode \\ break" :group 'org-faces)

   ;; Font for Org-Table
   (env-graphic (set-face-attribute 'org-table nil :font "fontset-table" :fontset "fontset-table"))

   ;; Remap keys
   (define-key org-mode-map (kbd "×") (kbd "*"))
   (define-key org-mode-map (kbd "－") (kbd "-"))

   ;; Babel
   (im/org-load-babel)
   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

   ;; Plugins
   (require 'ox-impress)
   ;; (require 'ox-freemind)
   (require 'org-download))

;; Remove extra blank line for example block!
(advice-add 'org-element-fixed-width-parser
            :filter-return
            (lambda (ret) (list 'fixed-width (plist-put (cadr ret) :value (string-trim (plist-get (cadr ret) :value))))))


;;; Org-Mode - Drawing, Download, etc.

(x ob-dot
   :if (executable-find "dot") ;; choco install graphviz
   :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :if (executable-find "java")
   :config (setq org-ditaa-jar-path "~/.emacs.d/ass/ditaa.jar"))

(x ob-plantuml
   :if (and (executable-find "java") (executable-find "dot"))
   :config
   (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
   (when (and (null (file-exists-p org-plantuml-jar-path))
              (yes-or-no-p "Download plantuml.jar Now?"))
     ;; IF NOT WORK, RUN THIS IN SHELL:
     ;; wget https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar -O ~/.emacs.d/plantuml.jar
     (url-copy-file "https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar" org-plantuml-jar-path)))

(x gnuplot ;; I can't make it work on Windows yet.
   :if (executable-find "gnuplot")
   :init (setq gnuplot-program "gnuplot"))

(x org-download
   :config
   (setq org-download-backend (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t))
   (advice-add 'org-download--dir-2 :around (lambda (_ &rest __) "img")))


(provide 'imokeys)

;;; imokeys.el ends here
