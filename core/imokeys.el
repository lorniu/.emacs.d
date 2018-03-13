;;; iokeys.el --- Keybinds And Org-Mode
;;; Commentary:

;;; Code:

(bind-keys
 (  [f1]          . info                 )
 (  [f6]          . toggle-truncate-lines)
 (  [f7]          . (lambda () (interactive) (im/open-file-view "~/.emacs.d/core/immor.el")))
 (  [C-f7]        . (lambda () (interactive) (find-file "~/.cases/notes/misc.hunter.org")))
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



;;; Org-Mode - Configurations

;;; Basic Configurations

(defun im/org-basic-config()
  (setq
   org-startup-indented                  t
   org-hide-leading-stars                t
   org-hide-block-startup                t
   org-startup-with-inline-images        t
   org-cycle-separator-lines             0
   org-pretty-entities                   t
   org-src-fontify-natively              t
   org-src-tab-acts-natively             nil

   org-log-into-drawer                   t
   org-clock-into-drawer                 t
   org-clock-out-remove-zero-time-clocks t

   org-use-sub-superscripts              '{}
   org-export-with-sub-superscripts      '{}
   org-export-copy-to-kill-ring          nil
   org-publish-list-skipped-files        nil

   org-emphasis-regexp-components '("：，。！、  \t('\"{" "- ：，。！、 \t.,:!?;'\")}\\" " \t\r\n,\"'"  "."  1)

   org-html-html5-fancy                  t
   org-html-doctype                      "html5"
   org-html-container-element            "section"
   org-html-validation-link              "Go ahead, never stop."
   org-html-htmlize-output-type          'css
   org-html-head-include-scripts         nil
   org-html-head-include-default-style   nil
   org-html-head                         (im/html-viewport)))

;;; GTD Configurations

(defun im/org-gtd-config ()
  (setq
   org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   org-tag-alist            '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Dodo" . ?d))
   org-todo-keywords        '((sequence "TODO(t!)" "TING(i!)" "|" "DONE(d!)" "CANCEL(c!)"))
   org-refile-targets       `((org-agenda-files . (:level . 1)))

   org-default-task-file    (concat org-directory "misc.task.org")
   org-default-notes-file   (concat org-directory "misc.journal.org")
   org-agenda-files         (list org-default-task-file org-default-notes-file
                                  (concat org-directory "misc.hunter.org")))

  (setq org-capture-templates
        `(("t" "新任务" entry (file+headline ,org-default-task-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
          ("d" "Diary"  plain (file+datetree ,org-default-notes-file) "%U\n\n%i%?" :empty-lines 1)
          ("n" "草稿箱" entry (file ,(concat (file-name-directory  org-default-notes-file) "misc.scratch.org")) "* %U\n\n%i%?" :prepend t :empty-lines 1))))

;;; Babel Configurations

(defun im/org-babel-config ()

  (setq org-confirm-babel-evaluate nil
        org-babel-default-header-args (cons '(:noweb . "no") (assq-delete-all :noweb org-babel-default-header-args)))

  (cl-flet ((-lbs (langs) (org-babel-do-load-languages 'org-babel-load-languages (mapcar (lambda (x) (cons x t)) langs))))
    (-lbs '( emacs-lisp
             sh
             lisp
             haskell
             python
             ruby
             java
             restclient
             sql
             ditaa
             dot
             plantuml ))))

;;; Publishment Configurations

(defun im/org-publishment-config ()
  (setq org-publish-timestamp-directory(concat _CACHE_ ".org-timestamps/")
        org-publish-project-alist
        `(("notes"
           :components           ("resources" "org-files"))

          ("org-files"
           :base-directory       ,org-directory
           :publishing-directory ,(concat org-directory base/out)
           :headline-levels      3
           :with-toc             3
           :html-preamble        nil
           :auto-sitemap         t
           :sitemap-filename     "index.org"
           :sitemap-title        ,notes-title
           :html-link-up         "index.html"
           :html-link-home       "index.html"
           :publishing-function  org-html-publish-to-html
           :recursive            t
           :html-head           ,(concat org-html-head
                                         (if css-inline
                                             (format "<style>\n%s\n</style>\n<script>\n%s\n</script>\n"
                                                     (im/read-file-content (concat notes-home html/css))
                                                     (im/read-file-content (concat notes-home html/js)))
                                           (format "<link rel=\"stylesheet\" href=\"%s\">\n<script src=\"%s\"></script>\n"
                                                   html/css html/js))))

          ("resources"
           :base-directory       ,(concat org-directory "assets/")
           :publishing-directory ,(concat org-directory base/out/res)
           :base-extension       "css\\|js\\|png\\|jpe?g\\|gif\\|svg"
           :publishing-function  org-publish-attachment
           :recursive            t)))

  (defun im/org-publish-note (&optional force)
    (interactive)
    (require 'ox-publish)
    (if force (ignore-errors (delete-directory org-publish-timestamp-directory t)))
    (save-excursion (org-publish "notes"))
    (message "Publish Finished!"))

  (defun im/org-publish-note-force ()
    (interactive)
    (im/org-publish-note 'force)))

;;; Initialization

(defun im/org-initial (notes-home &optional notes-title css-inline)
  (interactive (list (read-directory-name "选择笔记目录: ") (read-from-minibuffer "输入笔记标题: ")))
  (let* ((html/css (concat "assets/base.css"))
         (html/js  (concat "assets/base.js"))
         (base/out "html/")
         (base/out/res (concat base/out "assets/")))
    (setq org-directory notes-home)
    (im/org-basic-config)
    (im/org-gtd-config)
    (im/org-babel-config)
    (im/org-publishment-config)
    (message "%s@%s/%s" notes-home notes-title css-inline)))



;;; Org-Mode - Major-Mode

(x org/w
   :bind
   (( "C-c a"   . org-agenda )
    ( "C-c l"   . org-store-link )
    ( "C-c c"   . org-capture )
    ( "C-c n"   . im/org-publish-note )
    ( "C-c C-n" . im/org-publish-note-force )
    :map org-mode-map ( "C-x a a" . im/org-wrap-src))

   :config

   ;; Configurations
   (cond
    ((env-classroom) (im/org-initial "e:/share/notes/" "Index"))
    (t (im/org-initial "~/.cases/notes/" "imfineandu")))

   ;; Hooks
   (add-hook-lambda 'org-mode-hook
     (diminish 'org-indent-mode)
     (set (make-local-variable 'system-time-locale) "C")
     (font-lock-add-keywords nil '(("\\\\\\\\$" 0 'hi-org-break)
                                   ("\\<\\(FIXME\\|NOTE\\|AIA\\):" 1 'font-lock-warning-face prepend))))

   ;; Inline Image
   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

   ;; Faces
   (defface hi-org-break
     `((t (:foreground ,(pcase system-type ('gnu/linux "#222222") ('windows-nt "#eeeeee"))))) "for org mode \\ break" :group 'org-faces)
   (env-windows (set-face-attribute 'org-table nil :font "fontset-table" :fontset "fontset-table"))

   ;; Remap keys
   (define-key org-mode-map (kbd "×") (kbd "*"))
   (define-key org-mode-map (kbd "－") (kbd "-"))

   ;; other functions
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
         (forward-line -2)))))



;;; Org-Mode - Download

(x org-download
   :after '(org)
   :config
   (setq org-download-image-dir (concat org-my-base-res "image")
         org-download-backend (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t))

   (advice-add 'org-download--dir-2 :around
               (lambda (_ &rest __) ;; Where to save?
                 (or (file-name-base (buffer-file-name)) "")))

   (defun org-download-rename-file-at-point ()
     "Rename the downloaded file at point."
     (interactive)
     (let* ((pattern "^file:\\(.+\\)$")
            (atpoint (ffap-url-at-point))
            (origin (if (and atpoint (string-match pattern atpoint))
                        (match-string 1 atpoint)))
            (from (expand-file-name (or origin ""))))
       (if (and origin (file-exists-p from) (eq major-mode 'org-mode))
           (let ((to (format "%s/%s.%s"
                             (org-download--dir)
                             (read-string "Rename file to: " (file-name-base from))
                             (file-name-extension from))))
             (rename-file from to 1)
             (org-download-replace-all (file-name-nondirectory from)
                                       (file-name-nondirectory to))
             (org-display-inline-images))
         (message "Not available")))))



;;; Org-Mode - Drawing And So On.

(x ob-dot
   :if (executable-find "dot") ;; choco install graphviz
   :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :if (executable-find "java")
   :config (setq org-ditaa-jar-path "~/.emacs.d/ext/ditaa.jar"))

(x ob-plantuml
   :if (and (executable-find "java") (executable-find "dot"))
   :config
   (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
   (when (and (null (file-exists-p org-plantuml-jar-path))
              (yes-or-no-p "Download plantuml.jar Now?"))
     (url-copy-file "https://versaweb.dl.sourceforge.net/project/plantuml/1.2018.1/plantuml.1.2018.1.jar" org-plantuml-jar-path)))


(provide 'imokeys)

;;; imokeys.el ends here
