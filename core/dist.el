;;; -*- lexical-binding: t -*-

;; Set special packages demo:
;;
;;   (with-eval-after-load 'dist
;;     (p/special
;;      (sly . "~/source/sly")
;;      (go-translate . "~/source/go-translate")
;;      (company . nil)
;;    ))

;; Logic of package.el:
;;
;;   1. find [package-user-dir] and [package-directory-list], add to [package-alist]
;;   2. load [package-archives] to [package-archive-contents] as all packages in elpa
;;   3. according [package-load-list], load pkg, refresh [load-path] and [package-activated-list]

;;; Code:

(setq package-user-dir (loce "elpa"))

(package-initialize)

(add-hook 'package-menu-mode-hook (lambda () (hl-line-mode 1)))


;;; Install packages

(let ((ps '(diminish ; lighter
            standard-themes nano-theme gruvbox-theme srcery-theme ; themes
            gptel ; ai
            corfu cape vertico orderless consult marginalia hyperbole embark embark-consult ; basic
            yasnippet license-templates ; templates
            vundo emms wgrep trashed
            eat xterm-color ; term
            evil ; viper -> evil-local-mode
            magit git-timemachine ssh-agency git-modes ghub forge gitignore-templates ; git
            gcmh
            bbdb
            rg ; ripgrep
            hide-lines ; like occur
            ztree ; dir diff
            dired-dups ; dir dups
            engine-mode
            aes ; encrypt
            vlf ; view large file
            pyim pyim-basedict rime sis ; ime
            keycast ; show key-pressed
            kubernetes docker dockerfile-mode ; container
            uuidgen
            htmlize
            cal-china-x ; calc
            posframe ; childframe
            pcre2el ; regexp
            treemacs ; workspace
            disk-usage memory-usage
            ace-window
            page-break-lines
            erc-hl-nicks
            all-the-icons ; icons

            ;; org

            org-contrib org-make-toc
            org-reverse-datetree ; used in org-capture for journal
            org-present ; simple presentation
            org-roam org-roam-ui
            ox-pandoc
            pdf-tools org-noter org-noter-pdftools nov ; read & note

            ;; drawing/export

            gnuplot
            graphviz-dot-mode
            plantuml-mode
            auctex ; latex
            cowsay ; figlet ; ascii art

            ;; http
            plz simple-httpd
            websocket know-your-http-well
            restclient ob-restclient httprepl ; rest

            ;; development

            dape ; debug
            nhexl-mode ; binary
            macrostep rainbow-mode rainbow-delimiters package-lint ; elisp
            c-eldoc cmake-mode citre ; c/c++
            emmet-mode web-beautify sass-mode web-mode ; html
            typescript-mode ob-typescript ; typescript
            rust-mode ; rust
            php-mode ; php
            go-mode ; go
            robe ; ruby
            erlang ; erlang
            alchemist ; elixir
            lua-mode ; lua
            haskell-mode hindent attrap ; haskell
            csproj-mode fsharp-mode sharper ob-fsharp ; dotnet
            kotlin-mode clojure-mode groovy-mode scala-mode ; jvm
            jdecomp ; java decompile, use idea's fernflower.jar
            android-mode ; easy to run android tools
            markdown-mode markdown-toc ; markdown
            sql-indent ; database
            powershell ob-powershell
            yaml-mode csv-mode systemd udev-mode
            polymode edit-indirect)))
  (cl-macrolet ((ensure-packages-installed ()
                  `(cl-labels
                       ((all ()
                          (cl-loop for o in ps
                                   for p = (if (listp o) (if (eval (cadr o)) (car o)) o)
                                   if (and p (not (package-disabled-p p nil))) collect p))
                        (lacks ()
                          (cl-remove-if #'package-installed-p (all)))
                        (report (lacks)
                          (display-warning
                           'packages
                           (format
                            "Total %d packages missing:\n\n%s\n\nPlease install first:\n%s\n\n" (length lacks)
                            (with-temp-buffer (insert (format "%s" lacks)) (fill-region (point-min) (point-max)) (buffer-string))
                            "\n(progn\n  (call-interactively #'p/repo)\n  (call-interactively #'im/proxy)\n  (setq package-check-signature nil)\n  (p/install)\n  (restart-emacs)\n )")
                           :warning "*packages-lack-warning*")))
                     (defun p/install (&optional dont-select)
                       (interactive "P")
                       (package-refresh-contents)
                       (dolist (p (lacks)) (package-install p dont-select))
                       (message "Install Finished."))
                     (if-let* ((lst (lacks))) (report lst)))))
    (ensure-packages-installed)))


;;; Package Repo

(defconst p/elpa-repos
  '(:origin
    (setq package-archives
          `(("gnu"    . "https://elpa.gnu.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("melpa"  . "https://melpa.org/packages/")))
    :ustc
    (setq package-archives
          `(("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
            ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
            ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    :tsinghua
    (setq package-archives
          `(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu")
            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu")
            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa")))))

(defvar ic/elpa :origin)

(with-eval-after-load 'dist (p/repo ic/elpa)) ; init package-archives


;;; Use-Package

(setq use-package-always-defer t
      use-package-expand-minimally t)

(defmacro x (NAME &rest args)
  "Flags: e/demand d/diminish i/ensure x/disabled."
  (pcase-let* ((`(,name ,flags) (split-string (symbol-name NAME) "/"))
               (mode-name (replace-regexp-in-string "-mode" "" name))
               (doc-strings (cl-loop for i in args until (keywordp i) collect i))
               (options (cl-set-difference args doc-strings))
               (refs (prog1 (plist-get options :ref) (cl-remf options :ref)))
               (refs-name (if (and (cdr-safe refs) (symbolp (car-safe refs))) (pop refs) (intern mode-name)))
               (fopts (delq nil (list (if (seq-contains-p flags ?e) :demand :defer) t
                                      (if (seq-contains-p flags ?d) :diminish)
                                      (if (seq-contains-p flags ?i) :ensure)
                                      (if (seq-contains-p flags ?x) :disabled))))
               (name (intern name)))
    (delq nil
          `(progn
             (if init-file-debug (message "Loading %s..." ',name))
             ,(if doc-strings `(defhelper ,(intern mode-name) ,@doc-strings))
             ,(if refs `(defreference ,refs-name ,refs))
             (use-package ,name ,@fopts ,@options)))))


;;; Auxiliaries

(defmacro p/special (&rest pkg-list)
  "Specify package path / pin package version / disable specific package:
   : (p/special (diminish . 'path') (simple-httpd . (1 5 1)) (company . nil))"
  (macroexp-progn
   (cl-loop for (p . v) in pkg-list
            if (consp v) collect `(add-to-list 'package--builtin-versions '(,p ,@v)) ; pretend installed
            else if (stringp v) collect `(push ,v load-path) ; use this instead of elpa's one
            else if (null v) collect `(add-to-list 'package-load-list '(,p . nil)) ; don't load this
            else collect `',p)))

(defun p/repo (&optional upstream)
  (interactive (list (intern (completing-read "package-archives to use: " (cl-loop for m in p/elpa-repos by #'cddr collect (symbol-name m)) nil t))))
  (unless upstream (setq upstream (car p/elpa-repos)))
  (eval (plist-get p/elpa-repos upstream))
  (if (called-interactively-p 'any) (message "%s" package-archives) (message "[ELPA] %s" upstream)))

(defun p/clean-dups (&optional dry-run)
  (interactive "P")
  (let* ((dirs (directory-files package-user-dir nil "-"))
         (packages (mapcar (lambda (d)
                             (string-match "\\(.*\\)\\(-[0-9].*\\)" d)
                             (cons (match-string 1 d)
                                   (cl-subseq (match-string 2 d) 1)))
                           (cl-remove-if (lambda (d) (string-match-p "signed$" d)) dirs)))
         (packages-dup
          (cl-loop for p in packages
                   if (> (length (cl-remove-if-not (lambda (d) (string= (car p) (car d))) packages)) 1)
                   collect p))
         (packages-preserved
          (cl-remove-duplicates packages-dup :test 'string-equal :key 'car))
         (packages-should-removed
          (cl-set-difference packages-dup packages-preserved)))
    (if packages-should-removed
        (cl-loop for p in packages-should-removed
                 for d = (expand-file-name (concat (car p) "-" (cdr p)) package-user-dir)
                 do (progn
                      (message "Deleting %s %s" d (if dry-run "[dry-run]" ""))
                      (unless dry-run (delete-directory d t))))
      (message "Nothing to clean."))))

(defun p/add-to-load-path (path)
  (interactive (list (read-directory-name "Directory to add: " nil nil t)))
  (if (and path (file-directory-p path))
      (add-to-list 'load-path path)
    (user-error "Path '%s' is not available" path)))

(provide 'dist)
