;;; immor.el --- Modules Configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Download Emacs Mirror:
;; http://mirrors.ustc.edu.cn/gnu/emacs/windows/

;; Change Log:

;; - remove `Origami', I don't like it. 20180111
;; - remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; - remove `iedit', as `auto-highlight-symbol' is amazing. Hack it and enjoy it! 2018-07-03

;;; Code:

;;; Basic Config

(setq desktop-path `(,_CACHE_ "."))
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(setq syntax-subword-skip-spaces t)

(savehist-mode 1)
(winner-mode 1)
(show-paren-mode 1)
(size-indication-mode 1)

;; Hippie-Expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-file-name-partially try-complete-file-name
        try-expand-all-abbrevs try-expand-list try-expand-line
        try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; Auto-Mode-Alist
(mapc (lambda (s) (add-to-list 'auto-mode-alist s))
      '(("\\.\\(xml\\|xsl\\)\\'" . sgml-mode)
        ("\\.class\\'"           . class-mode)
        ("\\.scm\\'"             . scheme-mode)
        ("\\.\\(ba\\)?sh\\'"     . sh-mode)))

;; Display-Buffer-Alist

(setq even-window-heights nil)

(defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
  (display-buffer-at-bottom buffer alist)
  (select-window (get-buffer-window buffer))
  (with-current-buffer buffer (view-mode 1)))

(setq display-buffer-alist
      `(("\\*Compile-Log\\*"            ; regexp to filter buffer-name
         (display-buffer-reuse-window)  ; functions to deal with display
         (window-height . 0.3)          ; parameters to pass to functions above
         (window-width . 0.3))

        ("\\*Youdao Dictionary\\*\\|\\*Help\\*\\|\\*Messages\\*\\|\\*Shell Command Output\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (reusable-frames . t))

        ("\\*[cC]ompilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom))

        ("\\*\\([-+]?e?shell[-+]?\\|PowerShell\\|Python\\)\\*"
         (display-buffer-same-window)
         (reusable-frames . t))

        ("\\*Async Shell Command\\*"
         (%display-buffer-at-bottom-follows-with-quit)
         (window-height . 0.3))

        ("." nil (reusable-frames . t))))

;; Help Window

(defun my-bury-window-after-push-button (f &rest args)
  (let ((prev (current-buffer))
        (prev-major-mode major-mode))
    (apply f args)
    (when (memq prev-major-mode '(help-mode))
      (delete-window (get-buffer-window prev)))))

(advice-add 'push-button :around 'my-bury-window-after-push-button)


;;; Basic Hooks

(defun my-before-open ()
  ;; files should be readonly
  (and (not (string-match-p "/usr/home\\|.cache/\\|temp/\\|tmp/\\|vvv/" buffer-file-name))
       (not (string-match-p "\\(autoloads\\|loaddefs\\).el$" buffer-file-name))
       (let ((case-fold-search nil))
         (string-match-p "^/usr/\\|.emacs.d/packages\\|/emacs/\\|.roswell/lisp/quicklisp" buffer-file-name))
       (view-mode 1)))

(defun my-before-save ()
  ;; auto remove trailing whitespace
  (unless (seq-contains '(org-mode) major-mode)
    (delete-trailing-whitespace)))

(defun my-after-save ()
  (save-window-excursion
    (when (and (eq major-mode 'emacs-lisp-mode)
               (string-match-p "^[a-z]" (buffer-name))
               (string-match-p "\\/\\.emacs\\.d\\/\\(core\\|extra\\)\\/" (buffer-file-name)))
      (byte-compile-file (buffer-file-name)))))

(add-hook 'find-file-hook 'my-before-open)
(add-hook 'before-save-hook 'my-before-save)
(add-hook 'after-save-hook 'my-after-save)


;;; Sudo/Tramp/Eshell

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "Edit as ROOT"
  :group 'imfine)

(defun my-find-file-root-header-warning ()
  (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
    (setq header-line-format (propertize " Edit as ROOT! " 'face 'find-file-root-header-face))))

(add-hook 'find-file-hook 'my-find-file-root-header-warning)
(add-hook 'dired-mode-hook 'my-find-file-root-header-warning)

(defun su ()
  "Edit file as Super User."
  (interactive)
  (let ((pt (point)) (old-buf (current-buffer))
        (buf-name (expand-file-name (or buffer-file-name default-directory))))
    (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
    (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
      (find-file buf-name))
    (kill-buffer-if-not-modified old-buf)
    (goto-char pt)))

(x tramp/w
   :init
   (setq tramp-verbose 1)
   (setq remote-file-name-inhibit-cache nil)
   (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

   :config
   ;; fix a bug on FreeBSD: echo `uname -r` does not return a valid Lisp expression...
   (env-bsd (require 'tramp-sh)
            (advice-add 'tramp-send-string :before
                        (lambda (_ str)
                          (when (string-match-p "`uname -sr`" str)
                            (sit-for 1))))))

(x term
   "To work on Windows better, install cygwin/msys2, and activate fakecygpty."
   :init
   (when (and (env-windows) (executable-find "bash"))
     ;; Cygwin/Msys2 pty support.
     ;; If `Spawning child process: Exec format error` occurred, recompile or change permissions.
     (add-to-list 'exec-path (expand-file-name "~/.emacs.d/extra/fakecygpty/"))
     (require 'fakecygpty)
     ;; (add-to-list 'fakecygpty-ignored-program-regexps "httpd")
     (fakecygpty-activate))
   :config
   (defun my-term-send-forward-word () (interactive) (term-send-raw-string "\ef"))
   (defun my-term-send-backward-word () (interactive) (term-send-raw-string "\eb"))
   (define-key term-raw-map [C-left] 'my-term-send-backward-word)
   (define-key term-raw-map [C-right] 'my-term-send-forward-word)

   (defun my-term-mode-hook () (goto-address-mode 1))
   (add-hook 'term-mode-hook #'my-term-mode-hook))

(x shell
   :init
   (when (and (env-windows) (executable-find "bash"))
     ;; use bash.exe as default process if possible
     (setenv "PS1" "\\u@\\h \\w $ ")
     (setq explicit-shell-file-name (executable-find "bash"))
     (setq explicit-bash.exe-args '("--login" "-i")))

   (add-hook-lambda 'shell-mode-hook
     (when (and (env-windows) ;; cmd/powershell on windows, should be gbk encoding
                (not (string-match-p "bash" (or explicit-shell-file-name ""))))
       (im/local-encoding 'cp936))
     (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(x eshell/w
   :config
   (setq eshell-history-size 200
         eshell-hist-ignoredups t
         eshell-destroy-buffer-when-process-dies t
         eshell-directory-name (concat _CACHE_ "eshell")
         eshell-aliases-file "~/.emacs.d/resource/eshell.alias"
         comint-scroll-show-maximum-output nil
         eshell-scroll-show-maximum-output nil)

   ;; visual-commands
   (with-eval-after-load 'em-term
     (patch/eshell) ; force less pager
     (setq eshell-visual-subcommands
           '(("git" "log" "diff" "show")
             ("sudo" "vi" "visudo")
             ("sbt")))
     (setq eshell-visual-options
           '(("git" "--amend")))
     (add-to-list 'eshell-visual-commands "vim"))

   ;; 256-color
   (setenv "TERM" "xterm-256color")
   (add-hook-lambda 'eshell-before-prompt-hook
     (setq xterm-color-preserve-properties t)
     (setq eshell-preoutput-filter-functions '(xterm-color-filter))
     (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

   ;; mode hook
   (add-hook-lambda 'eshell-mode-hook
     (define-key eshell-mode-map (kbd "C-r") 'eshell/h))

   (defun eshell/! (&rest args)
     "Combination eshell and native-shell-command !"
     (let ((cmd (eshell-flatten-and-stringify args)))
       (shell-command-to-string cmd)))

   (defun eshell/h ()
     (interactive)
     (require 'em-hist)
     (let* ((start-pos (save-excursion (eshell-bol) (point)))
            (end-pos (point))
            (input (buffer-substring-no-properties start-pos end-pos))
            (command (ivy-read "Command: "
                               (delete-dups
                                (when (> (ring-size eshell-history-ring) 0)
                                  (ring-elements eshell-history-ring)))
                               :initial-input input)))
       (setf (buffer-substring start-pos end-pos) command)
       (end-of-line)))

   (defun eshell/ssh (&rest args)
     "Secure shell."
     (let ((cmd (eshell-flatten-and-stringify (cons "ssh" args)))
           (display-type (framep (selected-frame))))
       (cond
        ((and (eq display-type 't) (getenv "STY"))
         (send-string-to-terminal (format "\033]83;screen %s\007" cmd)))
        (t (apply 'eshell-exec-visual (cons "ssh" args)))))))


;;; Generic Modes

(setq auto-revert-mode-text "")
(global-auto-revert-mode 1)

(setq recentf-max-saved-items 40)
(setq recentf-save-file (concat _CACHE_ "_recentf"))
(recentf-mode 1)

(add-hook 'edebug-mode-hook (lambda () (view-mode -1)))

(x beacon/d
   :init (beacon-mode 1))

(x anzu/d
   :init (global-anzu-mode))

(x isearch
   :config
   (bind-key "\C-e" ; Superword-Mode when search
             (lambda ()
               (interactive)
               (superword-mode 1)
               (isearch-yank-word-or-char)
               (superword-mode -1)) isearch-mode-map)
   (bind-key "M-s o" ; Word under cursor first
             (lambda (str)
               (interactive (list (aif (symbol-at-point) (symbol-name it))))
               (occur (read-from-minibuffer "Occurs: " str nil nil 'regexp-history))))
   (add-function :after (symbol-function 'occur) ; After Ocur, jump to the result buffer.
                 (lambda (&rest _) (aif (get-buffer-window "*Occur*") (select-window it)))))

(x replace
   :config
   (define-key occur-mode-map "r" 'occur-edit-mode))

(x page-break-lines
   :init
   (setq page-break-lines-lighter "")
   (nconc page-break-lines-modes '(web-mode css-mode))
   (global-page-break-lines-mode 1))

(x ediff
   :init
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq-default ediff-highlight-all-diffs 'nil)
   (setq ediff-diff-options "-w"))

(x abbrev/d
   :config
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file)))

(x ibuffer/e
   :config
   (setq ibuffer-show-empty-filter-groups nil
         ibuffer-saved-filter-groups
         `(("default"
            ("apple"
             (or (predicate . (eq (get major-mode 'derived-mode-parent) 'prog-mode))
                 (predicate . (member major-mode '(nxml-mode sgml-mode)))))
            ("banana"
             (or (mode . org-mode)
                 (mode . tar-mode)))
            ("orange"
             (or (mode . erc-mode)
                 (mode . rcirc-mode)))
            ("melon"
             (or (name . "^\\*?magit")))
            ("peach"
             (or (name . "\\*.+\\*")))
            ("durin"
             (or (mode . dired-mode))))))

   (add-hook-lambda 'ibuffer-mode-hook
     (ibuffer-switch-to-saved-filter-groups "default")))

(x wdired/e
   :bind
   (:map dired-mode-map
         ( "6" . dired-up-directory )
         ( "r" . wdired-change-to-wdired-mode )
         ( "." . imdra-dired/body )
         ( "s" . hydra-dired-quick-sort/body )
         ( "z" . idp/dired-du-size )
         ( "Y" . idp/dired-rsync))
   :config
   ;; ls style
   (require 'ls-lisp)
   (setq ls-lisp-use-insert-directory-program nil
         dired-listing-switches "-alh"
         ls-lisp-use-localized-time-format t
         ls-lisp-format-time-list '("%Y/%m/%d %H:%M" "%Y/%m/%d %H:%M")
         ls-lisp-dirs-first t
         ls-lisp-UCA-like-collation nil)

   ;; others
   (setq dired-dwim-target t)
   (require 'im-dired-plus))

(x wgrep/e
   :init
   (setq wgrep-enable-key "r"))

(x treemacs/w
   :config
   (setq treemacs-position 'left
         treemacs-persist-file (concat _CACHE_ "treemacs-persist")
         treemacs-width 35))

(x view
   :bind
   (:map view-mode-map
         ( "h"     .  backward-char )
         ( "l"     .  forward-char  )
         ( "j"     .  next-line     )
         ( "k"     .  previous-line )
         ( "%"     .  his-match-paren )
         ( "<DEL>" .  nil )))

(x flyspell/x
   :preface (setq ispell-program-name "ispell") ;; apt install ispell
   :if (executable-find ispell-program-name)
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (rcirc-mode . flyspell-mode))
   :config (ispell-change-dictionary "american" t))

(x hideshow
   :delight hs-minor-mode
   :hook ((prog-mode) . hs-minor-mode)
   :bind* (:map hs-minor-mode-map
                ([(M-down-mouse-1)] . nil)
                ([(M-mouse-1)] . hs-mouse-toggle-hiding)
                ("C-c f"   . hs-toggle-hiding)
                ("C-c F"   . my-hs-toggle-all)
                ("C-c C-f" . imdra-hideshow/body))
   :config

   (defun my-hs-toggle-all ()
     (interactive)
     (if (seq-find
          (lambda (ov) (and (overlayp ov) (overlay-get ov 'hs)))
          (overlays-in (point-min) (point-max)))
         (hs-show-all)
       (hs-hide-all)))

   (defun my-display-code-line-counts (ov)
     (when (eq 'code (overlay-get ov 'hs))
       (overlay-put ov 'face font-lock-warning-face)
       (overlay-put ov 'display
                    (format " ...%d..."
                            (count-lines (overlay-start ov)
                                         (overlay-end ov))))))

   (setq hs-set-up-overlay 'my-display-code-line-counts)

   (add-to-list
    'hs-special-modes-alist
    '(ruby-mode
      "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
      (lambda (arg) (ruby-end-of-block)) nil)))

(x image-mode
   :config
   (define-key image-mode-map "c" 'im/yank-current-buffer-name))


;;; Navigation

(x ivy/d
   :bind
   (:map ivy-minibuffer-map
         ("C-q" . ivy-immediate-done)
         ("C-j" . ivy-done)
         ("C-m" . ivy-alt-done))
   :config
   (setq ivy-use-virtual-buffers t)
   (setq smex-save-file (concat _CACHE_ ".smex-items")) ; frequently used cmds
   (ivy-mode 1))

(x swiper/w)

(x ace-window
   :bind ("C-x w" . ace-window)
   :config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(x hydra
   :init
   (defvar my-hydra-stack nil)

   (defun hydra-push (expr)
     (push `(lambda () ,expr) my-hydra-stack))

   (defun hydra-pop ()
     (interactive)
     (let ((x (pop my-hydra-stack)))
       (when x (funcall x)))))

(x ztree
   "Use `ztree-diff' to diff directories."
   :config
   (setq ztree-draw-unicode-lines t)
   (setq ztree-diff-additional-options nil) ; '("-w" "-i")
   (add-to-list 'ztree-diff-filter-list "^~"))


;;; Miscellaneous

(x projectile/w
   :init
   (setq projectile-completion-system 'ivy
         projectile-cache-file (concat _CACHE_ "__projectile.cache")
         projectile-known-projects-file (concat _CACHE_ "__projectile-bookmark.eld")
         projectile-mode-line-prefix ""
         projectile-mode-line-function 'my-projectile-mode-line)

   (defun my-projectile-mode-line ()
     (format " [-%s-]" (projectile-project-name)))

   (projectile-mode 1)
   (counsel-projectile-mode 1)

   :config
   (setq projectile-globally-ignored-file-suffixes '(".class" ".jar" ".exe" ".dll" ".so"))
   (setq counsel-find-file-ignore-regexp "Volume\\|RECYCLE.BIN\\|\\.class$\\|\\.jar$\\|out/.*/classes/\\|build/libs")

   (add-to-list 'projectile-globally-ignored-directories ".gradle")
   (add-to-list 'projectile-project-root-files ".pro")
   (add-to-list 'projectile-project-root-files "package.json")
   (add-to-list 'projectile-project-root-files-bottom-up ".idea"))

(x magit/w
   :if (executable-find "git")
   :bind ("C-c m" . magit-status)
   :init
   (x git-auto-commit/e)
   (magit-auto-revert-mode -1)
   (modify-coding-system-alist 'process "git" 'utf-8))

(x youdao-dictionary/w
   "Text to speech support:
   "
   "  pacman -S mpg123
   "
   "youdao-dictionary-use-chinese-word-segmentation t
   "
   :init
   (setq url-automatic-caching t youdao-dictionary-search-history-file (concat _CACHE_ ".youdao")))

(x engine-mode
   :init (engine-mode 1)
   :config
   (require 'format-spec)

   (defengine google
     "https://google.com/search?q=%s")

   (defengine google-cn
     "https://google.com/search?q=%s&lr=lang_zh-CN")

   (defengine dict-iciba
     "http://www.iciba.com/%s")

   (defengine github
     "https://github.com/search?ref=simplesearch&q=%s")

   (defengine stackoverflow
     "http://stackoverflow.com/search?q=%s")

   (defengine wikipedia
     "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")

   (defengine arch-wiki
     "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go"
     :browser 'eww-browse-url)

   (defengine wolfram-alpha
     "http://www.wolframalpha.com/input/?i=%s")

   (defengine youtube
     "http://www.youtube.com/results?aq=f&oq=&search_query=%s"))

(x simple-httpd
   "Start local server with port 5555:
   "
   " - M-x im/http-here
   "
   "Define your own servlet:
   "
   "  (defservlet time text/html () (insert (format \"%s\" (time))))
   "
   "Then you can visit with 'http://host:5555/time'
   "
   :commands
   (httpd-start httpd-stop httpd-running-p httpd-serve-directory)

   :init
   (defun im/httpd-here (&optional arg)
     (interactive "P")
     (let ((root default-directory))
       (setq port (if arg (read-number "Port: " 5555) 5555))
       (httpd-start :port port :root root)
       (message "http://localhost:%s | %s" port root)))

   :config
   (defservlet time text/html ()
     (insert (format "<h1>%s</h1>" (time)))))

(x vlf
   "Open huge file, Part by Part."
   :init (require 'vlf-setup))

(x gimp :commands (connect-gimp gimp-mode))

(x plantuml-mode
   :init
   (setq plantuml-jar-path "~/.emacs.d/plantuml.jar"))


(provide 'immor)

;;; immor.el ends here
