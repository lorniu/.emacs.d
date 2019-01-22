;;; cust.el --- Packages Initialize And Basic Configurations
;;; Commentary:

;;; Code:


;;; Initialization

(defun im/refresh-package (packages-required &optional block)
  (setq package-enable-at-startup nil
        package-user-dir "~/.emacs.d/packages"
        package-archives
        `(("melpa" . ,(if block "http://elpa.emacs-china.org/melpa/"
                        "http://melpa.org/packages/"))
          ("org"   . "http://elpa.emacs-china.org/org/")
          ("gnu"   . "http://elpa.emacs-china.org/gnu/")
          ))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (mapc 'package-install (seq-remove 'package-installed-p packages-required)))

(im/refresh-package
 '(use-package
    ;; basic
    bind-key delight key-chord

    ;; looking
    spacegray-theme rainbow-delimiters beacon page-break-lines rcirc-styles xterm-color

    ;; edit and utils
    expand-region dired-du session attrap syntax-subword
    neotree engine-mode youdao-dictionary dired-dups
    magit git-timemachine

    ;; search and nav
    ag wgrep-ag anzu smex ivy hydra ace-window ivy-pages

    ;; org-mode
    org-download ob-restclient ox-pandoc graphviz-dot-mode gnuplot

    ;; projects
    counsel-projectile yasnippet company simple-httpd websocket

    ;; lsp
    lsp-mode lsp-ui company-lsp cquery

    ;; frontend
    web-mode emmet-mode yaml-mode sass-mode json-mode
    js2-mode tide htmlize web-beautify company-web rjsx-mode

    ;; backends
    php-mode robe elpy c-eldoc lua-mode go-mode
    kotlin-mode clojure-mode groovy-mode scala-mode ensime
    erlang dante hindent powershell csharp-mode

    ;; company
    company-ghc company-php company-go

    ))


;;; Use-Package

(defvar im/need-idle-loads nil)
(mapc 'require '(use-package delight bind-key))

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat 'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive) (describe-function ',fun-name))))

(defmacro x (NAME &rest args)
  " e:demand w:wait else:defer d:delight x:disabled "
  (let* ((name-arr (split-string (symbol-name NAME) "/"))
         (name (intern (car name-arr)))
         (name-without-mode (replace-regexp-in-string "-mode" "" (car name-arr)))
         (doc-strings (cl-loop for i in args until (keywordp i) collect i))
         (args (cl-set-difference args doc-strings))
         (flags (cadr name-arr)) x-options)
    (push (if (seq-contains flags ?e) ':demand ':defer) x-options)
    (if (seq-contains flags ?x) (push ':disabled x-options))
    (if (seq-contains flags ?d) (push ':delight x-options))
    `(progn
       ,(if doc-strings `(defhelper ,(intern name-without-mode) ,@doc-strings))
       ,(if (seq-contains flags ?w) `(add-to-list 'im/need-idle-loads ',name))
       (use-package ,name ,@x-options ,@args))))


;;; Basic Variables

(setq user-full-name           "imfine"
      user-mail-address        "lorniu@gmail.com"

      default-directory        "~/"
      _CACHE_                  "~/.cache/emacs/"
      bbdb-file                (concat _CACHE_ "_bbdb")
      diary-file               (concat _CACHE_ "_diary")
      abbrev-file-name         (concat _CACHE_ "_abbrevs")
      bookmark-default-file    (concat _CACHE_ "_bookmark")

      inhibit-startup-message  t
      gnus-inhibit-startup-message t
      track-eol                t
      visible-bell             nil
      ring-bell-function       'ignore
      confirm-kill-processes   nil

      auto-save-interval 0
      auto-save-list-file-prefix nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      scroll-step              1
      scroll-margin            0
      hscroll-step             1
      hscroll-margin           1
      scroll-conservatively    101

      resize-mini-windows      t
      enable-recursive-minibuffers t
      column-number-mode       1
      fringes-outside-margins  t

      enable-local-variables   :all
      enable-local-eval        t

      ad-redefinition-action   'accept
      kill-ring-max            200
      mark-ring-max            10
      select-enable-clipboard  t
      help-window-select       t
      man-notify-method        'pushy
      woman-use-own-frame      nil)

(setq-default tab-width        4
              truncate-lines   t
              indent-tabs-mode nil
              bidi-display-reordering nil)

(setenv "TZ" "PRC")
(fset 'yes-or-no-p 'y-or-n-p)

(mapc (lambda (x) (put x 'disabled nil))
      '(narrow-to-region narrow-to-page downcase-region upcase-region set-goal-column erase-buffer))


;;; Editable

(defvar mi/sudo-prefix "/sudo::" "How to switch to super mode")
(defface find-file-root-header-face '((t (:foreground "white" :background "red3"))) "Edit as ROOT")

(defun su ()
  (interactive)
  (let ((pt (point))
        (buf-name (expand-file-name (or buffer-file-name default-directory))))
    (setq buf-name (or (file-remote-p buf-name 'localname) (concat mi/sudo-prefix buf-name)))
    (cl-flet ((server-buffer-done (buffer (&optional for-killing)) nil))
      (find-file buf-name))
    (goto-char pt)))

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer."
  (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
    (setq header-line-format (propertize " Edit as ROOT! " 'face 'find-file-root-header-face))))

(add-hook 'find-file-hook 'find-file-root-header-warning)
(add-hook 'dired-mode-hook 'find-file-root-header-warning)


;;; Custom-Set

(custom-set-variables
 )
(custom-set-faces
 )
(defun package--save-selected-packages (&rest opt) nil)


(provide 'cust)

;;; cust.el ends here
