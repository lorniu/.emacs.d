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
    bind-key diminish

    ;; looking
    atom-dark-theme rainbow-delimiters beacon page-break-lines ivy-pages rcirc-styles

    ;; edit
    iedit multiple-cursors ivy smex

    ;; search
    ag wgrep-ag anzu

    ;; utils
    dired-du session graphviz-dot-mode magit neotree gnuplot attrap

    ;; org-mode
    org-download ob-restclient ox-pandoc dired-dups

    ;; fronts
    web-mode emmet-mode yaml-mode sass-mode impatient-mode js2-mode tide htmlize web-beautify

    ;; backends
    slime php-mode robe elpy c-eldoc lua-mode go-mode
    kotlin-mode scala-mode clojure-mode groovy-mode
    erlang dante hindent powershell

    ;; lsp
    lsp-mode lsp-ui company-lsp cquery

    ;; projects
    counsel-projectile yasnippet company

    ;; companies
    company-ghc company-php company-go

    ))


;;; Load-Path/Theme-Path

(dolist (dir (directory-files "~/.emacs.d/extra" t))
  (if (and (not (eq (file-name-extension dir) ""))
           (file-directory-p dir))
      (add-to-list 'load-path dir)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/ext/themes")


;;; Basic Variables

(setq default-directory        "~/"
      user-full-name           "imfine"
      user-mail-address        "lorniu@gmail.com"
      eshell-aliases-file      "~/.emacs.d/ass/eshell-alias"
      _CACHE_                  "~/.cache/emacs/"
      bbdb-file                (concat _CACHE_ "_bbdb")
      diary-file               (concat _CACHE_ "_diary")
      bookmark-default-file    (concat _CACHE_ "_bookmark")
      abbrev-file-name         (concat _CACHE_ "_abbrevs")
      recentf-save-file        (concat _CACHE_ "_recentf")
      eshell-directory-name    (concat _CACHE_ "eshell")
      org-publish-timestamp-directory (concat _CACHE_ "org-publish-timestamp/")

      auto-save-interval 0
      auto-save-list-file-prefix nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      inhibit-startup-message  t
      gnus-inhibit-startup-message t
      track-eol                t
      visible-bell             nil
      ring-bell-function       'ignore

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

      kill-ring-max            200
      select-enable-clipboard  t
      help-window-select       t
      man-notify-method        'pushy
      woman-use-own-frame      nil)

(setq-default tab-width        4
              truncate-lines   t
              indent-tabs-mode nil
              bidi-display-reordering nil
              show-trailing-whitespace nil)

(setenv "TZ" "PRC")
(fset 'yes-or-no-p 'y-or-n-p)

(mapc (lambda (x) (put x 'disabled nil))
      '(narrow-to-region narrow-to-page downcase-region upcase-region set-goal-column erase-buffer))


;;; Use-Package

(defvar im/need-idle-loads nil)
(mapc 'require '(use-package diminish bind-key))

(defmacro x (NAME &rest args)
  " e:demand w:wait else:defer v:dim x:disabled "
  (let* ((name-arr (split-string (symbol-name NAME) "/"))
         (name (intern (car name-arr)))
         (flag (cadr name-arr)) x-options)
    (push (if (seq-contains flag ?e) ':demand ':defer) x-options) ;
    (if (seq-contains flag ?x) (push ':disabled x-options))
    (if (seq-contains flag ?v) (push ':diminish x-options))
    `(progn
       ,(if (seq-contains flag ?w) `(add-to-list 'im/need-idle-loads ',name))
       (use-package ,name ,@x-options ,@args))))


;;; Editable

(defun su ()
  (interactive)
  (let ((pt (point))
        (buf-name (expand-file-name (or buffer-file-name default-directory))))
    (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
    (cl-flet ((server-buffer-done (buffer (&optional for-killing)) nil))
      (find-file buf-name))
    (goto-char pt)))

(defface find-file-root-header-face '((t (:foreground "white" :background "red3"))) "Edit as ROOT")

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
