;;; cist.el --- Package Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom *vps* "imxx.top"
  "The remote host. In the most top."
  :type 'string :group 'imfine)

(defcustom ic/private-custom-file (format "~/.emacs.d/init_%s.el" (system-name))
  "Private custom file, which contains configs for different endpoints."
  :type 'string :group 'imfine)

(defcustom ic/elpa-use-mirror t
  "ELPA use mirror or not."
  :type 'boolean :group 'imfine)

(defmacro p/refresh (&rest packages-required)
  `(let ((repo-origin
          `(("melpa" . "http://melpa.org/packages/")
            ("gnu"   . "http://elpa.gnu.org/packages/")))
         (repo-mirror
          `(("melpa" . "http://elpa.emacs-china.org/melpa/")
            ("gnu"   . "http://elpa.emacs-china.org/gnu/")))
         (tencent-mirror
          `(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
            ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/"))))

     (defun p/install ()
       (interactive)
       (package-refresh-contents)
       (cl-loop for p in ',packages-required unless (package-installed-p p) do (package-install p))
       (message "Install Finished."))
     (defun p/elpa-use-origin () (interactive) (message "%s" (setq package-archives repo-origin)))
     (defun p/elpa-use-mirror () (interactive) (message "%s" (setq package-archives repo-mirror)))

     (setq package-user-dir "~/.emacs.d/packages")
     (package-initialize)
     (if ic/elpa-use-mirror (p/elpa-use-mirror) (p/elpa-use-origin))

     (when (cl-find-if (lambda (p) (not (package-installed-p p))) ',packages-required)
       (push "* Some packages missing, run `p/install' to install." loaded-messages))))



;;; Packages

(p/refresh
 ;; basic
 use-package bind-key delight key-chord

 ;; looking
 spacegray-theme
 rainbow-delimiters beacon page-break-lines
 rcirc-styles xterm-color

 ;; edit and utils
 alert session attrap
 syntax-subword expand-region
 dired-dups ztree neotree
 magit git-timemachine
 engine-mode youdao-dictionary

 ;; search and nav
 ag wgrep-ag anzu smex
 ivy hydra ace-window ivy-pages

 ;; org-mode
 org-download ob-restclient ox-pandoc graphviz-dot-mode gnuplot

 ;; projects
 counsel-projectile yasnippet company websocket

 ;; lsp
 lsp-mode lsp-ui company-lsp

 ;; frontend
 web-mode emmet-mode yaml-mode sass-mode json-mode
 js2-mode tide htmlize web-beautify company-web rjsx-mode

 ;; backends
 c-eldoc cquery
 php-mode elpy lua-mode go-mode
 robe erlang alchemist
 dante hindent
 powershell
 csharp-mode
 kotlin-mode clojure-mode groovy-mode scala-mode ensime lsp-java

 ;; other language specified
 company-ghc company-php company-go

 ;; miscellaneous
 emms elfeed elfeed-org uuidgen)



;;; Use-Package

(setq use-package-form-regexp-eval
      `(concat ,(eval-when-compile
                  (concat "^\\s-*(" (regexp-opt '("use-package" "x" "require") t) "\\s-+\\("))
               (or (bound-and-true-p lisp-mode-symbol-regexp) "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
      use-package-enable-imenu-support t)

(require 'use-package)

(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat 'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))

(defmacro x (NAME &rest args)
  "Flags: e/demand w/wait d/delight x/disabled."
  (let* ((name-arr (split-string (symbol-name NAME) "/"))
         (name (intern (car name-arr)))
         (flags (cadr name-arr))
         (doc-strings (cl-loop for i in args until (keywordp i) collect i))
         (x-options
          (list-nn (if (seq-contains flags ?e) :demand :defer) t
                   (if (seq-contains flags ?d) :delight)
                   (if (seq-contains flags ?x) :disabled)))
         (options (cl-set-difference args doc-strings))
         (options* (use-package-normalize-keywords name options)))
    (list-nn 'progn
             (if (seq-contains flags ?w)
                 (let ((c (plist-get options* :if)))
                   `(if ,(or (eq c nil) c) (add-to-list 'im/lazy-modules ',name))))
             (if doc-strings
                 `(defhelper ,(intern (replace-regexp-in-string "-mode" "" (car name-arr))) ,@doc-strings))
             `(use-package ,name ,@x-options ,@options))))



;;; Lazy Load Modules

(defvar im/lazy-modules nil)

(defun my-lazy-load ()
  (let ((then (current-time)))
    (message "[%s] Lazy loading..." (time then 2))
    (cl-loop for mode in im/lazy-modules
             do (unless (featurep mode)
                  (require mode) (message "> %s" mode)))
    (message "[%s] Lazy loaded, %.2fs elapsed."
             (time nil 2) (time-subtract-seconds (current-time) then)))
  (remove-hook 'auto-save-hook 'my-lazy-load))

(add-hook 'auto-save-hook 'my-lazy-load)



;;; Load-Path/Theme-Path

(dolist (dir (directory-files "~/.emacs.d/extra" t))
  (if (and (not (eq (file-name-extension dir) "")) (file-directory-p dir))
      (add-to-list 'load-path dir)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/extra/themes")

;;; Tips for the first time

(unless (file-exists-p ic/private-custom-file)
  (push (concat "\nFirst time launch emacs? Maybe you should:\n"
                "- Add cygwin/msys2 to PATH variable;\n"
                "- Add private custom to `ic/private-custom-file' file.\n")
        loaded-messages))

;;; Hook for special endpoint

(setq custom-file (ensure-file ic/private-custom-file))
(defmacro -----> (&rest body) `(with-eval-after-load 'imfine ,@body))
(defer-til-hook '(custom-set-faces) 'after-init-hook) ; high period !
(load ic/private-custom-file t t) ; always on top !


(provide 'cist)

;;; cist.el ends here
