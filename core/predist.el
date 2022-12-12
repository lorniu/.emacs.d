;;; predist.el --- Packages. -*- lexical-binding: t -*-

;; Set special pacakges demo:
;;
;;   (with-eval-after-load 'predist
;;     (p/special
;;      (sly   . "~/source/sly")
;;      (go-translate . "~/source/go-translate")
;;      (company . nil)
;;    ))

;;; Code:

(defvar p/elpa-repos
  '(:origin
    (setq package-archives
          `(("elpa"       . "https://elpa.gnu.org/packages/")
            ("nongnu"     . "https://elpa.nongnu.org/nongnu/")
            ("nongnu-dev" . "https://elpa.nongnu.org/nongnu-devel/")
            ("melpa"      . "https://melpa.org/packages/")))
    :emacs-china
    (setq package-archives
          `(("gnu"    . "http://elpa.zilongshanren.com/gnu/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("melpa"  . "http://elpa.zilongshanren.com/melpa/")))
    :ustc
    (setq package-archives
          `(("elpa"  . "https://mirrors.ustc.edu.cn/elpa/gnu/")
            ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    :tsinghua
    (setq package-archives
          `(("elpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu")
            ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa")))))

(defun p/repo (&optional upstream)
  (interactive (list (intern (completing-read "package-archives: " (cl-loop for m in p/elpa-repos by #'cddr collect (symbol-name m)) nil t))))
  (unless upstream (setq upstream (car p/elpa-repos)))
  (eval (plist-get p/elpa-repos upstream))
  (if (called-interactively-p 'any)
      (message "%s" package-archives)
    (message "[repo:%s]" upstream)))

(defmacro p/special (&rest pkg-list)
  "(p/special (leaf . dir) (simple-httpd . (1 5 1)) (company . nil))"
  `(progn ,@(cl-loop for (p . v) in pkg-list
                     if (consp v) collect `(add-to-list 'package--builtin-versions '(,p ,@v)) ; pretend installed
                     else if (stringp v) collect `(push ,v load-path) ; use this instead of elpa's one
                     else if (null v) collect `(add-to-list 'package-load-list '(,p . nil)) ; dont load this
                     else collect `',p)))

(defmacro p/loads (&rest packages-required)
  (declare (indent 2))
  `(let ((tip "\n(progn\n  (im/proxy (quote SOCK))\n  (setq package-check-signature nil)\n  (p/repo :origin)\n  (p/install)\n)\n"))
     (defun p/packages ()
       (cl-loop for o in ',packages-required
                for p = (if (listp o) (if (eval (cadr o)) (car o)) o)
                if (and p (not (package-disabled-p p nil))) collect p))
     (defun p/packages-lacks ()
       (cl-remove-if #'package-installed-p (p/packages)))
     (defun p/install (&optional dont-select)
       (interactive "P")
       (package-refresh-contents)
       (dolist (p (p/packages-lacks)) (package-install p dont-select))
       (message "Install Finished."))
     (when-let ((lacks (p/packages-lacks)))
       (if (= (length lacks) (length (p/packages)))
           (warn "Have you installed any packages?\n\nMaybe:\n%s" tip))
       (warn "Total %s packages missing:\n\n%s\n\nPlease install first:\n%s\n\n"
             (length lacks)
             (with-temp-buffer
               (insert (format "%s" lacks))
               (fill-region (point-min) (point-max))
               (buffer-string))
             tip))))

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

(defun p/native-compile-async ()
  "Native specific directory manually."
  (interactive)
  (let ((dir (read-directory-name "Directory to native compile: " package-user-dir nil t)))
    (native-compile-async dir 'recursively)))


;;; package.el

(defconst pure-load-path (copy-sequence load-path))

;; 1. find [package-user-dir] and [package-directory-list], add to [package-alist]
;; 2. load [package-archives] to [package-archive-contents] as all packages in elpa
;; 3. according [package-load-list], load pkg, refresh [load-path] and [package-activated-list]
(require 'package)
(setq package-user-dir (loce "elpa"))
(p/repo ic/package-repo)
(package-initialize)

;; Record builtins in [package--builtins] and [package--builtin-versions]
;;(p/special (simple-httpd . (1 5 1)))
(run-hooks 'package-initialize-hook)
(add-hook 'package-menu-mode-hook (lambda () (hl-line-mode 1)))


;;; native comp

(when NATIVECOMP
  (add-to-list 'native-comp-eln-load-path (locc "eln/"))
  (setq native-comp-bootstrap-deny-list (list ".*/subr--trampoline.*")))


;;; load-path

(cl-loop for root in (append ic/external-load-path-tops (list (loce "extra") (loce "site-lisp")))
         for dirs = (ignore-errors (directory-files root t))
         do (cl-loop for dir in dirs
                     if (and (not (eq (file-name-extension dir) "")) (file-directory-p dir)) do
                     (add-to-list 'load-path dir)))

(add-to-list 'custom-theme-load-path (loce "themes"))

(provide 'predist)

;;; predist.el ends here
