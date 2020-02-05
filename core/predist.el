;;; predist.el --- Packages preparing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst pure-load-path (copy-sequence load-path))

(defun p/refresh-lock (&optional no-clean)
  "Lock, disable or use local package, according `ic/package-lock-alist'"
  (interactive "P")
  (unless no-clean
    (message "Reset load-path and package-load-list")
    (setq load-path (copy-sequence pure-load-path))
    (setq package-load-list '(all))
    (setq package-activated-list nil)
    (package-activate-all))
  (cl-loop for pv in ic/package-lock-alist
           for p = (car pv) and v = (cdr pv)
           for remove-from-lp = (lambda (p)
                                  (cl-delete-if
                                   (lambda (lp) (string-match-p (format "/%s-[0-9]" p) lp))
                                   load-path))
           if (null v) do
           (funcall remove-from-lp p)
           (add-to-list 'package-load-list (list p nil))
           else if (string-match-p "/" v) do
           (funcall remove-from-lp p)
           (add-to-list 'package-load-list (list p nil))
           (add-to-list 'load-path (expand-file-name v))
           else do
           (add-to-list 'package-load-list (list p v)))
  (message "Refresh Done: %s" ic/package-lock-alist))

(defmacro p/refresh-packages (&rest packages-required)
  (declare (indent 2))
  `(let ((origin `(("melpa" . "http://melpa.org/packages/")
                   ("gnu"   . "http://elpa.gnu.org/packages/")
                   ("org"   . "https://orgmode.org/elpa/")))
         (mirror `(("melpa" . "http://elpa.emacs-china.org/melpa/")
                   ("gnu"   . "http://elpa.emacs-china.org/gnu/")
                   ("org"   . "http://elpa.emacs-china.org/org/")))
         (tip "\n(progn\n  (im/proxy (quote SOCK))\n  (p/repo-origin)\n  (p/install)\n)\n"))
     (defun p/packages ()
       (cl-remove-if (lambda (p) (package-disabled-p p nil)) ',packages-required))
     (defun p/packages-lacks ()
       (cl-remove-if #'package-installed-p (p/packages)))
     (defun p/install ()
       (interactive)
       (package-refresh-contents)
       (cl-loop for p in (p/packages) unless (package-installed-p p) do (package-install p))
       (message "Install Finished."))
     (defun p/repo-origin ()
       (interactive)
       (setq package-archives origin)
       (message "%s" (if (called-interactively-p 'any) package-archives 'repo-origin)))
     (defun p/repo-mirror ()
       (interactive)
       (setq package-archives mirror)
       (message "%s" (if (called-interactively-p 'any) package-archives 'repo-mirror)))

     (if ic/elpa-use-mirror (p/repo-mirror) (p/repo-origin))
     (when (= (length (p/packages-lacks)) (length (p/packages))) (user-error "Have you installed any packages? Maybe:\n%s" tip))
     (when (p/packages-lacks) (user-error "Some packages missing %s\n\nPlease install first:\n%s\n\n" (p/packages-lacks) tip))))

;; pretend these dependencies already installed
(add-to-list 'package--builtin-versions `(simple-httpd 1 5 1))

;; yes, initialization
(package-initialize)

;; disable/redirect packages according `ic/package-lock-alist'
(p/refresh-lock t)


(provide 'predist)

;;; predist.el ends here
