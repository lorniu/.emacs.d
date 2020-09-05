;;; predist.el --- Packages preparing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst pure-load-path (copy-sequence load-path))
(setq package-user-dir (loce "elpa"))

(defun p/refresh-lock (&optional no-clean)
  "Lock, disable or use local package, according `ic/package-load-alist'"
  (interactive "P")
  (unless no-clean
    (message "Reset load-path and package-load-list")
    (setq load-path (copy-sequence pure-load-path))
    (setq package-load-list '(all))
    (setq package-activated-list nil)
    (package-activate-all))
  (cl-loop with remove-from-load-path = (lambda (pkg)
                                          (cl-delete-if
                                           (lambda (lp) (string-match-p (format "/%s-[0-9]" pkg) lp))
                                           load-path))
           for (p . v) in ic/package-load-alist
           if (null v) do ; nil: don't load
           (funcall remove-from-load-path p)
           (add-to-list 'package-load-list (list p nil))
           else if (string-match-p "/" v) do ; dir: replace with this
           (funcall remove-from-load-path p)
           (add-to-list 'package-load-list (list p nil))
           (add-to-list 'load-path (expand-file-name v))
           else do ; version/etc: load specific version
           (add-to-list 'package-load-list (list p v)))
  (when ic/package-load-alist
    (message "Pkg-Lock-Alist: %s" ic/package-load-alist)))



(defvar p/elpa-repos
  (list :163          "https://mirrors.163.com/elpa/"
        :tsinghua     "http://mirrors.tuna.tsinghua.edu.cn/elpa/"
        :ustc         "https://mirrors.ustc.edu.cn/elpa/"
        :emacs-china  "https://elpa.emacs-china.org/"
        :melpa-stable "https://stable.melpa.org/packages/"
        :origin `(("gnu"   . "https://elpa.gnu.org/packages/")
                  ("org"   . "https://orgmode.org/elpa/")
                  ("melpa" . "https://melpa.org/packages/"))))

(defun p/elpa-repo (&optional upstream)
  (interactive (list (intern
                      (completing-read "Choose your elpa upstream: "
                                       (cl-loop for m in p/elpa-repos by #'cddr
                                                collect (symbol-name m))
                                       nil t))))
  (unless upstream (setq upstream (car p/elpa-repos)))
  (setq package-archives
        (let ((repo (plist-get p/elpa-repos upstream)))
          (if (consp repo)
              repo
            (cl-loop for type in (list "gnu" "org" "melpa")
                     collect (cons type (format "%s%s/" repo type))))))
  (if (called-interactively-p 'any)
      (message "%s" package-archives)
    (message "[repo:%s]" upstream)))

(defmacro p/refresh-packages (&rest packages-required)
  (declare (indent 2))
  `(let ((tip "\n(progn\n  (im/proxy (quote SOCK))\n  (p/elpa-repo :origin)\n  (p/install)\n)\n"))
     (defun p/packages ()
       (cl-remove-if (lambda (p) (package-disabled-p p nil)) ',packages-required))
     (defun p/packages-lacks ()
       (cl-remove-if #'package-installed-p (p/packages)))
     (defun p/install ()
       (interactive)
       (package-refresh-contents)
       (cl-loop for p in (p/packages-lacks) do (package-install p))
       (message "Install Finished."))
     (when (= (length (p/packages-lacks)) (length (p/packages))) (user-error "Have you installed any packages? Maybe:\n%s" tip))
     (when (p/packages-lacks) (user-error "Some packages missing %s\n\nPlease install first:\n%s\n\n" (p/packages-lacks) tip))))

(p/elpa-repo ic/elpa-repo-type)



;; pretend these dependencies already installed
(add-to-list 'package--builtin-versions `(simple-httpd 1 5 1))

;; yes, initialization
(package-initialize)

;; disable/redirect packages according `ic/package-lock-alist'
(p/refresh-lock t)


(provide 'predist)

;;; predist.el ends here
