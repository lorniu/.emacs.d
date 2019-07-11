(in-package :cl)

(setf (fdefinition 'qload) #'ql:quickload)

(defmacro qload-and-in (pkg)
  `(progn
     (ql:quickload ,pkg)
     (in-package ,pkg)))

(defmacro defpackage-and-in (&optional (pkg :cl-user) &rest args)
  (unless args
    (setq args '((:use :cl))))
  `(progn
     (defpackage ,pkg ,@args)
     (in-package ,pkg)))

(defun dir-symbols (package &key filter all (fn #'print))
  "Print symbols in certain package."
  (let (lst)
    (flet ((add-match-to-list (p)
                              (if (or (not filter)
                                      (if (functionp filter) (funcall filter p)
                                        (search (string-upcase filter) (symbol-name p))))
                                  (push p lst))))
      (if all
          (do-symbols (s (find-package package) lst)
            (add-match-to-list s))
        (do-external-symbols (s (find-package package) lst)
                             (add-match-to-list s))))
    (mapc fn (sort lst #'string<))) t)

(defmacro alias-package (package nickname)
  `(let ((package ,package) (nickname ,nickname))
     (when (or (stringp package) (keywordp package))
       (setf package (find-package package)))
     (check-type package package)
     (check-type nickname (or string keyword))
     (rename-package package (package-name package)
                     (adjoin (symbol-name nickname) (package-nicknames package)
                             :test #'string=))
     (package-nicknames package)))

(defun alias% (as symbol &key (package *package*))
  (if (fboundp symbol) (setf (symbol-function as) (symbol-function symbol)))
  (if (boundp symbol) (setf (symbol-value as) (symbol-value symbol)))
  (setf (symbol-plist as) (symbol-plist symbol))
  (shadowing-import as package))

(defmacro import-as (symbol as &key (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (alias% ',as ',symbol :package ,package)))

(defun load-relative (filename)
  (load (compile-file (merge-pathnames filename *load-truename*))))

(export '(qload
          qload-and-in
          defpackage-and-in
          dir-symbols
          alias-package
          import-as
          load-relative
          ))
