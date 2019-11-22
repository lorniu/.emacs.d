(in-package :cl-user)


(defparameter *vps* "imxx.top" "Private Host")
(defparameter *ql-home* #+ros.init (merge-pathnames "lisp/quicklisp/" (roswell.util:homedir)) "~/.quicklisp/")


;;; quicklisp

#-quicklisp
(load (merge-pathnames "setup.lisp" *ql-home*) :verbose t)

(setf (fdefinition 'qload) #'ql:quickload)

(pushnew "~/vvv/lisp-local/" ql:*local-project-directories*)


;;; cl patcher

(in-package :cl)

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

;; custom hashtable output
(defparameter *print-hashtable* nil "Use custom HASH print method.")
(defun im/toggle-print-hashtable () (setf *print-hashtable* (not *print-hashtable*)))
(defmethod print-object :around ((object hash-table) stream)
  (if *print-hashtable*
      (format stream "#HashTable {~%    ~{~{~a: ~a~%~}~^    ~}}"
              (loop for key being the hash-keys of object using (hash-value value)
                    collect (list key value)))
    (call-next-method)))

;; exports
(export '(qload-and-in
          defpackage-and-in
          dir-symbols
          alias-package
          import-as
          load-relative
          im/toggle-print-hashtable
          ))


;;; dexador

(ql:quickload :dexador)

(in-package :dexador)
(shadow 'trace) (export 'trace)

(defun trace (uri &rest args
              &key version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects
                force-binary want-stream
                ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (let ((*verbose* t))
    (apply #'request uri :method :get args)
    (values)))


;;; alexandria

(in-package :cl-user)

(asdf:load-system :alexandria)
(unlock-package :alexandria)
(rename-package :alexandria :alexandria '(a alexandria.1.0.0))

;;; load settings from external file
;;; try to load external configuration

(defparameter imlisp-share "~/.notes/x.share/lisp/")

(load (merge-pathnames "init.lisp" imlisp-share)
      :verbose t :if-does-not-exist nil)

(load (merge-pathnames (string-downcase (format nil "~a-~a" (software-type) (uiop:run-program "hostname" :output '(:string :stripped t))))
                       imlisp-share)
      :verbose t :if-does-not-exist nil)


;;; miscellaneous

(export '(*vps qload))

(pushnew :im-loaded *features*)

(declaim (optimize (debug 3)))
