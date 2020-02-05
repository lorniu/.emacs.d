(in-package :cl-user)

(defparameter *vps* "imxx.top" "Private Host")
(defparameter *quicklisp-home* #+ros.init (merge-pathnames "lisp/quicklisp/" (roswell.util:homedir)) "~/.quicklisp/")


;;; quicklisp

#-quicklisp
(load (merge-pathnames "setup.lisp" *quicklisp-home*) :verbose t)


;;; cl patcher

(in-package :cl)

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

(defun add-to-path-quicklisp (dir)
  (pushnew dir ql:*local-project-directories*))

(defun add-to-path-asdf (dir)
  (pushnew dir asdf:*central-registry*))

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
(export '(dir-symbols
          alias-package
          import-as
          load-relative
          im/toggle-print-hashtable
          add-to-path-asdf
          add-to-path-quicklisp
          ))


;;; [preload] alexandria

(in-package :cl-user)

(asdf:load-system :alexandria)
(unlock-package :alexandria)
(rename-package :alexandria :alexandria '(a alexandria.1.0.0))


;;; [preload] dexador

(ql:quickload :dexador)

(in-package :dexador)
(shadow 'trace)
(export 'trace)

(defun trace (uri &rest args
              &key version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects
                force-binary want-stream
                ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (let ((*verbose* t))
    (apply #'request uri :method :get args)
    (values)))


;;; Load external/private configuration

(let ((dir "~/.notes/share/lisp/"))
  (load (merge-pathnames "init.lisp" dir) :verbose t)
  (load (merge-pathnames (string-downcase (format nil "init-~a.lisp" (machine-instance))) dir)
        :verbose t :if-does-not-exist nil))


;;; miscellaneous

(export '(*vps*))

(pushnew :imloaded *features*)

(declaim (optimize (debug 3)))
