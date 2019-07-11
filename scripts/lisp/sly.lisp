(in-package :cl-user)


(defparameter *vps* "imxx.top" "Private Host")
(defparameter imlisp-home "~/.notes/x.code.lisp/")
(defparameter imlisp-local "~/vvv/lisp-local/")
(defparameter *ql-home*
  #+ros.init (merge-pathnames "lisp/quicklisp/" (roswell.util:homedir))
  "~/.quicklisp/")


;;; quicklisp
#-quicklisp (load (merge-pathnames "setup.lisp" *ql-home*) :verbose t)
(pushnew imlisp-local ql:*local-project-directories*)

;;; some hacks for cl package
(load (merge-pathnames "util.lisp" *load-truename*))
(load (merge-pathnames "plus.lisp" *load-truename*))

;;; external settings if possible
(load (merge-pathnames "bootstrap.lisp" imlisp-home) :if-does-not-exist nil)


;;; miscellaneous
(export '(*vps*))
(pushnew :im-loaded *features*)
(declaim (optimize (debug 3)))
