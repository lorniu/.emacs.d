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

;;; common utils

(ql:quickload :dexador)
(in-package :dexador)
(shadow 'trace) (export 'trace)
(defun trace (url) (let ((*verbose* t)) (get url)) (values))

(in-package :cl-user)
(setf (fdefinition 'curl) #'dex:get)
(setf (fdefinition 'qload) #'ql:quickload)

;;; private settings

(load (merge-pathnames "bootstrap.lisp" imlisp-home) :if-does-not-exist nil)

;;; miscellaneous

(export '(*vps* qload curl))
(declaim (optimize (debug 3)))
(pushnew :im-loaded *features*)
