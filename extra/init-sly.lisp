(in-package :cl-user)


(defconstant *quicklisp-home*
  #+ros.init (merge-pathnames "lisp/quicklisp/" (roswell.util:homedir))
  "~/.quicklisp/")


(defparameter *vps* "127.0.0.1" "Private Host")

(defvar local-custom-directory "~/.notes/x.code.lsp/")


;;; Asdf

(defun sload (path &optional home)
  (setq path (if home
                 (merge-pathnames path home)
                 (pathname path)))
  (if (search "*" (namestring path))
      (map nil (lambda (d) (pushnew d asdf:*central-registry*)) (directory path))
      (pushnew path asdf:*central-registry*))
  asdf:*central-registry*)

(unless (find-if (lambda (p) (search "quicklisp" (namestring p))) asdf:*central-registry*)
  (sload "dists/quicklisp/software/*/" *quicklisp-home*))


;;; Quicklisp

#-quicklisp
(load (merge-pathnames "setup.lisp" *quicklisp-home*) :verbose t)


;;; Miscellaneous

(ql:quickload :dexador)

(if (uiop:directory-exists-p local-custom-directory)
    (load (merge-pathnames "bootstrap.lisp" local-custom-directory)))

(setf (fdefinition 'qload) #'ql:quickload)
(setf (fdefinition 'curl) #'dex:get)


(declaim (optimize (debug 3)))

(pushnew :im-loaded *features*)
