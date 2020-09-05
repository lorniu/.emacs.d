;; -*- lexical-binding: t -*-

(defconst IS-G         (display-graphic-p))
(defconst IS-NG        (not (display-graphic-p)))
(defconst IS-LINUX-G   (and IS-LINUX (display-graphic-p)))
(defconst IS-LINUX-NG  (and IS-LINUX (not (display-graphic-p))))



(unless quiet-init
  (imload 'itop)

  (imload 'dist)
  (imload 'face)

  (imload 'iccc)
  (imload 'imods)
  (imload 'ioox)
  (imload 'icoding)
  (imload 'iwww)

  (imload 'icmd-keys)
  (imload 'icmd-kmacro)

  (imload 'imsilly)
  (imload 'iserver)

  (imload 'implay 'load-from-anywhere-if-it-exists)
  (imload 'imsketch 'load-from-anywhere-if-it-exists))
