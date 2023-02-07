;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Min

(if-let* ((env (or (getenv-internal "M") (getenv-internal "MIN") (getenv-internal "DM")))
          (min (and env (if (file-exists-p env) init (expand-file-name "init-min.el" user-emacs-directory)))))
    (load min nil (not init-file-debug) nil 'must-suffix)

;;; Constants

  (defconst IS-MAC       (eq system-type 'darwin))
  (defconst IS-BSD       (eq system-type 'berkeley-unix))
  (defconst IS-LINUX     (eq system-type 'gnu/linux))
  (defconst IS-WIN       (memq system-type '(cygwin windows-nt ms-dos)))
  (defconst NATIVECOMP   (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
  (defconst IS-G         (display-graphic-p))
  (defconst IS-NG        (not (display-graphic-p)))
  (defconst IS-LINUX-G   (and IS-LINUX (display-graphic-p)))
  (defconst IS-LINUX-NG  (and IS-LINUX (not (display-graphic-p))))

;;; Paths & Modules

  (add-to-list 'load-path (locate-user-emacs-file "core"))
  (load (expand-file-name "init-aux.el" user-emacs-directory) 'noerror (not init-file-debug)) ; Most first, personal + ignored by git

  (prog1 (require 'bm)
    (imload 'util)
    (imload 'fast)
    (imload 'cust) ; will load personal-init.el inner this
    (imload 'dist)
    (imload 'face)

    (imload 'imods)
    (imload 'imod-safety)
    (imload 'imod-transient)
    (imload 'imod-scratch)
    (imload 'imod-session)
    (imload 'imod-display)
    (imload 'imod-edit+)
    (imload 'imod-colorful-media)
    (imload 'imod-tools)
    (imload 'imod-calx)
    (imload 'imod-print)
    (imload 'imod-tramp)
    (imload 'imod-mmm)
    (imload 'imod-dired)
    (imload 'imod-project)
    (imload 'imod-evil)
    (imload 'imod-search)
    (imload 'imod-completing)
    (imload 'imod-snippet)
    (imload 'imod-folding)
    (imload 'imod-tui-actions)
    (imload 'imod-vcs)
    (imload 'imod-shell)
    (imload 'imod-ime)
    (imload 'imod-gpt)

    (imload 'idevelop)
    (imload 'idev-hexl)
    (imload 'idev-lsp)
    (imload 'idev-cc)
    (imload 'idev-rust)
    (imload 'idev-jvm)
    (imload 'idev-dotnet)
    (imload 'idev-lisp)
    (imload 'idev-python)
    (imload 'idev-ruby)
    (imload 'idev-erlang)
    (imload 'idev-elixir)
    (imload 'idev-haskell)
    (imload 'idev-golang)
    (imload 'idev-php)
    (imload 'idev-sql)
    (imload 'idev-web)
    (imload 'idev-markdown)

    (imload 'iwww)
    (imload 'iwww-irc)
    (imload 'iwww-gnus)

    (imload 'iorg)
    (imload 'iorg-notes)
    (imload 'iorg+TeX)

    (imload 'ikmd-keys-and-commands)
    (imload 'ikmd-kmacro)

    (imload 'imsilly)
    (imload 'implay 'load-from-anywhere-if-it-exists)
    (imload 'imsketch 'load-from-anywhere-if-it-exists)
    (imload 'over)))

;;; init.el ends here
