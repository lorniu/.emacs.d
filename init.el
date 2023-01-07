;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(if-let* ((env (or (getenv-internal "M") (getenv-internal "MIN") (getenv-internal "DM")))
          (min (and env (if (file-exists-p env) init (expand-file-name "init-min.el" user-emacs-directory)))))
    (load min 'noerror (not init-file-debug) nil 'must-suffix)

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
  (load (expand-file-name "init-aux.el" user-emacs-directory) 'noerror (not init-file-debug))

  (prog1 (require 'bm)
    (imload 'utils)
    (imload 'favors)
    (imload 'cust)
    (imload 'dist)
    (imload 'face)

    (imload 'imod-transient)
    (imload 'imod-general)
    (imload 'imod-security)
    (imload 'imod-edit+)
    (imload 'imod-apps)
    (imload 'imod-reader)
    (imload 'imod-tramp)
    (imload 'imod-mmm)
    (imload 'imod-dired)
    (imload 'imod-project)
    (imload 'imod-evil)
    (imload 'imod-search+)
    (imload 'imod-completing)
    (imload 'imod-snippet)
    (imload 'imod-folding)
    (imload 'imod-tui-actions)
    (imload 'imod-vcs)
    (imload 'imod-shell)
    (imload 'imod-ime)
    (imload 'imod-print)
    (imload 'imod-eaf)

    (imload 'icode)
    (imload 'icod-lsp)
    (imload 'icod-tags)
    (imload 'icod-cc)
    (imload 'icod-rust)
    (imload 'icod-jvm)
    (imload 'icod-dotnet)
    (imload 'icod-lisp)
    (imload 'icod-python)
    (imload 'icod-ruby)
    (imload 'icod-erlang)
    (imload 'icod-elixir)
    (imload 'icod-haskell)
    (imload 'icod-golang)
    (imload 'icod-php)
    (imload 'icod-sql)
    (imload 'icod-hexl)
    (imload 'icod-resx)
    (imload 'icod-frontend)
    (imload 'icod-markdown)

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
