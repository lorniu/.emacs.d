;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;; Read code of `command-line-1' for more detail of the load progress.

;;; Code:

(if-let* ((env (or (getenv-internal "M") (getenv-internal "MIN") (getenv-internal "DM"))))
    (load (if (file-exists-p env) env (locate-user-emacs-file "init-min.el")) nil (not init-file-debug) nil 'must-suffix)

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

;;; Most first, personal + ignored by git

  (load (expand-file-name "init-aux.el" user-emacs-directory) 'noerror (not init-file-debug) nil 'must-suffix)

;;; Then basic benchmark util, including method imload

  (load (expand-file-name "core/bm.el" user-emacs-directory) nil (not init-file-debug) nil 'must-suffix)

;;; At last, load all the modules

  (imload 'utils)
  (imload 'cust)

  (imload 'imod-assoc)
  (imload 'imod-proxy)

  (imload 'dist)
  (imload 'quicker)
  (imload 'ui)

  (imload 'imods)
  (imload 'imod-scroll)
  (imload 'imod-safe)
  (imload 'imod-keys)
  (imload 'imod-kmacro)
  (imload 'imod-transient)
  (imload 'imod-sudo)
  (imload 'imod-tramp)
  (imload 'imod-scratch)
  (imload 'imod-session)
  (imload 'imod-window)
  (imload 'imod-edit)
  (imload 'imod-image)
  (imload 'imod-read)
  (imload 'imod-media)
  (imload 'imod-dired)
  (imload 'imod-project)
  (imload 'imod-calx)
  (imload 'imod-print)
  (imload 'imod-mmm)
  (imload 'imod-search)
  (imload 'imod-completing)
  (imload 'imod-snippet)
  (imload 'imod-folding)
  (imload 'imod-diff)
  (imload 'imod-vcs)
  (imload 'imod-shell)
  (imload 'imod-hyperbole)
  (imload 'imod-evil)
  (imload 'imod-ime)
  (imload 'imod-gpt)
  (imload 'imod-docker)
  (imload 'imod-mine)

  (imload 'iwww)
  (imload 'iwww-irc)
  (imload 'iwww-gnus)

  (imload 'iorg)
  (imload 'iorg+TeX)
  (imload 'iorg-notes)
  (imload 'iorg-denote)

  (imload 'idevs)
  (imload 'idev-treesit)
  (imload 'idev-lsp)
  (imload 'idev-dap)
  (imload 'idev-cc)
  (imload 'idev-rust)
  (imload 'idev-hexl)
  (imload 'idev-jvm)
  (imload 'idev-dotnet)
  (imload 'idev-lisp)
  (imload 'idev-python)
  (imload 'idev-ruby)
  (imload 'idev-erlang)
  (imload 'idev-elixir)
  (imload 'idev-haskell)
  (imload 'idev-go)
  (imload 'idev-php)
  (imload 'idev-sql)
  (imload 'idev-web)
  (imload 'idev-markdown)

  (imload 'imsilly)
  (imload 'implay 'load-from-paths-if-it-exists)
  (imload 'im-notes 'load-from-paths-if-it-exists)
  (imload 'im-sketch 'load-from-paths-if-it-exists)
  (imload 'over))

;;; init.el ends here
