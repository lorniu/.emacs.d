;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Code:

(defconst IS-G         (display-graphic-p))
(defconst IS-NG        (not (display-graphic-p)))
(defconst IS-LINUX-G   (and IS-LINUX (display-graphic-p)))
(defconst IS-LINUX-NG  (and IS-LINUX (not (display-graphic-p))))



(progn (imload 'dist)
       (imload 'face)

       (imload 'imod-transient)
       (imload 'imod-general)
       (imload 'imod-edit+)
       (imload 'imod-apps)
       (imload 'imod-reader)
       (imload 'imod-tramp)
       (imload 'imod-mmm)
       (imload 'imod-auth)
       (imload 'imod-dired)
       (imload 'imod-project)
       (imload 'imod-evil)
       (imload 'imod-search+)
       (imload 'imod-completing)
       (imload 'imod-snippet)
       (imload 'imod-folding)
       (imload 'imod-internet)
       (imload 'imod-vcs)
       (imload 'imod-shell)
       (imload 'imod-ime)
       (imload 'imod-print)
       (imload 'imod-eaf)
       (imload 'imod-emacs-server)

       (imload 'imod-code)
       (imload 'icod-lsp)
       (imload 'icod-tags)
       (imload 'icod-cc)
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
       (imload 'over))

;;; init.el ends here
