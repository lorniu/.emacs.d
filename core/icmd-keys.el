;;; icmd-keys.el --- Keys and Commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'bind-key)


;;; Keybinds

(global-unset-key (kbd "C-x i"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-k"))
(global-unset-key (kbd "<mouse-2>"))

(bind-keys
 ( "C-x i"         .  imtt/transient-gt                           )
 ( "C-c i"         .  imtt/transient-act                          )
 ( "C-c p"         .  imtt/transient-tpl                          )
 ( "C-c C-f"       .  imtt/transient-fold                         )
 ( "C-x C-o"       .  imtt/transient-window                       )
 ( "C-x o"         .  imtt/transient-other-window                 )

 ( "C-c a"         .  org-agenda                                  )
 ( "C-c c"         .  imtt/transient-gtd                          )
 ( "C-c g"         .  magit-status                                )
 ( "C-c m"         .  im/welcome-to-mode                          )

 ( [f2]            .  name-last-kbd-macro                         )
 ( [(control f2)]  .  name-and-insert-last-kbd-macro              )
 ( [f6]            .  toggle-truncate-lines                       )
 ( [f7]            .  quick-calc                                  )
 ( [f8]            .  calendar                                    )
 ( [f9]            .  compile                                     )
 ( [f10]           .  im/silly                                    )
 ( [f12]           .  im/popup-eshell                             )
 ( [M-f12]         .  im/popup-xshell                             )

 ( "%"             .  his-match-paren                             )
 ( "C-#"           .  cua-rectangle-mark-mode                     )
 ( "C-s"           .  im/isearch-regexp                           )
 ( "C-o"           .  im/open-line                                )
 ( "M-w"           .  im/yank-more                                )
 ( "M-q"           .  im/tiny-code                                )
 ( "M-Q"           .  im/tiny-code-buffer                         )

 ( "<C-backspace>" .  im/backward-delete-word                     )
 ( "ESC <down>"    .  im/copy-lines                               )
 ( "ESC <up>"      .  (lambda () (interactive) (im/copy-lines 1)) )
 ( "ESC <right>"   .  im/kill-lines                               )
 ( "<insertchar>"  .  undo                                        )
 ( "<select>"      .  im/toggle-dedicated                         )
 ( "C-{"           .  shrink-window                               )
 ( "C-}"           .  enlarge-window                              )

 ( "C-x f"         .  project-find-file                           )
 ( "C-x F"         .  consult-file-externally                     )
 ( "C-x d"         .  im/project-find-dir                         )
 ( "C-x C-b"       .  ibuffer                                     )
 ( "C-x b"         .  consult-buffer                              )
 ( "C-x 4 b"       .  consult-buffer-other-window                 )
 ( "C-x 5 b"       .  consult-buffer-other-frame                  )
 ( "C-x C-j"       .  ffap                                        )
 ( "C-x C-k"       .  kill-this-buffer                            )
 ( "C-x n"         .  imtt/transient-narrow                       )

 ( "C-c `"         .  im/toggle-diagnostics-buffer                )
 ( "C-c /"         .  im/search-rg+                               )

 ( "C-x C-r"       .  im/view-favorites                           )

 ( "M-g g"         .  consult-goto-line                           )
 ( "M-g M-g"       .  consult-goto-line                           )

 ( "M-s m"         .  consult-multi-occur                         )

 ( "C-,"           .  er/expand-region                            )
 ( "C-M-,"         .  er/contract-region                          )
 ( "C-."           .  imtt/transient-interact                     )

 ( "C-h h"         .  imtt/transient-help                         )
 ( "C-h C-h"       .  imtt/transient-help                         )
 ( "C-h c"         .  quick-calc                                  )
 ( "C-h C-c"       .  calc                                        )
 ( "C-h g"         .  im/toggle-gnus                              )
 ( "C-h C-g"       .  im/toggle-gnus                              )
 ( "C-h e"         .  im/toggle-eshell-buffer                     )
 ( "C-h C-e"       .  im/toggle-eshell-buffer                     )
 ( "C-h s"         .  im/toggle-scratch-or-ielm                   )
 ( "C-h C-s"       .  im/toggle-scratch-or-ielm                   )
 ( "C-h t"         .  im/open-system-terminal-here                )
 ( "C-h C-t"       .  im/open-system-terminal-here                )
 ( "C-h w"         .  im/toggle-view-messages-buffer              )
 ( "C-h C-w"       .  im/toggle-view-messages-buffer              ))


;;; Transients

(transient-define-prefix imtt/transient-gt ()
  [:hide
   (lambda () t)
   ("p  " ""              im/pages+)
   ("l  " ""              consult-line)
   ("o  " ""              consult-outline)
   ("n  " ""              consult-global-mark)
   ("r  " ""              im/view-favorites)
   ("C  " ""              iy-go-to-char-backward)
   ("SPC" ""              consult-register)
   ]
  [["Navi to"
    ("c" "Char"           iy-go-to-char)
    ("v" "Views"          im/views+)
    ("i" (lambda () (transient-my-description "i/p/l/o" ".."))   consult-imenu    :format " %d")
    ]
   ["Save Points"
    ("j" (lambda () (transient-my-description "j/ " "Register")) consult-register :format " %d")
    ("b" "  Bookmark"      consult-bookmark)
    ("m" (lambda () (transient-my-description "m/n" "Marks"))    consult-mark     :format " %d")
    ]
   ["Misc"
    ("/ " "Ripgrep"        consult-ripgrep)
    ("` " "Diagnostics"    im/toggle-diagnostics-buffer)
    ("e " "Compile Errors" consult-compile-error)
    ]
   [""
    ("f " "Ideas"          im/find-file-in-ideas-dir)
    ]
   ]
  (interactive)
  (let ((transient-show-popup -3))
    (transient-setup 'imtt/transient-gt)))

(transient-define-prefix imtt/transient-act ()
  [:hide
   (lambda () t)
   ("TAB"   "" imtt/transient-act-misc)
   ("C-h"   "" (lambda () (interactive) (message "TAB (More)")))
   ]
  [[("a  "  "emabark-act"     embark-act)
    ("b  "  "set-bookmark"    bookmark-set)
    ("SPC"  "set-register"    consult-register-store)
    ]
   [("C-y"  "Kill Ring"       consult-yank-from-kill-ring)
    ("c b"  "Buffer Name"     im/yank-current-buffer-name)
    ("c d"  "Directory Name"  im/yank-current-directory)
    ]
   [("s"    "Silly"           im/silly)
    ("t"    "Go Translate"    go-translate)
    ("g"    "Google Search"   engine/search-google)
    ]
   [("e"    "Ediff"           imtt/transient-ediff)
    ("d"    "Desktop"         imtt/transient-desktop)
    ("m"    "Macroexpand"     im/dispatch-macroexpand :if (lambda () (member major-mode '(emacs-lisp-mode lisp-interaction-mode))))
    ]
   [("i"    "-IRC-"           erc)
    ("n"    "-GNUS-"          gnus)
    ]
   ]
  (interactive)
  (let ((transient-show-popup t))
    (transient-setup 'imtt/transient-act)))

(transient-define-prefix imtt/transient-act-misc ()
  :transient-non-suffix #'transient--do-exit
  [[("c w" "im/count-words"               im/count-words)
    ("c l" "im/count-lines-in-directory"  im/count-lines-in-directory)
    ]
   [("y"   "im/youtube-dl-url"            im/youtube-dl-url)
    ("a"   "ascii-table-show"             ascii-table-show)
    ]
   [("s"   "im/screenshot-svg"            im/screenshot-svg)
    ("i"   "im/ipinfo"                    im/ipinfo)
    ]
   ]
  )

(transient-define-prefix imtt/transient-tpl ()
  [:hide
   (lambda () t)
   ("d t" ""     (lambda () (interactive) (im/insert-date t)))
   ("d i" ""     org-time-stamp-inactive)
   ]
  [[("a" "Yasnippet"    imtt/transient-yasnippet)
    ("p" "Pick Snippet" yas-insert-snippet)
    ]
   [("d d" (lambda () (transient-my-description "d d/t" (format-time-string "%F"))) im/insert-date :format "%d")
    ("d s" (lambda () (transient-my-description "d s/i" "[Timestamp]"))             org-time-stamp :format "%d")
    ]
   [("SPC" "Zero-Width Space" (lambda () (interactive) (insert "\u200b")))
    ]
   [:if-mode
    'org-mode
    ("m" "Demarcate Block"    org-babel-demarcate-block)
    ("s" "new/wrap src-block" im/org-wrap-src)
    ]
   ])

(transient-define-prefix imtt/transient-gtd ()
  [:hide
   (lambda () t)
   ("N"      "" (lambda () (interactive) (im/note-publish t)))
   ("M"      "" (lambda () (interactive) (im/note-publish-interactively t)))
   ("R"      "" im/note-publish-reset-directories)
   ("p"      "" org-timer-pause-or-continue :if-non-nil org-timer-start-time)
   ("SPC"    "" org-timer-pause-or-continue :if-non-nil org-timer-start-time)
   ("i"      "" (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer?"))
                                           (call-interactively #'org-timer-item))))
   ]
  [["Agenda"
    ("a" "Agenda"       org-agenda)
    ("f" "Agenda Files" im/org-agenda-files+)
    ]
   ["Notes"
    ("c" "Capture"      org-capture)
    ("/" "Search Notes" im/search-notes)
    ]
   ["Publish"
    ("n" (lambda () (transient-my-description "n/N" "   Publish"))   im/note-publish               :format "%d")
    ("m" (lambda () (transient-my-description "m/M/R" " Publish..")) im/note-publish-interactively :format "%d")
    ]
   [:description
    (lambda ()
      (cond (org-timer-countdown-timer
             (format "%s/%s"
                     (propertize "Countdown" 'face 'transient-heading)
                     (propertize (if (eq org-timer-countdown-timer 'paused) "Paused" "Running") 'face 'font-lock-warning-face)))
            (org-timer-start-time
             (format "%s/%s"
                     (propertize "Timer" 'face 'transient-heading)
                     (propertize "Running" 'face 'font-lock-warning-face)))
            (t "Timer/Countdown")))
    ("t"
     (lambda () (if (and org-timer-start-time (null org-timer-countdown-timer)) "  Reset" "Create Timer"))
     (lambda () (interactive) (if (or (null org-timer-start-time) (y-or-n-p "Reset the Timer ?")) (org-timer-start) (message "Do Nothing.")))
     :if (lambda () (or (null org-timer-countdown-timer)
                        (null org-timer-start-time))))
    ("d"
     (lambda () (if org-timer-countdown-timer "  Reset" "Create Countdown"))
     org-timer-set-timer
     :if (lambda () (or org-timer-countdown-timer (null org-timer-start-time))))
    ("s"
     (lambda () (transient-my-description "s/p" "Stop/Pause"))
     (lambda () (interactive) (if (y-or-n-p "Stop it ?") (org-timer-stop) (message "Do Nothing.")))
     :format "%d"
     :if-non-nil org-timer-start-time)
    ]
   ["Misc"
    (" C " "Goto Clock" org-clock-goto)
    ("r  "
     (lambda () (transient-my-description "i/r" "Record Timer"))
     (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer?"))
                                (call-interactively #'org-timer)))
     :format " %d")
    ]
   ]
  (interactive)
  (require 'ox-publish)
  (transient-setup 'imtt/transient-gtd))

(transient-define-prefix imtt/transient-interact ()
  :transient-non-suffix 'transient--do-exit
  [:hide
   (lambda () t)
   ("y"   "" go-translate-popup)
   ("C-y" "" go-translate-popup)
   ("C-t" "" go-translate)
   ("C-." "" go-translate-echo-area-and-kill-ring-save)
   ("T"   "" youdao-dictionary-search-from-input)
   ("q"   "" im/wrap-current)
   ("+"   "" im/shengciben-add)
   ("-"   "" im/shengciben-add)
   ("="   "" im/shengciben-add)
   ("C-w" "" (lambda () (interactive) (let (browse-url-default-handlers) (call-interactively #'browse-url-of-buffer))))
   ("C-h" "" (lambda () (interactive) (message "C-w (open in browser) | C-. (translate-and-kill) | q (wrap word)")))
   ]
  [[("w"  "eww"        browse-url)]
   [("t"  "Translate"  go-translate)]
   [("g"  "Google"     engine/search-google)]
   [("f"  "Wolfram"    engine/search-wolfram-alpha)]
   [("h"  "Github"     engine/search-github)]
   [("s"  "SO"         engine/search-stackoverflow)]
   [("v"  "Wikipedia"  engine/search-wikipedia)]
   [("a"  "ArchWiki"   engine/search-arch-wiki)]
   [("i"  "Iconify"    engine/search-iconify)]
   ]
  (interactive)
  (let ((transient-map nil))
    (transient-setup 'imtt/transient-interact)))

(transient-define-prefix imtt/transient-ediff ()
  "Launch Ediff in all it's variants"
  [["2 Way"
    ("b"  "Buffers"        ediff-buffers)
    ("f"  "Files"          ediff-files)
    ("d"  "Directories"    ediff-directories)
    ("c"  "Buffer vs File" ediff-current-file)
    ("~"  "File vs Backup" ediff-backup)
    ]
   ["3 Way"
    ("3b" "Buffers"        ediff-buffers3)
    ("3f" "Files"          ediff-files3)
    ("3d" "Directories"    ediff-directories3)
    ]
   ["Patches"
    ("pb" "Buffer"         ediff-patch-buffer)
    ("pf" "File"           ediff-patch-file)
    ]
   ["Regions"
    ("rl" "Linewise"       ediff-regions-linewise)
    ("rw" "Wordwise"       ediff-regions-wordwise)
    ]
   ["Windows"
    ("wl" "Linewise"       ediff-windows-linewise)
    ("ww" "Wordwise"       ediff-windows-wordwise)
    ]
   ]
  )

(transient-define-prefix imtt/transient-fold ()
  :transient-suffix 'transient--do-stay
  [:hide
   (lambda () t)
   ("s"   "" hs-show-block)
   ("h"   "" hs-hide-block)
   ("L"   "" hs-hide-level)
   ("f"   "" im/hs-toggle-current)
   ("l"   "" im/hs-toggle-level)

   ("t"   "" outline-show-subtree)
   ("d"   "" outline-hide-subtree)
   ("o"   "" outline-toggle-children)
   ("O"   "" im/outline-toggle-all)

   ("n"   "" outline-next-visible-heading)
   ("p"   "" outline-previous-visible-heading)

   ("C-f" "" forward-char)
   ("C-b" "" backward-char)
   ("C-a" "" move-beginning-of-line)
   ("C-e" "" move-end-of-line)
   ("C-n" "" next-line)
   ("C-p" "" previous-line)
   ("C-l" "" recenter-top-bottom)
   ("C-s" "" im/isearch-regexp :transient nil)
   ("C-r" "" isearch-backward  :transient nil)
   ]
  [["Hideshow"
    ("S" (lambda () (transient-my-description "S/s" "  Show"))        hs-show-all              :format "%d")
    ("H" (lambda () (transient-my-description "H/h/L" "Hide"))        hs-hide-all              :format "%d")
    ("F" (lambda () (transient-my-description "F/f/l" "Toggle"))      im/hs-toggle-all         :format "%d")
    ]
   ["Outline"
    ("k" (lambda () (transient-my-description "k/t" "  Show"))        outline-show-branches    :format "%d")
    ("Q" (lambda () (transient-my-description "Q/d" "  Hide"))        outline-hide-sublevels   :format "%d")
    ("N" (lambda () (transient-my-description "O/o/N" "Toggle"))      im/outline-toggle-narrow :format "%d")
    ]
   ["Misc"
    ("$" (lambda () (transient-my-description "  $" "  Selective"))   set-selective-display    :format "%d")
    ("#" (lambda () (transient-my-description "  #" "  Fold-This"))   fold-this                :format "%d" :transient nil)
    ("u" (lambda () (transient-my-description "u/n/p" "Outline Nav")) outline-up-heading       :format "%d")
    ]]
  )

(transient-define-prefix imtt/transient-narrow ()
  [:hide
   (lambda () t)
   ("D" "" ni-narrow-to-defun-indirect-other-window)
   ("N" "" ni-narrow-to-region-indirect-other-window)
   ("P" "" ni-narrow-to-page-indirect-other-window)
   ]
  [:if-mode
   'org-mode
   [("b" (lambda () (format "(%s) %s"
                            (propertize "org" 'face 'font-lock-warning-face)
                            (transient-my-description "b" "Narrow to Block")))
     org-narrow-to-block :format "%d")]
   [("e" "Narrow to Element" org-narrow-to-element)]
   [("s" "Narrow to Subtree" org-narrow-to-subtree)]
   [("B" "Indirect Narrow"   org-tree-to-indirect-buffer)]
   ]
  [:if-mode
   'restclient-mode
   [("c"
     (lambda () (format "(%s) %s"
                        (propertize "restclient" 'face 'font-lock-warning-face)
                        (transient-my-description "c" "Narrow to Current")))
     restclient-narrow-to-current :format "%d")]
   ]
  [[("d" (lambda () (transient-my-description "d"     "Narrow to Defun"))   narrow-to-defun  :format "%d")]
   [("n" (lambda () (transient-my-description "n"     "Narrow to Region"))  narrow-to-region :format "%d")]
   [("p" (lambda () (transient-my-description "p"     "Narrow to Page"))    narrow-to-page   :format "%d")]
   [("O" (lambda () (transient-my-description "D/N/P" "Indirect-Narrow"))   ignore           :format "%d")]
   [("w" "Widen" widen)]
   ]
  (interactive)
  (let ((transient-show-popup -0.3))
    (transient-setup 'imtt/transient-narrow)))

(transient-define-prefix imtt/transient-desktop ()
  ["Desktop"
   [("l" "Load from Dir"   (lambda () (interactive) (let ((default-directory desktop-save-default-dir)) (call-interactively 'desktop-change-dir))))
    ("s" "Save to Dir"     (lambda () (interactive) (let ((default-directory desktop-save-default-dir)) (call-interactively 'desktop-save))))
    ("d" "Save to Default" (lambda () (interactive) (desktop-save desktop-save-default-dir)))
    ]
   [("c" "Desktop Clear"   desktop-clear)
    ("r" "Desktop Revert"  desktop-revert)
    ]
   ]
  (interactive)
  (defvar desktop-save-default-dir (locc ""))
  (transient-setup 'imtt/transient-desktop))

(transient-define-prefix imtt/transient-window ()
  :transient-non-suffix #'transient--do-exit
  [:hide
   (lambda () t)
   [("-"   "Balance Win"   balance-windows)]
   [("M-p" "Win-Redo"      winner-redo :transient t)]
   ]
  [[("o"   "Ace Window"    ace-window)]
   [("s"   "Swap Window"   (lambda () (interactive) (if (> (length (window-list)) 2) (ace-swap-window) (window-swap-states))))]
   [("x"   "Switch Layout" im/change-window-split-layout)]
   [("="   (lambda () (transient-my-description "=/-" "Balance Window")) balance-windows-area :format "%d")]
   [("v"    "Views"        im/views+)]
   [("M-n" (lambda () (transient-my-description "M-n/p" "Winner")) winner-undo :format "%d" :transient t)]
   ]
  (interactive)
  (let ((transient-map nil))
    (transient-setup 'imtt/transient-window)))

(transient-define-prefix imtt/transient-other-window ()
  :transient-non-suffix #'transient--do-exit
  [[("o" "Other Window" ip/other-window- :transient t)]
   [("w" "Ace Window"   ace-window)]
   ]
  (interactive)
  (require 'super-window)
  (call-interactively 'ip/other-window-)
  (let (transient-map transient-show-common-commands-p)
    (transient-setup 'imtt/transient-other-window nil t)))

(transient-define-prefix imtt/transient-yasnippet ()
  [[("n" "new-snippet"           yas-new-snippet)
    ("f" "open-snippet"          yas-visit-snippet-file)
    ]
   [("e" "activate-extra-mode"   yas-activate-extra-mode)
    ("c" "clear-all-extra-modes" im/yas--clear-extra-mode)
    ]
   [("i" "pick-snippet"          yas-insert-snippet)
    ("l" "view-yas-tables"       (lambda ()
                                   (interactive)
                                   (progn (yas-describe-tables)
                                          (pop-to-buffer "*YASnippet Tables*"))))
    ]
   [("a" "reload-all"            yas-reload-all)
    ("d" "load-from-directory"   yas-load-directory)
    ]
   ])

(transient-define-prefix imtt/transient-help ()
  "Help commands that I use. A subset of C-h with others thrown in."
  [["Describe"
    ("f"    "Function"        describe-function)
    ("v"    "Variable"        describe-variable)
    ("k"    "Key"             describe-key)
    ("c"    "Key Briefly"     describe-key-briefly)
    ("s"    "Symbol"          describe-symbol)
    ("C-f"  "Face"            describe-face)
    ("="    "Position"        what-cursor-position)
    ]
   ["Info Manual"
    ("C-c"  "Emacs Command"   Info-goto-emacs-command-node)
    ("C-f"  "Function"        describe-function)
    ("C-v"  "Variable"        describe-variable)
    ("C-k"  "Emacs Key"       Info-goto-emacs-key-command-node)
    ("C-i"  "Info"            info)
    ("C-4"  "Other Window "   info-other-window)
    ("C-e"  "Emacs"           info-emacs-manual)
    ]
   ["Internals"
    ("I  "  "Input Method"    describe-input-method)
    ("G  "  "Language Env"    describe-language-environment)
    ("S  "  "Syntax"          describe-syntax)
    ("O  "  "Coding System"   describe-coding-system)
    ("C-o"  "Coding Brief"    describe-current-coding-system-briefly)
    ("T  "  "Display Table"   describe-current-display-table)
    ("l  "  "Lossage"         view-lossage)
    ]
   ["Mode & Bindings"
    ("m"    "Mode"            describe-mode)
    ("d"    "Descbinds"       describe-bindings)
    ("b"    "Major Bindings"  which-key-show-full-major-mode)
    ("B"    "Minor Bindings"  which-key-show-full-minor-mode-keymap)
    ("t"    "Top Bindings  "  which-key-show-top-level)
    ]
   ]
  )


;;; Transients / Mode specific

(transient-define-prefix imtt/transient-org-mode ()
  [:hide
   (lambda () t)
   ("s"    ""   org-schedule)
   ("v T"  ""   (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'org-babel-tangle))))
   ("v f"  ""   org-babel-tangle-file)
   ("v C"  ""   org-babel-tangle-clean)
   ("v D"  ""   org-babel-detangle)
   ]
  [["Schedule/Timeit"
    ("d" (lambda () (transient-my-description "s/d" "schedule/deadline")) org-deadline :format " %d")
    ""
    ("c i"   "Clock In"               org-clock-in     :if-not (lambda () (org-clocking-p)))
    ("c o"   "Clock Out"              org-clock-out    :if (lambda () (org-clocking-p)))
    ("c u"   "Clock Cancel"           org-clock-cancel :if (lambda () (org-clocking-p)))
    ("c d"   "Clock Display"          org-clock-display)
    ("c r"   "Clock Report"           org-clock-report)
    ("c e"   "Effort Estimate"        org-clock-modify-effort-estimate)
    ("C-y"   "Evaluate Timespan "     org-evaluate-time-range)
    ]
   ["Prop/Drawer"
    ("t  "   "Org Todo"               org-todo)
    ("q  "   "Org Tag"                org-set-tags-command)
    (",  "   "Org Priority"           org-priority)
    ("x p"   "Set Property"           org-set-property)
    ("x x"   "Insert Dblock"          org-dynamic-block-insert-dblock)
    ("x a"   "Toggle Archive Tag"     org-toggle-archive-tag)
    ]
   ["Refactor"
    ("C-w"   "Org Refile"             org-refile)
    ("r W"   "Org Copy"               org-copy)
    ("r $"   "Archive Subtree"        org-archive-subtree)
    ""
    ("l s"   "Store Link"             org-store-link)
    ("l i"   "Insert Link"            org-insert-link)
    ("l o"   "Open Current"           org-open-at-point)
    ]
   ["View"
    ("v c"   "Org Columns"            org-columns)
    ("v ;"   "Toggle Comment"         org-toggle-comment)
    ("v :"   "Toggle Fixed-Width"     org-toggle-fixed-width)
    ("v l"   "Toggle Link-Display"    org-toggle-link-display)
    ("v v"   "Toggle Inline-Images"   org-toggle-inline-images)
    ("v \\"  "Toggle pretty-entities" org-toggle-pretty-entities)
    ]
   ["Misc"
    ("C-z"   "Add Note"               org-add-note)
    ("C-a"   "Add Attach"             org-attach)
    ("\\"    "Org Sparse Tree"        org-sparse-tree)
    ""
    ("m"   "Demarcate Block"          org-babel-demarcate-block)
    ("g"   "Goto Named Block"         org-babel-goto-named-src-block)
    ("v t"
     (lambda () (transient-my-description "v t/T/f/C/D" "Tangle"))
     org-babel-tangle :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'org-mode)
      (transient-setup 'imtt/transient-org-mode)
    (user-error "Sorry, but this is not org-mode")))

(transient-define-prefix imtt/transient-hexl-mode ()
  [:hide
   (lambda () t)
   ("x"     "" hexl-insert-hex-char)
   ("o"     "" hexl-insert-octal-char)
   ("j"     "" hexl-goto-address)
   ("g"     "" hexl-goto-hex-address)
   ("C-x ]" "" hexl-end-of-1k-page)
   ("C-M-e" "" hexl-end-of-512b-page)
   ]
  [[("C-M-d"   "Insert decimal char" hexl-insert-decimal-char)
    ("C-M-x"   "Insert hex char"     hexl-insert-hex-char)
    ("C-M-o"   "Insert octal char"   hexl-insert-octal-char)
    ]
   [("C-M-a  " "To 512b-page"        hexl-beginning-of-512b-page)
    ("C-x [  " "To 1k-page"          hexl-beginning-of-1k-page)
    ("C-c C-c" "Exit hexl mode"      hexl-mode-exit)
    ]
   [("M-j"     "To address"          hexl-goto-address)
    ("M-g"     "To hex address"      hexl-goto-hex-address)
    ("d" (lambda () (transient-my-description "d/x/o j/g" "Shortcuts")) hexl-insert-decimal-char :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'hexl-mode)
      (transient-setup 'imtt/transient-hexl-mode)
    (user-error "You should invoke this in hexl-mode.")))

(transient-define-prefix imtt/transient-dired ()
  :transient-non-suffix 'transient--do-exit
  [["View"
    ("o  " "Open (other window)"  dired-find-file-other-window)
    ("C-o" "Open (other no select)" dired-display-file)
    ("v  " "Open (view mode)"     dired-view-file)
    ("W  " "Open (externally)"    browse-url-of-dired-file)
    ("i  " "Insert Subdir"        dired-maybe-insert-subdir)
    ("j  " "Goto file"            dired-goto-file)
    ]
   ["Attribute"
    ("G "  "Chg Group"            dired-do-chgrp)
    ("M "  "Chg Mode"             dired-do-chmod)
    ("O "  "Chg Owner"            dired-do-chown)
    ("T "  "Chg Timestamp"        dired-do-touch)
    ("y "  "Show file type"       dired-show-file-type)
    ]
   ["Display"
    ("g  " "Refresh buffer"       revert-buffer)
    ("l  " "Refresh file"         dired-do-redisplay)
    ("(  " "Toggle detail info"   dired-hide-details-mode)
    ("$  " "Hide subdir"          dired-hide-subdir)
    ("M-$" "Hide subdir all"      dired-hide-subdir)
    ]
   ["Misc"
    ("s "  "Sort"                 imtt/transient-dired-sort)
    ("= "  "Diff"                 dired-diff)
    ("e "  "wdired"               wdired-change-to-wdired-mode)
    ("w "  "Copy filename"        dired-copy-filename-as-kill)
    (", "  (lambda () (concat "Collapse Mode " (if dired-collapse-mode "Off" "On"))) dired-collapse-mode)
    ]
   ]
  [["Action"
    ("D "  "Delete"               dired-do-delete)
    ("R "  "Rename"               dired-do-rename)
    ("C "  "Copy"                 dired-do-copy)
    ("+ "  "New Dir"              dired-create-directory)
    ("S "  "Symlink"              dired-do-symlink)
    ("H "  "Hardlink"             dired-do-hardlink)
    ("k "  "Kill Lines"           dired-do-kill-lines)
    ]
   [""
    ("L"   "Elisp Load"           dired-do-load)
    ("B"   "Elisp Bytecompile"    dired-do-byte-compile)
    ("!"   "Shell Command"        dired-do-shell-command)
    ("X"   "Shell Command"        dired-do-shell-command)
    ("&"   "Async Shell Command"  dired-do-async-shell-command)
    ("Z"   "Compress"             dired-do-compress)
    ("z"   "Compress to"          dired-do-compress-to)
    ]
   ["Mark"
    ("m "  "Mark"                 dired-mark)
    ("u "  "Unmark"               dired-unmark)
    ("U "  "Unmark all"           dired-unmark-all-marks)
    ("t "  "Toggle mark"          dired-toggle-marks)
    ("**"  "Executables"          dired-mark-executables)
    ("*/"  "Directories"          dired-mark-directories)
    ("*@"  "Symlinks"             dired-mark-symlinks)
    ]
   ["Flag"
    ("d "  "flag/deletion"        dired-flag-file-deletion)
    ("~ "  "flag/backup"          dired-flag-backup-files)
    ("# "  "flag/auto-save"       dired-flag-auto-save-files)
    ("%&"  "flag/garbages"        dired-flag-garbage-files)
    ""
    ("x "  "flagged Delete"       dired-do-flagged-delete)
    ]
   ["Go"
    ("A "  "Find"                 dired-do-find-regexp)
    ("Q "  "Replace"              dired-do-find-regexp-and-replace)
    ("Y "  "Rsync To"             idp/dired-rsync)
    ""
    ("%m"  "Regexp"               dired-mark-files-regexp)
    ("%g"  "Regexp file contents" dired-mark-files-containing-regexp)
    ]
   ]
  (interactive)
  (if (eq major-mode 'dired-mode)
      (progn
        (transient-setup 'imtt/transient-dired)
        (message "[C-t] for image-display, [:] for epa-actions"))
    (user-error "Should be used in Dired.")))

(transient-define-prefix imtt/transient-dired-sort ()
  [[("n" "By Name" (lambda () (interactive) (dired-quick-sort :name)) :transient t)
    ("t" "By Time" (lambda () (interactive) (dired-quick-sort :time)) :transient t)
    ]
   [("s" "By Size" (lambda () (interactive) (dired-quick-sort :size)) :transient t)
    ("e" "By Ext"  (lambda () (interactive) (dired-quick-sort :ext))  :transient t)
    ]
   [("r"
     "Revert dired buffer"
     (lambda () (interactive) (ignore-error (find-alternate-file default-directory)))
     :transient t)
    ("g"
     (lambda () (format "Group Dirs [%s]" (if (plusp dired-quick-sort-group-dir) "X" " ")))
     (lambda () (interactive) (dired-quick-sort nil (* -1 dired-quick-sort-group-dir)))
     :transient t
     )
    ]
   ]
  (interactive)
  (if (eq major-mode 'dired-mode)
      (transient-setup 'imtt/transient-dired-sort)
    (user-error "Should be used in Dired.")))

(transient-define-prefix imtt/transient-erc-mode ()
  [[("-t" (lambda ()
            (if erc-hide-timestamps
                (propertize "Timestamp" 'face 'font-lock-doc-face)
              "Timestamp"))
     erc-toggle-timestamps)
    ]
   [("-o" (lambda ()
            (if erc-hide-list
                (propertize "Sys Msg" 'face 'font-lock-doc-face)
              "Sys Msg"))
     erc-toggle-hide-list)
    ]
   [("o" "Occur.." erc-occur)
    ]
   [("s" "Save Buffer in Logs" erc-save-buffer-in-logs)
    ]
   [("m" "Join.." erc-join-my-groups)
    ]
   ]
  (interactive)
  (if (eq major-mode 'erc-mode)
      (transient-setup 'imtt/transient-erc-mode)
    (user-error "You should invoke this in erc-mode.")))


;;; Hack or Enhanced

(defun im/yank-more ()
  "Copy-Current-Line-Or-Region."
  (interactive)
  (cond ((use-region-p)
         (call-interactively 'kill-ring-save)
         (message "Region Yanked."))
        ((and (eq major-mode 'org-mode)
              (char-equal (char-after (line-beginning-position)) ?#)
              (equal (org-element-type (org-element-at-point)) 'src-block))
         (save-mark-and-excursion
           (org-edit-special)
           (copy-region-as-kill (point-min) (point-max))
           (org-edit-src-exit)
           (message "Src-Block Yanked.")))
        (t
         (copy-region-as-kill (line-beginning-position) (line-end-position))
         (message "No Region, Whole Line Yanked."))))

(defun im/open-line ()
  (interactive)
  (cond
   ((equal major-mode 'org-mode)
    (call-interactively 'org-return))
   ((looking-at-p "[ \t]*$")
    (newline-and-indent))
   ((looking-back "^[ \t]*" nil)
    (beginning-of-line)
    (open-line 1)
    (indent-for-tab-command))
   (t
    (newline-and-indent)
    (newline-and-indent)
    (forward-line -1)
    (indent-for-tab-command))))

(defun im/backward-delete-word ()
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(defun im/isearch-regexp ()
  "Isearch+, default with region word, enable regexp."
  (interactive)
  (if (use-region-p)
      (progn
        (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (isearch-resume string nil nil t string nil)))
    (call-interactively 'isearch-forward-regexp)))

(defun im/toggle-view-messages-buffer ()
  "Toggle show the *Messages* buffer."
  (interactive)
  (let ((name "*Messages*")
        (ws (window-list)))
    (if (cl-find name (mapcar (lambda (w) (buffer-name (window-buffer w))) ws) :test 'string-equal)
        (save-excursion
          (dolist (w ws)
            (if (string-equal (buffer-name (window-buffer w)) name)
                (ignore-errors (delete-window w)))))
      (call-interactively 'view-echo-area-messages))))

(defun im/toggle-scratch-or-ielm (&optional ielmp)
  "Toggle show the *Scratch* buffer or ielm."
  (interactive "P")
  (let ((name (if ielmp "*ielm*" "*scratch*"))
        (ws (window-list)))
    (cond ((member (buffer-name) '("*ielm*" "*scratch*"))
           (ignore-errors (delete-window)))
          ((cl-find name (mapcar (lambda (w) (buffer-name (window-buffer w))) ws) :test 'string-equal)
           (save-excursion
             (dolist (w ws)
               (if (string-equal (buffer-name (window-buffer w)) name)
                   (ignore-errors (delete-window w))))))
          (t (let ((display-buffer-alist '(("*" (display-buffer-reuse-window display-buffer-at-bottom)))))
               (if ielmp (ielm)
                 (resume-scratch)))))))

(defun im/toggle-gnus ()
  "Toggle show the *Group* buffer."
  (interactive)
  (let ((buf (get-buffer "*Group*"))
        (ws (window-list)))
    (cond ((null buf)
           (user-error "You should start GNUS first."))
          ((cl-find buf (mapcar #'window-buffer ws))
           (save-excursion
             (dolist (w ws)
               (if (equal buf (window-buffer w))
                   (ignore-errors (delete-window w))))))
          (t (let ((display-buffer-alist '(("*" (display-buffer-reuse-window display-buffer-in-direction)))))
               (display-buffer buf))))))

(defun im/toggle-eshell-buffer ()
  "Toggle show the *EShell* buffer."
  (interactive)
  (let ((name "*eshell*")
        (ws (window-list)))
    (if (cl-find name (mapcar (lambda (w) (buffer-name (window-buffer w))) ws) :test 'string-equal)
        (save-excursion
          (dolist (w ws)
            (if (string-equal (buffer-name (window-buffer w)) name)
                (ignore-errors (delete-window w)))))
      (let ((display-buffer-alist '(("*" (display-buffer-reuse-window display-buffer-at-bottom)))))
        (call-interactively 'eshell)))))

(defun im/toggle-diagnostics-buffer()
  "Show the error messages for flycheck or flymake."
  (interactive)
  (cond ((and (boundp 'flycheck-mode) flycheck-mode)
         (let ((buf flycheck-error-list-buffer))
           (call-interactively 'flycheck-list-errors)
           (select-window (get-buffer-window buf))))
        ((and (boundp 'flymake-mode) flymake-mode)
         (let ((buf (flymake--diagnostics-buffer-name)))
           (call-interactively 'flymake-show-diagnostics-buffer)
           (select-window (get-buffer-window buf))))
        (t (message "Nothing to do, check flycheck or flymake toggled?"))))

(defun open-it-externally (&optional @fname)
  "Open file in OS way."
  (interactive)
  (let (($file-list
         (if @fname (list @fname)
           (cond ((string-equal major-mode "dired-mode")
                  (dired-get-marked-files))
                 ((string-equal major-mode "eshell-mode")
                  (list (eshell/pwd)))
                 ((string-equal major-mode "shell-mode")
                  (list default-directory))
                 (t (list (or (buffer-file-name)
                              (if (y-or-n-p "No location, goto default dir or QUIT?" )
                                  default-directory
                                (message "Do Nothing.")))))))))
    (when (or (<= (length $file-list) 5) (y-or-n-p "Open more than 5 files? "))
      (cond
       (IS-WIN
        (mapc (lambda ($fpath) (w32-shell-execute "open" $fpath)) $file-list))
       (IS-MAC
        (mapc (lambda ($fpath) (shell-command (concat "open " (shell-quote-argument $fpath))))  $file-list))
       (IS-G
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $file-list))))))


;;; Commands

(defun im/view-url-cursor ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url) 'norecord)
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (1+ (point)))
    (search-forward "><")
    (replace-match ">\n<")
    (delete-blank-lines)
    (set-auto-mode)))

(defun im/youtube-dl-url (&optional url)
  "Run 'youtube-dl' over the URL. If URL is nil, use URL at point."
  (interactive (list
                (let ((up (thing-at-point-url-at-point)))
                  (or up (read-string "youtube url: ")))))
  (if (zerop (length url))
      (user-error "URL not found.")
    (let ((eshell-buffer-name "*youtube-dl*")
          (directory (seq-find (lambda (dir)
                                 (and (file-directory-p dir) (expand-file-name dir)))
                               '("~/temp") ".")))
      (eshell)
      (when (eshell-interactive-process) (eshell t))
      (eshell-interrupt-process)
      (insert (format " cd '%s' && youtube-dl " directory) url)
      (eshell-send-input))))

(defun grep-cursor (word)
  "Grep the current WORD in the files."
  (interactive (list (im/thing-at-region-or-point)))
  (if (or (not word) (< (length word) 3))
      (message "word not available")
    (let* ((oldcmd grep-find-command)
           (ext (aif (file-name-extension (buffer-name))
                  (concat "." it) ""))
           (newcmd (format "find . -maxdepth 1 -type f -name '*%s' -exec grep -nH -e '%s' {} + " ext word)))
      (unwind-protect
          (progn
            (grep-apply-setting 'grep-find-command newcmd)
            (call-interactively 'grep-find))
        (grep-apply-setting 'grep-find-command oldcmd)))))

(defun im/tiny-code ()
  "Indent codes according mode."
  (interactive)
  (cond ((and (eq major-mode 'org-mode) ; org-src-block
              (eq (org-element-type (org-element-context)) 'src-block))
         (org-edit-special)
         (save-mark-and-excursion
           (indent-region (point-min) (point-max)))
         (org-edit-src-exit))
        (t (let (beg end whole)
             (cond ((use-region-p) ; region
                    (setq beg (region-beginning)
                          end (region-end)))
                   (current-prefix-arg ; whole buffer
                    (setq beg (point-min)
                          end (point-max)
                          whole t))
                   ((string-match-p ")\\|}" (char-to-string (preceding-char))) ; sexp end
                    (save-excursion
                      (setq end (point))
                      (backward-sexp 1)
                      (setq beg (point)))))
             (cond ((eq major-mode 'lsp-mode) ; lsp-mode
                    (cond (whole (lsp-format-buffer))
                          ((and beg end) (lsp-format-region beg end))
                          (t (call-interactively #'lsp-format-region))))
                   (t (if (and beg end) ; default
                          (indent-region beg end)
                        (indent-according-to-mode))))))))

(defun im/tiny-code-buffer ()
  "Indent codes according mode for whole buffer."
  (interactive)
  (save-excursion
    (call-interactively 'mark-whole-buffer)
    (im/tiny-code)))

(defun im/go-to-char (&optional backwardp)
  "Jump to the next CHAR, like `f' in vim, when backwardp is t, then search backward."
  (interactive)
  (let (char n p)
    (setq p (point))
    (push-mark p)
    (setq char (read-char "Go to char: ") n 1)
    (while (or (= char 20)
               (and (> char 31) (< char 127))
               (and (> char 134217776) (< char 134217786)))
      (condition-case nil
          (cond ((= char 20) (message "Turn back: ") (setq backwardp (not backwardp) n 1))
                ((> char 134217776) (setq n (- char 134217776)))
                (t (if backwardp
                       (search-backward (string char) nil nil n)
                     (search-forward (string char) nil nil n))
                   (setq n 1)))
        (error (message "No more...")))
      (setq char (read-char)))
    (cond ((= char 13) (message "Reached."))
          ((= char 23) (kill-region p (point)) (message "Killed to ring."))
          ((= char 134217847) (copy-region-as-kill p (point)) (message "Saved to ring."))
          (t (setq unread-command-events (list last-input-event))))))

(defun his-match-paren (n)
  "Like `%' in vim. N is self-insert times."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        ((or (derived-mode-p 'web-mode)
             (derived-mode-p 'sgml-mode))
         (require 'sgml-mode)
         (cond ((looking-at "<")
                (sgml-skip-tag-forward 1))
               ((looking-back ">")
                (sgml-skip-tag-backward 1))
               (t (self-insert-command (or n 1)))))
        (t
         (self-insert-command (or n 1)))))

(defun ascii-table-show ()
  "Print ASCII table."
  (interactive)
  (with-current-buffer
      (switch-to-buffer "*ASCII table*" 'norecord)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i 0))
      (insert (propertize
               "                         [ASCII table]\n\n"
               'face font-lock-comment-face))
      (while (< i 32)
        (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
          (insert (concat
                   (propertize (format "%3d " tmp)
                               'face font-lock-function-name-face)
                   (propertize (format "[%2x]" tmp)
                               'face font-lock-constant-face)
                   " "
                   (propertize (format "%3s" (single-key-description tmp))
                               'face font-lock-string-face)
                   (unless (= tmp (+ 96 i))
                     (propertize "  |  " 'face font-lock-variable-name-face)))))
        (newline)
        (setq i (+ i 1)))
      (goto-char (point-min)))
    (local-set-key "q" 'bury-buffer)
    (local-set-key "Q" 'kill-this-buffer)
    (read-only-mode 1)))

(defun resume-scratch ()
  "This sends you to the *Scratch* buffer."
  (interactive)
  (let ((name "*scratch*"))
    (unless (get-buffer name)
      (progn
        (get-buffer-create name)
        (with-current-buffer name
          (insert ";; scratch buffer\n\n")
          (funcall initial-major-mode))))
    (pop-to-buffer name '(display-buffer-pop-up-window) t)))

(defun im/copy-lines (&optional direction)
  "Copy lines down/up, like in Eclipse."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (goto-char (setq end (line-end-position)))
        (kill-ring-save beg end)
        (newline)
        (yank)
        (if direction (goto-char end)))
    (kill-ring-save (line-beginning-position) (line-end-position))
    (end-of-line)
    (newline)
    (yank)
    (forward-line -1)))

(defun im/kill-lines ()
  "Fast move/del like in eclipse."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (goto-char (setq end (line-end-position)))
        (kill-region beg end))
    (kill-whole-line)))

(defun im/clear-comment ()
  "Delete all comments in the buffer."
  (interactive)
  (let (pmin pmax lines kill-ring)
    (if (use-region-p)
        (setq pmin (region-beginning) pmax (region-end))
      (setq pmin (point-min) pmax (point-max)))
    (save-excursion
      (setq lines (count-lines pmin pmax))
      (when lines (goto-char pmin) (comment-kill lines)))))

(defun im/toggle-dedicated ()
  "Whether the current active window is dedicated."
  (interactive)
  (face-remap-add-relative
   'mode-line-buffer-id
   (if (let ((window (get-buffer-window (current-buffer))))
         (set-window-dedicated-p
          window
          (not (window-dedicated-p window))))
       '(:foreground "red")
     '(:foreground "black")))
  (current-buffer))

(defun im/wrap-current (chars)
  "Wrap current word with some CHARS, x_y format."
  (interactive (list
                (read-string (format "Wrap %s with%s: "
                                     (if (use-region-p) "region"
                                       (propertize (or (word-at-point) (user-error "No region or no word at point."))
                                                   'face 'font-lock-warning-face))
                                     (if my-wrap-current (format " (%s)" (car my-wrap-current)) ""))
                             nil 'my-wrap-current (car my-wrap-current))))
  (save-excursion
    (let* ((fill (split-string chars " +"))
           (left (car fill))
           (right (or (cadr fill)
                      (pcase left
                        ("(" ")")
                        ("[" "]")
                        ("{" "}")
                        ("<" ">")
                        (_ left))))
           beg end)
      (if (use-region-p)
          (setq beg (region-beginning) end (region-end))
        (if (looking-at-p "\\sw") (forward-word))
        (setq end (point))
        (backward-word) (setq beg (point)))
      (goto-char end) (insert right)
      (goto-char beg) (insert left)
      (deactivate-mark))))

(defun im/count-words (beg end)
  "Count words in marked region(BEG to END)."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end)
          total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Count Result: %d words(cn: %d, en: %d), %d bytes."
                     total-word cn-word en-word total-byte))))

(defun im/count-lines-in-directory ()
  (interactive)
  (let* ((cs "find . -path ./.git -prune -o -type f -execdir cat {} \\; | wc -l")
         (cmd (read-string "Command: " cs nil cs)))
    (async-shell-command
     (concat "echo `pwd`; echo; " cmd))))

(defun im/insert-date (&optional timep)
  (interactive "P")
  (insert (format-time-string (if timep "%F %T" "%F"))))

(defun im/yank-current-directory ()
  (interactive)
  (if-let ((d default-directory))
      (progn
        (kill-new d)
        (message "Yanked: %s" d))
    (user-error "Nothing to copy.")))

(defun im/yank-current-buffer-name ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffname (buffer-name)))
    (if filename (kill-new filename))
    (kill-new buffname)
    (message "Yanked: %s" buffname)))

(defun im/set-lisp-indent-function-buffer-local ()
  "Toggle the current indent-function."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (user-error "Should be invoked in emacs-lisp-mode."))
  (cl-loop for item in '("common-lisp-indent-function" "lisp-indent-function")
           unless (string= item (symbol-name lisp-indent-function))
           collect item into cs
           finally do
           (let ((exp (completing-read "Change to: " cs nil t)))
             (setq-local lisp-indent-function (intern exp))
             (message "Changed to `%s' locally." lisp-indent-function))))

(defun trailing-whitespace-mode ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show-Trailing-Whitespace: %S" show-trailing-whitespace))

(defun im/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun im/ipinfo (ip)
  "Return ip info from ipinfo.io for IP."
  (interactive "sEnter IP to query (blank for own IP): ")
  (require 'request)
  (request
   (concat "https://ipinfo.io/" ip)
   :headers '(("User-Agent" . "Emacs ipinfo.io Client")
              ("Accept" . "application/json")
              ("Content-Type" . "application/json;charset=utf-8"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message
                (mapconcat
                 (lambda (e)
                   (format "%10s: %s" (capitalize (symbol-name (car e))) (cdr e)))
                 data "\n"))))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Can't receive ipinfo. Error %S " error-thrown)))))

(defun im/dispatch-macroexpand ()
  "Expand macro for emacs lisp."
  (interactive)
  (if (and (member major-mode '(emacs-lisp-mode lisp-interaction-mode ))
           (listp (pp-last-sexp)))
      (call-interactively #'pp-macroexpand-last-sexp)
    (call-interactively #'pp-macroexpand-expression))
  (switch-to-buffer-other-window "*Pp Macroexpand Output*")
  (use-local-map
   (let ((map (make-sparse-keymap)))
     (define-key map "q" #'delete-window)
     map)))

(defun im/change-window-split-layout ()
  "Switch between vertical and horizontal split. It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun im/shengciben-add (word)
  (interactive (list
                (read-string "Save to shengciben: "
                             (im/thing-at-region-or-point))))
  (deactivate-mark)
  (when (zerop (length word))
    (user-error "Word is empty, nothing done."))
  (save-window-excursion
    (let* ((f-default ic/shengciben)
           (f (if (file-exists-p f-default) f-default (locc "words.org"))))
      (find-file f)
      (goto-char (point-max))
      (skip-chars-backward " \n")
      (insert (format "\n- %-35s (%s)" word (time-str)))
      (save-buffer)))
  (message "[%s] saved." (propertize word 'face 'font-lock-string-face)))


(provide 'icmd-keys)

;;; icmd-keys.el ends here
