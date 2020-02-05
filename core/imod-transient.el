;;; imod-transient.el --- Transients -*- lexical-binding: t -*-

;;; Code:

(x transient
   :ref "magit/transient"
   :init
   (require 'transient)

   ;; Imenu Support
   (with-eval-after-load 'lisp-mode
     (add-to-list 'lisp-imenu-generic-expression
                  (list "Functions"
                        "^\\s-*(\\(transient-define-\\(?:\\(?:in\\|pre\\|suf\\)fix\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
                        2)))

   ;; Common Commands
   (defvar transient-show-common-commands-p t)
   (setq transient--common-command-prefixes (list (kbd "<f1>")))
   (defun:override transient--init-suffixes$ (name)
     (let ((levels (alist-get name transient-levels)))
       (cl-mapcan (lambda (c) (transient--init-child levels c))
                  (append (get name 'transient--layout)
                          (and (not transient--editp)
                               transient-show-common-commands-p
                               (get 'transient-common-commands
                                    'transient--layout))))))

   ;; Display Cases
   (defun:around transient--redisplay$short (f &rest args)
     (cond
      ((string-match-p "other-window" (format "%s" this-command))
       (message "C-x o-"))
      (t (apply f args))))

   ;;Helper
   (defmacro transient-my-bind-service (mode)
     "Bind transient to `C-c m' for MODE."
     (let ((map (intern (concat (symbol-name mode) "-map")))
           (trans (intern (concat "imtt/transient-" (symbol-name mode)))))
       `(define-key ,map (kbd "C-c m") ',trans)))

   (defun !tdesc (key desc)
     "Generate string used by `transient-define-pre'."
     (concat (propertize key 'face 'transient-key) " " desc)))

(defun im/welcome-to-mode ()
  (interactive)
  (let ((tn (intern (format "imtt/transient-%s" major-mode))))
    (if (fboundp tn) (funcall tn)
      (message
       "Current mode is %s, happy to service you."
       (propertize (format "%s" major-mode) 'face 'font-lock-warning-face)))))



(transient-define-prefix imtt/transient-goto () ; C-x i
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
  [["Goto:"
    ("f" "Char"           iy-go-to-char)
    ("v" "Views"          im/views+)
    ("i" (lambda () (!tdesc "i/p/l/o" ".."))   consult-imenu    :format " %d")
    ]
   [""
    ("b" "  Bookmark" consult-bookmark)
    ("m" (lambda () (!tdesc "m/n" "Marks"))    consult-mark     :format " %d")
    ("j" (lambda () (!tdesc "j/ " "Register")) consult-register :format " %d")
    ]
   [""
    ("/ " "Ripgrep"        consult-ripgrep)
    ("` " "Diagnostics"    im/toggle-diagnostics-buffer)
    ("e " "Compile Errors" consult-compile-error)
    ]
   ]
  (interactive)
  (let ((transient-show-popup -1.5))
    (transient-setup 'imtt/transient-goto)))

(transient-define-prefix imtt/transient-act () ; C-c i
  [:hide
   (lambda () t)
   ("TAB"   "" imtt/transient-act-misc)
   ("C-h"   "" (lambda () (interactive) (message "TAB (More)")))
   ]
  [["Action (TAB for more):"
    ("a  "  "emabark-act"     embark-act)
    ("b  "  "set-bookmark"    bookmark-set)
    ("SPC"  "set-register"    consult-register-store)
    ]
   [""
    ("C-y"  "Kill Ring"       consult-yank-from-kill-ring)
    ("w b"  "Buffer Name"     im/yank-current-buffer-name)
    ("w d"  "Directory Name"  im/yank-current-directory)
    ]
   [""
    ("e"    "Ediff"           imtt/transient-ediff)
    ("d"    "Desktop"         imtt/transient-desktop)
    ("c"    "Calendar"        calendar)
    ]
   [""
    ("D"    "Docker"          docker)
    ("t"    "Translate"       gts-do-translate)
    ]
   [""
    ("i"    "-IRC-"           erc)
    ("n"    "-GNUS-"          gnus)
    ]
   ]
  (interactive)
  (let ((transient-show-popup t))
    (transient-setup 'imtt/transient-act)))

(transient-define-prefix imtt/transient-act-misc () ; C-c i TAB
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
   ])

(transient-define-prefix imtt/transient-tpl () ; C-c p
  [:hide
   (lambda () t)
   ("d t" "1"     (lambda () (interactive) (im/insert-date t)))
   ("d i" "2"     org-time-stamp-inactive)
   ("F"   "3"     (lambda () (interactive) (let ((current-prefix-arg 4)) (call-interactively 'figlet))))
   ]
  [[("a" "Yasnippet"    imtt/transient-yasnippet)
    ("p" "Pick Snippet" yas-insert-snippet)
    ]
   [("d d" (lambda () (!tdesc "d d/t" (format-time-string "%F"))) im/insert-date :format "%d")
    ("d s" (lambda () (!tdesc "d s/i" "[Timestamp]"))             org-time-stamp :format "%d")
    ]
   [("f  " (lambda () (!tdesc "f/F" "FIGlet")) figlet :format " %d")
    ("SPC" "Zero-Width Space" (lambda () (interactive) (insert "\u200b")))
    ]
   [:if-mode
    'org-mode
    ("m" "Demarcate Block"    org-babel-demarcate-block)
    ("s" "new/wrap src-block" im/org-wrap-src)
    ]
   ])

(transient-define-prefix imtt/transient-gtd () ; C-c c
  [:hide
   (lambda () t)
   ("N"   "N"  (lambda () (interactive) (im/note-publish t)))
   ("M"   "M"  (lambda () (interactive) (im/note-publish-interactively t)))
   ("R"   ""   im/note-publish-reset-directories)
   ("p"   ""   org-timer-pause-or-continue :if-non-nil org-timer-start-time)
   ("SPC" ""   org-timer-pause-or-continue :if-non-nil org-timer-start-time)
   ("i"   "i"  (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer??")) (call-interactively #'org-timer-item))))
   (","   ""   citre-ace-peek)
   ("U"   ""   citre-update-this-tags-file)
   ]
  [["Agenda"   ("a" "Agenda" org-agenda)   ("f" "Agenda Files" im/org-agenda-files+)]
   ["Notes"    ("c" "Capture" org-capture) ("/" "Search Notes" im/search-notes)     ]
   ["Publish"
    ("n" (lambda () (!tdesc "nN" "   Pub"))   im/note-publish                :format "%d")
    ("m" (lambda () (!tdesc "mMR" " Pub...")) im/note-publish-interactively :format "%d")]
   [:description
    (lambda () (cond (org-timer-countdown-timer (format "%s/%s" (propertize "Countdown" 'face 'transient-heading) (propertize (if (eq org-timer-countdown-timer 'paused) "Paused" "Running") 'face 'font-lock-warning-face)))
                     (org-timer-start-time (format "%s/%s" (propertize "Timer" 'face 'transient-heading) (propertize "Running" 'face 'font-lock-warning-face)))
                     (t "Timer/Countdown")))
    ("t"
     (lambda () (if (and org-timer-start-time (null org-timer-countdown-timer)) "  Reset" "New Timer"))
     (lambda () (interactive) (if (or (null org-timer-start-time) (y-or-n-p "Reset the Timer ?")) (org-timer-start) (message "Do Nothing.")))
     :if (lambda () (or (null org-timer-countdown-timer) (null org-timer-start-time))))
    ("d"
     (lambda () (if org-timer-countdown-timer "  Reset" "New Countdown"))
     org-timer-set-timer
     :if (lambda () (or org-timer-countdown-timer (null org-timer-start-time))))
    ("s"
     (lambda () (!tdesc "s/p" "Stop/Pause"))
     (lambda () (interactive) (if (y-or-n-p "Stop it ?") (org-timer-stop) (message "Do Nothing.")))
     :format "%d" :if-non-nil org-timer-start-time)]
   ["Clock"
    (" C " "Goto Clock" org-clock-goto)
    ("r  "
     (lambda () (!tdesc "i/r" "Record Timer"))
     (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer?")) (call-interactively #'org-timer)))
     :format " %d")]
   ["Misc"
    (" v "   (lambda () (if evil-local-mode "Vil" "Evil")) evil-local-mode)
    (".  "   (lambda () (!tdesc ".,U" "Citre")) citre-peek :format " %d")
    ]
   ]
  (interactive)
  (require 'ox-publish)
  (transient-setup 'imtt/transient-gtd))

(transient-define-prefix imtt/transient-custom () ; C-c e
  [:hide
   (lambda () t)
   ]
  [("h"   "setup minibuffer-height"   im/set-minibuffer-height)
   ("w"   "setup indent-width/offset" im/set-indent-width-or-offset)
   ("k"   "temporary bind Key for function at point" im/bind-key-to-command)
   ]
  (interactive)
  (transient-setup 'imtt/transient-custom))

(transient-define-prefix imtt/transient-interact () ; C-.
  :transient-non-suffix 'transient--do-exit
  [:hide
   (lambda () t)
   ("y"   "" im/translate-pop)
   ("C-y" "" im/translate-pop)
   ("C-t" "" im/translate-buf)
   ("C-." "" gts-do-translate)
   ("T"   "" im/translate-buf)
   ("q"   "" im/wrap-current)
   ("+"   "" im/shengciben-add)
   ("-"   "" im/shengciben-add)
   ("="   "" im/shengciben-add)
   ("C-w" "" (lambda () (interactive) (let (browse-url-default-handlers) (call-interactively #'browse-url-of-buffer))))
   ("C-h" "" (lambda () (interactive) (message "C-w (open in browser) | C-. (translate-and-kill) | q (wrap word)")))
   ]
  [[("w"  "eww"        browse-url)]
   [("t"  "Translate"  im/translate-buf-prompt)]
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

(transient-define-prefix imtt/transient-ediff () ; C-c i e
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
   ])

(transient-define-prefix imtt/transient-desktop () ; C-c i d
  ["Desktop"
   [("l" "Load from Dir"   (lambda () (interactive) (let ((default-directory desktop-save-default-dir)) (call-interactively 'desktop-change-dir))))
    ("s" "Save to Dir"     (lambda () (interactive) (let ((default-directory desktop-save-default-dir)) (call-interactively 'desktop-save))))
    ("d" "Save to Default" (lambda () (interactive) (desktop-save desktop-save-default-dir)))
    ]
   [("c" "Desktop Clear"   desktop-clear)
    ("r" "Desktop Revert"  desktop-revert)
    ]
   ])

(transient-define-prefix imtt/transient-window () ; C-x C-o
  :transient-non-suffix #'transient--do-exit
  [:hide
   (lambda () t)
   [("-"   "Balance Win"   balance-windows)]
   [("M-p" "Win-Redo"      winner-redo :transient t)]
   ]
  [[("d"   "Dedicate"      (lambda () (interactive) (set-window-dedicated-p nil (not (window-dedicated-p)))))]
   [("o"   "Ace Window"    ace-window)]
   [("s"   "Swap Window"   (lambda () (interactive) (if (> (length (window-list)) 2) (ace-swap-window) (window-swap-states))))]
   [("x"   "Switch Layout" im/change-window-split-layout)]
   [("="   (lambda () (!tdesc "=/-" "Balance Window")) balance-windows-area :format "%d")]
   [("v"   "Views" im/views+)]
   [("M-n" (lambda () (!tdesc "M-n/p" "Winner")) winner-undo :format "%d" :transient t)]
   ]
  (interactive)
  (let ((transient-map nil))
    (transient-setup 'imtt/transient-window)))

(transient-define-prefix imtt/transient-yasnippet () ; C-c p a
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

(transient-define-prefix imtt/transient-help () ; C-h h
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
   ])

(provide 'imod-transient)

;;; imod-transient.el ends here
