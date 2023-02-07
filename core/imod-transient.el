;;; imod-transient.el --- Transients -*- lexical-binding: t -*-

;;; Code:

(x transient
   :ref "magit/transient"
   :config
   ;; use 'c-g' to quit all, 'C-c' to quit one
   (setq transient-substitute-key-function
         (lambda (obj)
           (let ((key (oref obj key)))
             (cond ((string= "C-q" key) "C-g")
                   ((string= "C-g" key) "C-c")
                   ((string= "C-g" key) "q")
                   (t key))))))

;; Imenu Support
(with-eval-after-load 'lisp-mode
  (add-to-list 'lisp-imenu-generic-expression
               (list "Functions"
                     "^\\s-*(\\(transient-define-\\(?:\\(?:in\\|pre\\|suf\\)fix\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
                     2)))

(defun !tdesc (key desc)
  "Generate string used by `transient-define-prefix'."
  (concat (propertize key 'face 'transient-key) " " desc))



(transient-define-prefix im/transient-dashbox () ; C-c d
  [:hide (lambda () t)]
  [[("f"  "Files"      (lambda () (interactive) (im:run-choose-command 'ztree-diff 'ztree-dir 'find-dired 'find-grep-dired)))
    ("d"  "Desktop"    im/transient-desktop)]
   [("n"  "Network"    im/network-interface)
    ("w"  "Windows"    im/transient-windows)]
   [("y"  "Happy"      (lambda () (interactive) (im:run-choose-command 'emms 'mpv-play 'gnus 'erc 'doctor)))
    ("r"  "Rs/Fast"    im/transient-fast)]
   [("e"  "Ediff"      im/transient-ediff)
    ("."  "Retrieve"   im/transient-retrieve)]
   [("p"  "Print"      im/print)
    ("c"  "Agenda"     im/transient-agenda)]
   [("h"  (lambda () (concat " Help" (propertize "    -> Dashbox" 'face 'font-lock-comment-face))) im/transient-help)]
   ])

(transient-define-prefix im/transient-agenda () ; C-c c
  [:hide
   (lambda () t)
   ("m"   "1"  im/note-publish-interactively)
   ("N"   "2"  (lambda () (interactive) (im/note-publish t)))
   ("M"   "3"  (lambda () (interactive) (im/note-publish-interactively t)))
   ("R"   "4"  im/note-publish-reset-directories)
   ("SPC" "5"  org-timer-pause-or-continue :if-non-nil org-timer-start-time)
   ("i"   "6"  (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer??")) (call-interactively #'org-timer-item))))
   ("W"   "7"  im/view-freshwords)]
  [["Agenda"
    ("l"  "Agenda Lines"   im/org-agenda-lines)
    ("f"  "Agenda Files"   im/org-agenda-files)]
   ["Notes"
    ("  p"  "   Capture"  org-capture)
    ("n" (lambda () (!tdesc "nN mMR"  " Publish..")) im/note-publish :format "%d")]
   [:description
    (lambda () (cond (org-timer-countdown-timer (format "%s/%s" (propertize "Countdown" 'face 'transient-heading) (propertize (if (eq org-timer-countdown-timer 'paused) "Paused" "Running") 'face 'warning)))
                     (org-timer-start-time (format "%s/%s" (propertize "Timer" 'face 'transient-heading) (propertize "Running" 'face 'warning)))
                     (t "Timer/Countdown")))
    ("t"
     (lambda () (if (and org-timer-start-time (null org-timer-countdown-timer)) "    Reset" "  New Timer"))
     (lambda () (interactive) (if (or (null org-timer-start-time) (y-or-n-p "Reset the Timer ?")) (org-timer-start) (message "Do Nothing.")))
     :if (lambda () (or (null org-timer-countdown-timer) (null org-timer-start-time))))
    ("d"
     (lambda () (if org-timer-countdown-timer "    Reset" "New Countdown"))
     org-timer-set-timer
     :if (lambda () (or org-timer-countdown-timer (null org-timer-start-time))))
    ("S"
     (lambda () (!tdesc "S/SPC" "Stop/Pause"))
     (lambda () (interactive) (if (y-or-n-p "Stop it ?") (org-timer-stop) (message "Do Nothing.")))
     :format "%d" :if-non-nil org-timer-start-time)]
   ["Clock"
    (" c " "Goto Clock" org-clock-goto)
    ("r  "
     (lambda () (!tdesc "i/r" "Record Timer"))
     (lambda () (interactive) (when (or org-timer-start-time (y-or-n-p "Start a new timer?")) (call-interactively #'org-timer)))
     :format " %d")]
   ["Miscellaneous"
    ("v" (lambda () (if evil-local-mode " -vil" " Evil")) evil-local-mode)
    ("w" (lambda () (!tdesc "wWp" "Words")) im/pick-freshword :format "%d")]
   ]
  (interactive)
  (require 'ox-publish)
  (transient-setup 'im/transient-agenda))

(transient-define-prefix im/transient-goto () ; C-x i
  [:hide
   (lambda () t)
   ("p"     "1"   im/pages+)
   ("l"     "2"   consult-line)
   ("o"     "3"   consult-outline)
   ("n"     "4"   consult-global-mark)
   ("/"     "5"   consult-ripgrep)
   ("C"     "6"   iy-go-to-char-backward)
   ("SPC"   "7"   consult-register)
   ("s SPC" "8"   consult-register-store)]
  [["Goto:"
    ("f" "Char" iy-go-to-char)
    ("v" "Views" (lambda () (interactive) (im/toggle-layout t)))
    ("i" (lambda () (!tdesc "i p l o" "..")) consult-imenu :format " %d")
    ]
   [""
    ("b" "  Bookmark" consult-bookmark)
    ("m" (lambda () (!tdesc "m/n" "Marks")) consult-mark :format " %d")
    ("j" (lambda () (!tdesc "j/ " "Register")) consult-register :format " %d")
    ]
   [""
    ("w " "Treemacs"       im/treemacs-follow)
    ("` " "Diagnostics"    im/toggle-diagnostics-buffer)
    ("e " "Compile Errors" consult-compile-error)
    ]
   [""
    ("s b"  "set-bookmark" bookmark-set)
    ("s j"  "set-register" consult-register-store)]
   ]
  (interactive)
  (let ((transient-show-popup -0.6))
    (transient-setup 'im/transient-goto)))

(transient-define-prefix im/transient-insert () ; C-c i
  [:hide
   (lambda () t)
   ("d t" "1"     (lambda () (interactive) (im/insert-date t)))
   ("d i" "2"     org-time-stamp-inactive)
   ("F  " "3"     (lambda () (interactive) (let ((current-prefix-arg 4)) (call-interactively 'figlet))))
   ]
  ["Insert to buffer"
   [("s" "Yasnippet" im/transient-yasnippet)
    ("i" "Insert Snippet" yas-insert-snippet)
    ]
   [("d d" (lambda () (!tdesc "d d/t" (format-time-string "%F"))) im/insert-date :format "%d")
    ("d s" (lambda () (!tdesc "d s/i" "[Timestamp]"))             org-time-stamp :format "%d")
    ]
   [("f  " (lambda () (!tdesc "f/F" "FIGlet")) figlet :format " %d")
    ("SPC" "Zero-Width Space" (lambda () (interactive) (insert "\u200b")))
    ]
   [:if-mode
    'org-mode
    ("m" "Demarcate Block" org-babel-demarcate-block)
    ("b" "new/wrap src-block" im/org-wrap-src)
    ]
   ])

(transient-define-prefix im/transient-retrieve () ; C-.
  :transient-non-suffix 'transient--do-exit
  [:hide
   (lambda () t)
   ("C-t"  "0"  gt-do-translate)
   ("C-."  "1"  r/websites)
   ("C-,"  "2"  im/transient-desktop)]
  [[("g"  "Google"          engine/search-google)]
   [("f"  "Wolfram"         engine/search-wolfram-alpha)]
   [("h"  "Github"          engine/search-github)]
   [("s"  "StackOverflow"   engine/search-stackoverflow)]
   [("v"  "Wikipedia"       engine/search-wikipedia)]
   [("a"  "ArchWiki"        engine/search-arch-wiki)]
   [("t"  "Translate"       gt-do-translate)]
   [("i"  "Iconify"         engine/search-iconify)]
   [("."  "Websites"        r/websites)]
   [(","  "Destktop"        im/transient-desktop)]]
  (interactive)
  (let ((transient-map nil))
    (transient-setup 'im/transient-retrieve)))

(transient-define-prefix im/transient-ediff () ; C-c d e
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

(transient-define-prefix im/transient-help () ; C-h C-h
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
    ]
   ])

(provide 'imod-transient)

;;; imod-transient.el ends here
