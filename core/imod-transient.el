;;; -*- lexical-binding: t -*-

;;; Code:

(defreference transient "magit/transient")

(setq transient-substitute-key-function
      (lambda (obj)
        ;; 'c-g' to quit all, 'C-c' to quit one
        (let ((key (oref obj key)))
          (cond ((string= "C-q" key) "C-g")
                ((string= "C-g" key) "C-c")
                ((string= "C-g" key) "q")
                (t key)))))

(defun:around transient--lookup-key (fn keymap key)
  (unless (string-blank-p (format "%s" key))
    (funcall fn keymap key)))

(with-eval-after-load 'lisp-mode
  ;; Imenu Support
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
    ("r"  "Rs/Fast"    im/transient-quicker)]
   [("e"  "Ediff"      im/transient-ediff)
    ("."  "Retrieve"   im/transient-retrieve)]
   [("p"  "Print"      im/print)
    ("c"  "Agenda"     im/transient-agenda)]
   [("h"  (lambda () (concat " Help" (propertize "    -> Dashbox" 'face 'font-lock-comment-face))) im/transient-help)]
   ])

(transient-define-prefix im/transient-goto () ; C-x i
  [:hide
   (lambda () t)
   ("p"     "1"   im/pages+)
   ("l"     "2"   consult-line)
   ("o"     "3"   consult-outline)
   ("n"     "4"   consult-global-mark)
   ("/"     "5"   consult-ripgrep)
   ("SPC"   "7"   consult-register)
   ("s SPC" "8"   consult-register-store)]
  [["Goto:"
    ("f" "Char" im/go-to-char)
    ("v" "Views" (lambda () (interactive) (im/toggle-layout t)))
    ("i" (lambda () (!tdesc "i p l o" "..")) consult-imenu :format " %d")
    ]
   [""
    ("b" "  Bookmark" consult-bookmark)
    ("m" (lambda () (!tdesc "m/n" "Marks")) consult-mark :format " %d")
    ("j" (lambda () (!tdesc "j/ " "Register")) consult-register :format " %d")
    ]
   [""
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
   ("C-t"  "0"  gt-translate)
   ("C-,"  "2"  im/transient-desktop)]
  [[("g"  "Google"          r/search-google)]
   [("f"  "Wolfram"         r/search-wolfram-alpha)]
   [("h"  "Github"          r/search-github)]
   [("s"  "StackOverflow"   r/search-stackoverflow)]
   [("v"  "Wikipedia"       r/search-wikipedia)]
   [("a"  "ArchWiki"        r/search-arch-wiki)]
   [("t"  "Translate"       gt-translate)]
   [("i"  "Iconify"         r/search-iconify)]
   [(","  "Destktop"        im/transient-desktop)]]
  (interactive)
  (let ((transient-map nil))
    (transient-setup 'im/transient-retrieve)))

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
