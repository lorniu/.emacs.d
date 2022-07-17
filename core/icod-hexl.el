;;; icod-hexl.el --- Hexl Edit -*- lexical-binding: t -*-

;; nhexl is enhanced hexl-mode located in elpa:
;; - <INS> toggle override/insert-mode
;; - nhexl-nibble-edit-mode, edit by per 4-bit
;; - nhexl-mode overrides C-u to use hexadecimal, so you can do C-u a 4 C-f to advance by #xa4 characters

;;; Code:

(x hexl
   :ref ("https://elpa.gnu.org/packages/nhexl-mode.html"))

(x nhexl-mode
   "Can insert/search/nibbie-edit and so on.")

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
    ("d" (lambda () (!tdesc "d/x/o j/g" "Shortcuts")) hexl-insert-decimal-char :format " %d")
    ]
   ]
  (interactive)
  (if (eq major-mode 'hexl-mode)
      (transient-setup 'imtt/transient-hexl-mode)
    (user-error "You should invoke this in hexl-mode.")))

(provide 'icod-hexl)

;;; icod-hexl.el ends here
