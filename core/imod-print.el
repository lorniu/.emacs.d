;;; -*- lexical-binding: t -*-

;; Environment:
;;
;;   sudo pacman -S cups cups-pdf
;;
;;   yay -S epson-inkjet-printer-escpr # driver!
;;   yay -S intlfonts                  # fonts for emacs print!
;;
;;   sudo sc enable cups.socket
;;
;; Config:
;;
;;   http://localhost:631/
;;
;; Command:
;;
;;  M-x customize-group ps-print
;;
;;  M-x ps-print-buffer
;;  M-x ps-print-buffer-with-faces
;;

;;; Code:

(defvar im.print-duplex nil)

(xzz ps-print
  :config
  (setopt ps-paper-type 'a4
          ps-landscape-mode nil
          ps-selected-pages nil
          ps-print-header nil

          ps-font-size 8
          ps-font-family 'Courier

          ps-print-background-text nil
          ps-print-background-image nil)

  ;; ps-lpr-command
  ;; ps-lpr-switches
  ;; ps-printer-name

  (setopt bdf-directory-list
          (if IS-WIN (list (expand-file-name "fonts/bdf" installation-directory))
            '("/usr/share/emacs/fonts/bdf/" "/usr/local/share/emacs/fonts/bdf"))
          ps-multibyte-buffer 'bdf-font-except-latin))



(defclass transient-lisp-variable-buffer-local (transient-lisp-variable) ())

(cl-defmethod transient-infix-set ((obj transient-lisp-variable) value)
  (funcall (oref obj set-value) (make-local-variable (oref obj variable)) (oset obj value value)))

(transient-define-infix impr:paper-type ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-paper-type
  :key "-p"
  :reader (lambda (&rest _) (completing-read "Paper-Type: " (mapcar #'car ps-page-dimensions-database) nil t nil nil ps-paper-type)))

(transient-define-infix impr:font-size ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-font-size
  :key "-f"
  :reader (lambda (&rest _) (read-number "Font-Size: " (if (numberp ps-font-size) ps-font-size 8))))

(transient-define-infix impr:font-family ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-font-family
  :key "-F"
  :reader (lambda (&rest _) (intern (completing-read "Font-Family: "
                                                     (mapcar #'car ps-font-info-database)
                                                     nil t nil nil ps-font-family))))

(transient-define-infix impr:landscape-mode ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-landscape-mode
  :key "-m"
  :reader (lambda (&rest _) (not ps-landscape-mode)))

(transient-define-infix impr:duplex-style ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'im.print-duplex
  :key "-d"
  :reader (lambda (&rest _)
            (setq im.print-duplex
                  (completing-read "Duplex style: " '("two-sided-long-edge" "two-sided-short-edge" "one-sided")
                                   nil t nil nil im.print-duplex))
            (make-local-variable 'ps-lpr-switches)
            (setq ps-lpr-switches (cl-remove-if (lambda (x) (string-match-p "-o sides=" x)) ps-lpr-switches))
            (push (format "-o sides=%s" im.print-duplex) ps-lpr-switches)
            (setq-local ps-spool-duplex (not (string-equal "one-sided" im.print-duplex)))
            im.print-duplex))

(transient-define-infix impr:print-header ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-print-header
  :key "-H"
  :reader (lambda (&rest _) (not ps-print-header)))

(transient-define-infix impr:print-footer ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-print-footer
  :key "-T"
  :reader (lambda (&rest _) (not ps-print-footer)))

(transient-define-infix impr:selected-pages ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-selected-pages
  :key "-n"
  :reader (lambda (&rest _) (read--expression "Format (a (b . c) d): " ps-selected-pages)))

(transient-define-infix impr:ps-lpr-command ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-lpr-command
  :key "-C")

(transient-define-infix impr:ps-lpr-switches ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-lpr-switches
  :key "-S")

(transient-define-infix impr:background-text ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-print-background-text
  :key "-B"
  :reader (lambda (&rest _) (read--expression "Format ((STRING X Y FONT FONTSIZE GRAY ROTATION PAGES...)...): " (format "%S" ps-print-background-text))))

(transient-define-infix impr:line-number ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-line-number
  :key "-L"
  :reader (lambda (&rest _) (not ps-line-number)))

(transient-define-infix impr:number-of-columns ()
  :class 'transient-lisp-variable-buffer-local
  :description "columns per page"
  :variable 'ps-number-of-columns
  :key "-c"
  :reader (lambda (&rest _) (read-number "Columns: " ps-number-of-columns)))

(transient-define-infix impr:number-of-pages-per-paper ()
  :class 'transient-lisp-variable-buffer-local
  :description "pages per paper"
  :variable 'ps-n-up-printing
  :key "-x"
  :reader (lambda (&rest _) (read-number "Pages per paper: " ps-n-up-printing)))

(transient-define-infix impr:margin-top ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-top-margin
  :key "-t"
  :reader (lambda (&rest _) (read-number "Margin Top: " ps-top-margin)))

(transient-define-infix impr:margin-bottom ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-bottom-margin
  :key "-b"
  :reader (lambda (&rest _) (read-number "Margin Bottom: " ps-bottom-margin)))

(transient-define-infix impr:margin-left ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-left-margin
  :key "-l"
  :reader (lambda (&rest _) (read-number "Margin Left: " ps-left-margin)))

(transient-define-infix impr:margin-right ()
  :class 'transient-lisp-variable-buffer-local
  :variable 'ps-right-margin
  :key "-r"
  :reader (lambda (&rest _) (read-number "Margin Right: " ps-right-margin)))

(defun ipr--ps-print ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'ps-print-region)
    (call-interactively 'ps-print-buffer)))

(defun ipr--ps-print-with-faces ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'ps-print-region-with-faces)
    (call-interactively 'ps-print-buffer-with-faces)))

(transient-define-prefix im/print ()
  [["Options"
    (impr:paper-type)
    (impr:landscape-mode)
    (impr:font-size)
    (impr:font-family)
    (impr:duplex-style)
    (impr:selected-pages)
    ""
    (impr:margin-top)
    (impr:margin-bottom)
    (impr:margin-left)
    (impr:margin-right)
    ]
   ["Printer"
    (impr:print-header)
    (impr:print-footer)
    (impr:background-text)
    (impr:line-number)
    ""
    (impr:number-of-pages-per-paper)
    (impr:number-of-columns)
    ""
    (impr:ps-lpr-command)
    (impr:ps-lpr-switches)
    ]]
  ["Actions"
   [("p" "ps-print" ipr--ps-print)]
   [("P" "ps-print-with-faces" ipr--ps-print-with-faces)]]
  (interactive)
  (require 'ps-print)
  (transient-setup 'im/print))
