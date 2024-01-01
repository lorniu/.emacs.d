;;; -*- lexical-binding: t -*-

;;; Code:

(require 'wdired)
(require 'ls-lisp)

(xzz dired
  "Maybe should add this to .bashrc:
  "
  "  export LC_COLLATE=C
  "
  :bind ( :map dired-mode-map
	      ( "6" . dired-up-directory )
	      ( "^" . im/dired-up-directory-follow-symlink )
	      ( "e" . wdired-change-to-wdired-mode )
          ( "g" . (lambda () (interactive) (im/dired-sort)) )
          ( "s" . im/dired-sort )
	      ( "z" . im/dired-du-size )
	      ( "Y" . im/dired-rsync )
	      ( "," . im/dired--toggle-collapse ) )
  :config
  (setopt wgrep-enable-key "e"
          dired-dwim-target t

          dired-listing-switches "-alh"
          ls-lisp-use-insert-directory-program nil
          ls-lisp-dirs-first t
          ls-lisp-use-string-collate nil
          ls-lisp-UCA-like-collation nil
          ls-lisp-use-localized-time-format t
          ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))

  (defvar im:ls-lisp-xid-shorten-threshold (if IS-WIN 8)
    "Shorten display the Uid/Gid column, eg, Administrators is toooooo long.")

  (defun:before ls-lisp-format//shorten (_fn file-attr _fs _sw _ti)
	(when im:ls-lisp-xid-shorten-threshold
	  (cl-labels ((norm (file n &optional
					          (threshold im:ls-lisp-xid-shorten-threshold)
					          (len (- im:ls-lisp-xid-shorten-threshold 3)))
				    (let ((item (nth n file)))
				      (when (and (stringp item) (> (length item) threshold))
				        (setf (nth n file)
					          (propertize (format "%s~" (upcase (cl-subseq item 0 len)))
						                  'help-echo item))))))
		(norm file-attr 2)
		(norm file-attr 3)))))

(xzz dired-collapse/e
  :init
  (defvar im:dired-collapse-enable t)
  (defun:hook dired-mode-hook/collapse ()
    (dired-collapse-mode (if im:dired-collapse-enable 1 -1)))
  (defun im/dired--toggle-collapse (&optional arg)
    "Toggle collapse, with `C-u' then futrue will effact future dired buffer."
    (interactive "P")
    (let (current-prefix-arg)
      (call-interactively 'dired-collapse-mode))
    (when arg
      (setq im:dired-collapse-enable dired-collapse-mode)
      (message "Global dired-collapse mode: %s" im:dired-collapse-enable))))

(defun im/dired-du-size ()
  "Count size of marked dir/files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-scb" files)
      (message
       "Size of all %s marked files: %s"
       (length files)
       (progn
         (re-search-backward "^\\([ 0-9.,]+[A-Za-z]*\\).*\\(total\\|总用量\\)$")
         (file-size-human-readable (string-to-number (match-string 1))))))))

(defun im/dired-rsync (dest)
  "Send marked files with rsync."
  (interactive
   (list (progn
           (require 'dired-aux)
           (expand-file-name
            (read-file-name "Rsync to:" (dired-dwim-target-directory))))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        (rsync-command "rsync -arvz --progress "))
    (dolist (file files)
      (setq rsync-command
            (concat rsync-command (shell-quote-argument file) " ")))
    (setq rsync-command (concat rsync-command (shell-quote-argument dest)))
    (async-shell-command rsync-command "*rsync*")
    (other-window 1)))

(defun im/dired-up-directory-follow-symlink (&optional arg)
  (interactive "P")
  (let ((dir (directory-file-name (dired-current-directory))))
    (if (not (not (file-symlink-p dir)))
        (cl-letf* (((symbol-function #'dired-current-directory)
                    (lambda (&optional _) (file-truename dir))))
          (dired-up-directory arg))
      (dired-up-directory arg))))


;;; Enhanced Dired Sort

(defvar im:dired-sort-list '((:name "" -1) (:date "-t" -1) (:size "-S" -1) (:extension "-X" 1)))

(defvar-local im:dired-sort-current nil)

(defun im/dired-sort (&optional key order)
  "Sort dired by the given criteria.
When 'current-prefix-arg' non-nil then fall back to builtin dired-sort."
  (interactive)
  (if (called-interactively-p 'any)
      (call-interactively (if current-prefix-arg #'dired-sort-toggle-or-edit #'im/transient-dired-sort))
    (unless (eq major-mode 'dired-mode)
      (user-error "You should use this in dired mode"))
    (if (and (null key) (null order)) ; revert
        (setq dired-actual-switches dired-listing-switches
              mode-name "Dired by name"
              im:dired-sort-current nil)
      (unless key (setq key :name)) ; default sort by name
      (unless order ; reverse or default
        (setq order (if (equal key (car-safe im:dired-sort-current))
                        (* -1 (cdr-safe im:dired-sort-current))
                      (caddr (assoc key im:dired-sort-list)))))
      (let ((switches (format "%s %s %s"
                              dired-listing-switches
                              (cadr (assoc key im:dired-sort-list))
                              (if (cl-plusp order) "" "-r"))))
        (dired-sort-R-check switches)
        (setq dired-actual-switches switches)
        (setq mode-name (format "Dired by %s %s"
                                (cl-subseq (symbol-name key) 1)
                                (if (cl-plusp order) "⇡" "⇣")))
        (setq im:dired-sort-current (cons key order))))
    (force-mode-line-update)
    (revert-buffer)))

(transient-define-prefix im/transient-dired-sort ()
  :transient-non-suffix #'transient--do-exit
  [:hide
   (lambda () t)
   ("N"  "1"          (lambda () (interactive) (im/dired-sort :name 1)))
   ("D"  "2"          (lambda () (interactive) (im/dired-sort :date 1)))
   ("S"  "3"          (lambda () (interactive) (im/dired-sort :size 1)))
   ("E"  "4"          (lambda () (interactive) (im/dired-sort :extension 1)))
   ("g"  "5"          (lambda () (interactive) (im/dired-sort)))]
  [[("n" "Name"       (lambda () (interactive) (im/dired-sort :name))      :transient t)]
   [("d" "Date"       (lambda () (interactive) (im/dired-sort :date))      :transient t)]
   [("s" "Size"       (lambda () (interactive) (im/dired-sort :size))      :transient t)]
   [("e" "Extension"  (lambda () (interactive) (im/dired-sort :extension)) :transient t)]]
  (interactive)
  (if (eq major-mode 'dired-mode)
      (transient-setup 'im/transient-dired-sort)
    (user-error "Should be used in Dired")))


;;; Interface (Helper)

(transient-define-prefix im/assist-dired-mode ()
  :transient-non-suffix 'transient--do-exit
  [["Dired"
    ("o"  (lambda () (!tdesc " ovWj " "Open...")) dired-find-file-other-window :format " %d")
    ("G"  (lambda () (!tdesc " GMOT " "Change Group/Mode/Owner/Touch")) dired-do-chgrp :format " %d")
    ("L"  (lambda () (!tdesc " LB!& " "Load/Compile/ShellCmd/AsyncCmd")) dired-do-load :format " %d")
    ("D"  (lambda () (!tdesc "RCDSHc" "Move/Copy/Del/SLink/HLink/Compress")) dired-do-delete :format " %d")]
   ["Mark & Flag"
    ("m " (lambda () (!tdesc "mut " "Mark")) dired-mark :format " %d")
    ("**" (lambda () (!tdesc "* % " "Mark...")) dired-mark-executables :format " %d")
    ("d " (lambda () (!tdesc "d~# " "Flag (k/x)")) dired-flag-file-deletion :format " %d")
    ("A " (lambda () (!tdesc "AQY " "Find/Replace/RsyncTo")) dired-do-find-regexp :format " %d")]
   ["View & Others"
    (","  (lambda () (!tdesc ",$( " "Toggle View")) dired-collapse-mode :format " %d")
    ("y"  (lambda () (!tdesc "yzi " "Type/Size/Insert")) dired-show-file-type :format " %d")
    ""
    ("/ d" "dotnet-cli" sharper-main-transient)]
   ]
  (interactive)
  (if (eq major-mode 'dired-mode)
      (progn
        (transient-setup 'im/assist-dired-mode)
        (message (propertize "[C-t] for image-display, [:] for epa-actions" 'face 'font-lock-comment-face)))
    (user-error "Should be used in Dired")))
