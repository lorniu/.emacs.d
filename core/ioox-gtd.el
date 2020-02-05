;;; ioox-gtd.el --- GTD in Org -*- lexical-binding: t -*-

;;; Code:

(defvar org-default-gtd-dir (or (loco "000/" t) (loco "./")))

(defvar org-agenda-notes (append
                          (file-expand-wildcards (expand-file-name "*/*.org" org-directory))
                          (file-expand-wildcards (expand-file-name "*/*/*.org" org-directory))))

(defun org-default-file-expand (name) (expand-file-name name org-default-gtd-dir))

(setq bbdb-file (org-default-file-expand ".bbdb")
      diary-file (org-default-file-expand ".diary"))

(setq org-log-into-drawer t
      org-clock-into-drawer t

      org-deadline-warning-days 5
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t

      org-default-notes-file (org-default-file-expand "notes.org")
      org-default-tasks-file (org-default-file-expand "tasks.org")
      org-default-journal-file (org-default-file-expand "journal.org")
      org-default-sketch-file (org-default-file-expand "sketch.org")

      org-refile-targets `((org-agenda-files . (:level . 1))))

(setq org-agenda-window-setup 'current-window
      org-agenda-files (cl-remove-if-not
                        (lambda (f) (string-match-p "^[a-zA-Z]" (file-name-nondirectory f)))
                        org-agenda-notes)
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %-16:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(setq org-clock-idle-time 30
      org-clock-x11idle-program-name "xprintidle" ; pacman -S xprintidle
      org-clock-persist t
      org-clock-persist-file (locc "org-clock-save.el")
      org-clock-sound t
      org-show-notification-handler (lambda (m)
                                      (let ((ring-bell-function nil))
                                        (org-clock-play-sound org-clock-sound)
                                        (alert m :timeout 1200 :title "Org Clock Notify" :severity 'high))))



(setq org-tag-alist
      '(("Learns" . ?c) ("Work" . ?w) ("Life" . ?l) ("Teaching")
        ("Dodo" . ?d) ("Yiyi" . ?e)
        ("Scope") ("Object") ("Function") ("Closure") ("Module") ("Concurrent") ("Network") ("IO") ("DateTime")))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCEL(c!)")))

(setq org-capture-templates
      `(("n" "Notes" entry (file ,org-default-notes-file) "* %U\n\n%i%?" :prepend t :empty-lines 1)
        ("t" "Tasks" entry (file+headline ,org-default-tasks-file "Ungrouped") "* TODO %i%?" :jump-to-captured t)
        ("j" "Journal" plain (file+olp+datetree ,org-default-journal-file) "%U\n\n%i%?" :empty-lines 1)
        ("d" "涂鸦簿" entry (file ,org-default-sketch-file) "* %U\n\n%i%?" :prepend t :empty-lines 1)))

(defun:hook org-after-todo-statistics-hook ()
  "Switch entry to DONE when all subentries are done"
  (let (org-log-done org-todo-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun:hook kill-emacs-hook/clock ()
  (if (featurep 'org) (org-clock-save)))



(with-eval-after-load 'org
  (require 'org-clock)
  (require 'org-timer)
  (run-with-idle-timer 10 nil (lambda () (with-message nil (org-clock-load)))))

(defun:around org-get-agenda-file-buffer$ (f &rest args)
  (with-supress-recentf (apply f args)))



(x calendar
   :defer-config
   (require 'cal-china-x)
   (setq calendar-week-start-day 1
         mark-holidays-in-calendar t
         cal-china-x-important-holidays cal-china-x-chinese-holidays
         calendar-holidays (append cal-china-x-important-holidays
                                   cal-china-x-general-holidays)))

(defun im/diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (when year
    (setq year (let* ((d-date (diary-make-date lunar-month lunar-day year))
                      (a-date (calendar-absolute-from-gregorian d-date))
                      (c-date (calendar-chinese-from-absolute a-date)))
                 (+ (* 100 (car c-date)) (cadr c-date)))))
  (diary-chinese-anniversary lunar-month lunar-day year mark))

(provide 'ioox-gtd)

;;; ioox-gtd.el ends here
