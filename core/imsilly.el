;;; imsilly.el --- External And Miscellaneous
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))


;;; generate config for emacs on windows

(defun si/start-emacs-for-windows ()
  "Initialization ContextMenu for Windows, etc:
1. execute this function, and import the generated `.reg` file to OS
2. put the symlink of `runemacs.exe` with arg `--daemon` to `shell:start`
3. put the symlink of `emacsclientw.exe` with arg `-n -c -a ''` to Desktop
4. if CYGWIN exist, put it to 'PATH' env"
  (interactive)
  (unless (eq system-type 'windows-nt) (error "NOT-WINNT, this is useless!"))

  (cl-flet ((win-path (n path) (replace-regexp-in-string "/" (if (> n 1) "\\\\" "\\") (file-truename path) t t)))
    (let* ((emacs-home (win-path 2 (car (split-string exec-directory "libexec"))))
           (reg-file    (concat emacs-home "OpenWithEmacs.reg"))
           (reg-string  "Windows Registry Editor Version 5.00\n
[HKEY_CLASSES_ROOT\\*\\shell\\emacs]\n@=\"Edit &With GNU Emacs\"\n
[HKEY_CLASSES_ROOT\\*\\shell\\emacs\\Command]\n@=\"\\\"%s\\\" -n -na \\\"%s\\\" \\\"%%1\\\"\" ")
           (buffer-file-coding-system 'gbk))

      ;; SETENV: emacs-server-file
      (start-process "a" "f" "setx" "EMACS_SERVER_FILE" (win-path 1 (concat server-auth-dir "server")))

      ;; Generate ContextMenu.reg
      (with-temp-file reg-file
        (insert (format reg-string
                        (concat emacs-home "bin\\\\" "emacsclientw.exe")
                        (concat emacs-home "bin\\\\" "runemacs.exe"))))

      ;; Import manually please.
      (if (yes-or-no-p (format "已保存为 %s。跳转？" reg-file))
          (w32-shell-execute "open" emacs-home)))))


;;; wark through directory

(defmacro si/walk-with-directory-buffers (dir filter &rest form)
  "Walk the DIR under FILTER, operate each with current buffer. FORM is the deal."
  (declare (indent defun))
  (let ((file (gensym)))
    `(save-window-excursion
       (dolist (,file (directory-files-recursively ,dir (or ,filter "*")))
         (with-current-buffer (find-file ,file)
           (unwind-protect (progn ,@form)
             (if (buffer-modified-p)
                 (save-buffer)
               (kill-buffer)))))
       (message "\n||| Finished. |||\n") 1)))

(defun si/refactor-encoding-in-directory ()
  "Emacs style batch mode / change coding under the dir."
  (dolist (file (directory-files-recursively "/var/www/" ".*\\.rb"))
    (with-current-buffer (find-file file)
      (when (or (eq buffer-file-coding-system 'chinese-gbk-dos)
                (eq buffer-file-coding-system 'chinese-gbk-unix))
        (set-buffer-file-coding-system 'utf-8)
        (save-buffer))
      (kill-buffer))))


;;; convert postgres to mysql

(defun si/sql-pg-to-mysql ()
  "Convert sql statement from Postgres to MySQL. Open the sql file, execute it, you will get the result."
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (flush-lines "^SET\\|^SELECT\\|^GRANT\\|OWNER TO")
    (im/replace-all-in-buffer
     '(("\"" . "`")
       ("public\\." . "")
       ("character varying" . "varchar")
       ("timestamp without time zone\\|timestamp" . "datetime")
       ("TABLE ONLY" . "TABLE")))))

(cl-defun si/dump-pg-to-mysql (table-name dest-file &optional (dbname "imdata"))
  "Dump table data from local Postgres to SQL file can be used by MySQL."
  (let ((cmd "pg_dump -U postgres %s -t %s --insert --column-inserts"))
    (setq cmd (format cmd dbname table-name))
    (if (file-exists-p dest-file)
        (message "File-already exist, rename first.")
      (with-temp-file dest-file
        (call-process-shell-command cmd nil (current-buffer))
        (si/sql-pg-to-mysql))
      (find-file dest-file))))



(provide 'imsilly)

;;; imsilly.el ends here
