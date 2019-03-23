;;; imsilly.el --- External Scripts and Utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ic/external-script-dir (expand-file-name "~/.emacs.d/scripts/bin")
  "Where are the scripts files."
  :type 'string :group 'imfine)

;; path
(setenv "PATH" (concat ic/external-script-dir (if (eq system-type 'windows-nt) ";" ":") (getenv "PATH")))
(add-to-list 'exec-path ic/external-script-dir)

;; silly run
(defun im/silly (&optional arg)
  "Silly run scripts, default in emacs.d dir, or if ARG t, in current dir."
  (interactive "P")
  (let* ((dir (if arg default-directory "~/.emacs.d/scripts/"))
         (script (read-file-name "Choose script:" dir)))
    (if (and (file-exists-p script) (not (directory-name-p script)))
        (cond ((string-match-p "\\.sh$" script)
               (async-shell-command (concat "bash -c " script)))
              ((string-match-p "\\(\\.bat\\|\\.cmd\\)$" script)
               (if (eq system-type 'windows-nt) (async-shell-command script)
                 (error "Batch file should be executed on windows.")))
              (t (async-shell-command script)))
      (error "Please choose a script file first."))))



;;; Walk through directory

(defmacro im/walk-with-directory-buffers (dir filter &rest what-to-do)
  "Walk the DIR under FILTER, operate each with buffer."
  (declare (indent defun))
  (let ((file (gensym)))
    `(save-window-excursion
       (dolist (,file (directory-files-recursively ,dir (or ,filter "*")))
         (with-current-buffer (find-file ,file)
           (unwind-protect (progn ,@what-to-do)
             (if (buffer-modified-p)
                 (save-buffer)
               (kill-buffer)))))
       (message "\n||| Finished. |||\n") 1)))

(defun s/refactor-encoding-in-directory ()
  "Emacs style batch mode / change coding under the dir to UTF8."
  (dolist (file (directory-files-recursively "/var/www/" ".*\\.rb"))
    (with-current-buffer (find-file file)
      (when (or (eq buffer-file-coding-system 'chinese-gbk-dos)
                (eq buffer-file-coding-system 'chinese-gbk-unix))
        (set-buffer-file-coding-system 'utf-8)
        (save-buffer))
      (kill-buffer))))



;;; Generate config for emacs on windows

(defun s/start-emacs-for-windows ()
  "Initialization ContextMenu for Windows:
1. execute this function, and import the generated `.reg` file to OS
2. put the symlink of `runemacs.exe` with arg `--daemon` to `shell:start`
3. put the symlink of `emacsclientw.exe` with arg `-n -c -a ''` to Desktop
4. if CYGWIN exist, put it to 'PATH' env"
  (interactive)
  (unless (eq system-type 'windows-nt) (error "This only works on Windows!"))

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
      (when (yes-or-no-p (format "已保存为 %s。跳转？" reg-file))
        (w32-shell-execute "open" emacs-home)))))



;;; Convert postgres to mysql

(defun s/sql-pg-to-mysql ()
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

(cl-defun s/dump-pg-to-mysql (table-name dest-file &optional (dbname "imdata"))
  "Dump table data from local Postgres to SQL file can be used by MySQL."
  (let ((cmd "pg_dump -U postgres %s -t %s --insert --column-inserts"))
    (setq cmd (format cmd dbname table-name))
    (if (file-exists-p dest-file)
        (message "File-already exist, rename first.")
      (with-temp-file dest-file
        (call-process-shell-command cmd nil (current-buffer))
        (s/sql-pg-to-mysql))
      (find-file dest-file))))



;;; ssh/nat

(defun s/generate-ssh-config-file ()
  "Generate .ssh/config file."
  (interactive)
  (let ((file "~/.ssh/config"))
    (if (file-exists-p file)
        (error "File '%s' already exist." file)
      (with-temp-file file
        (insert
         "Host *
  ControlMaster auto
  ControlPath ~/.ssh/master-%C
  ControlPersist no
"))
      (find-file (expand-file-name file)))))

(defun s/generate-nat-traverse-cmd (name vps inner)
  (interactive (list (read-string "Please input [SERVICE_NAME]: ")
                     (read-string "Please Input [PUBLIC:PORT]: ")
                     (read-string "Please Input [INNER:PORT]: ")))
  (let ((vps-arr (split-string vps ":")))
    (insert (format "cygrunsrv -I ah_%s -p /usr/bin/autossh -a \"-M 0 -o ServerAliveInterval=30 -o ServerAliveCountMax=3 -o ExitOnForwardFailure=yes -i ~/.ssh/id_rsa -NR %s:%s root@%s\" -y tcpip"
                    name (second vps-arr) inner (first vps-arr)))))


(provide 'imsilly)

;;; imsilly.el ends here
