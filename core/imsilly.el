;;; -*- lexical-binding: t -*-

;;; Code:

(defun im/silly (&optional arg)
  "Silly run scripts, default in emacs.d dir, or if ARG t, in current dir."
  (interactive "P")
  (let* ((dir (cond ((null arg) (loce "bin/"))
                    ((= arg 1) (loco "scripts/"))
                    (t default-directory)))
         (script (read-file-name "Choose script: " dir))
         args script-with-args)
    (if (and (file-exists-p script) (not (directory-name-p script)))
        (progn
          ;; search elisp line '#.(foo bar)',
          ;; eval and pass result to script
          (with-temp-buffer
            (insert-file-contents-literally script)
            (goto-char (point-min))
            (when (search-forward "#." nil t)
              (let* ((sexp (buffer-substring-no-properties
                            (match-end 0) (line-end-position)))
                     (res (eval (read sexp))))
                (setq args (cl-loop for i in (if (listp res) res (list res))
                                    concat (format " %s" (replace-regexp-in-string " +" "\\\\ " i)))))))
          (setq script-with-args (concat script args))
          ;; different platform
          (cond ((string-match-p "\\.sh$" script)
                 (async-shell-command (concat "bash -c '" script-with-args "'")))
                ((string-match-p "\\(\\.bat\\|\\.cmd\\)$" script)
                 (if IS-WIN
                     (async-shell-command script-with-args)
                   (error "Batch file should be executed on windows.")))
                (t (async-shell-command script-with-args))))
      (error "Please choose a script file first."))))

(defun im/interactive-script (&optional script buffer)
  "Choose some scripts to run. Often used in mode-hook."
  (interactive (list (read-file-name "Script file: ")))
  (let ((id (+ 10000 (random 20000))))
    (if (and script (file-executable-p script))
        (progn
          (unless buffer (setq buffer "*interactive-script*"))
          (alert "Script running..." :timeout 120 :severity 'low :title script :id id)
          (set-process-sentinel
           (start-process-shell-command "interactive-script" buffer script)
           (lambda (proc event)
             (alert (format "Script executed %s" event) :timeout 10 :title script :id id)
             (when (string-match-p "finished" event)
               (with-current-buffer (process-buffer proc)
                 (view-mode -1)
                 (goto-char (point-max))
                 (insert (format "\n======= %s ========\n\n\n" (time-str)))
                 (view-mode 1))))))
      (when (called-interactively-p 'any)
        (message "Script %s is not available." script)))))


;;; Emacself

(defun sl/emacs-recompile-els (&optional force)
  "Recompile .el files."
  (interactive "P")
  (let* ((def (loce "extra/"))
         (dir (read-directory-name "Byte recompile directory: " def def)))
    (byte-recompile-directory dir 0 force)))

(defun sl/emacs-generate-configs-for-windows ()
  "Invoke this and then (1) import `.reg' file (2) move `eee.cmd' to PATH."
  (interactive)
  (unless IS-WIN (error "This only works for Windows!"))

  (cl-labels ((win-path (path &optional escp)
                (replace-regexp-in-string "/" (if escp "\\\\" "\\") (file-truename path) t t))
              (emacs-file (file &optional escp)
                (win-path (concat (car (split-string exec-directory "libexec")) file) escp))
              (s-bin (&optional escp) (emacs-file "bin/runemacs.exe" escp))
              (c-bin (&optional escp) (emacs-file "bin/emacsclientw.exe" escp)))
    ;; SetEnv: emacs-server-file
    (start-process "a" " for-set-env" "setx" "EMACS_SERVER_FILE" (win-path (concat server-auth-dir "server")))

    ;; Need copy this to PATH
    (with-temp-file (emacs-file "eee.cmd" t)
      (setq-local buffer-file-coding-system 'gbk)
      (insert (format "\"%s\" -n -na \"%s\" \"%%1\"" (c-bin) (s-bin))))

    ;; Need import this to REGEDIT
    (with-temp-file (emacs-file "Open-With-Emacs.reg" t)
      (setq-local buffer-file-coding-system 'gbk)
      ;; [open-with-emacs]
      (insert (string-join (cons "Windows Registry Editor Version 5.00\n"
                                 (mapcar (lambda (subpath)
                                           (format
                                            (format (concat "[HKEY_CLASSES_ROOT\\%%s\\Shell\\Emacs]\n"
                                                            "@=\"使用 %sEmacs 编辑\"\n"
                                                            "\"Icon\"=\"%s\"\n\n"
                                                            "[HKEY_CLASSES_ROOT\\%%s\\Shell\\Emacs\\Command]\n"
                                                            "@=\"\\\"%s\\\" -n -na \\\"%s\\\" \\\"%%%%V\\\"\"\n")
                                                    (if (string-match-p "Background" subpath) "" "&")
                                                    (c-bin t) (c-bin t) (s-bin t))
                                            subpath subpath))
                                         '("*" "Directory" "Directory\\Background")))
                           "\n\n"))
      ;; [org-Protocol]
      (insert "
[HKEY_CLASSES_ROOT\\org-protocol]
@=\"URL:Org Protocol\"
\"URL Protocol\"=\"\"
[HKEY_CLASSES_ROOT\\org-protocol\\shell]
[HKEY_CLASSES_ROOT\\org-protocol\\shell\\open]
[HKEY_CLASSES_ROOT\\org-protocol\\shell\\open\\command]" (format "
@=\"\\\"%s\\\" \\\"%%1\\\"\"\n\n" (c-bin t))))

    ;; Manually Step
    (when (yes-or-no-p (format "Saved in %s. View ?" (emacs-file "\\")))
      (w32-shell-execute "open" (emacs-file ".")))))

(defun sl/emacs-current-build-configure ()
  (interactive)
  (let ((cmd (format "./configure %s" system-configuration-options)))
    (kill-new cmd)
    (message cmd)))


;;; Batch convert encodings

(defun sl:convert-encodings-in-directory ()
  "Emacs style batch mode / change coding under the dir to UTF8."
  (im/walk-directory ("/var/www/" ".*\\.rb")
    (with-current-buffer (find-file-noselect it)
      (when (or (eq buffer-file-coding-system 'chinese-gbk-dos)
                (eq buffer-file-coding-system 'chinese-gbk-unix))
        (set-buffer-file-coding-system 'utf-8)
        (save-buffer)))))


;;; Convert postgres to mysql

(defun sl:sql-pg-to-mysql ()
  "Convert sql statement from Postgres to MySQL. Open the sql file, execute it, you will get the result."
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (flush-lines "^SET\\|^SELECT\\|^GRANT\\|OWNER TO")
    (im:replace-all-in-buffer
     '(("\"" . "`")
       ("public\\." . "")
       ("character varying" . "varchar")
       ("timestamp without time zone\\|timestamp" . "datetime")
       ("TABLE ONLY" . "TABLE")))))

(cl-defun sl:dump-pg-to-mysql (table-name dest-file &optional (dbname "imdata"))
  "Dump table data from local Postgres to SQL file can be used by MySQL."
  (let ((cmd "pg_dump -U postgres %s -t %s --insert --column-inserts"))
    (setq cmd (format cmd dbname table-name))
    (if (file-exists-p dest-file)
        (message "File-already exist, rename first.")
      (with-temp-file dest-file
        (call-process-shell-command cmd nil (current-buffer))
        (sl:sql-pg-to-mysql))
      (find-file dest-file))))


;;; ssh/nat

(defun:when sl/generate-ssh-config-file (executable-find "ssh") ()
  "Generate .ssh/config file."
  (interactive)
  (let ((file "~/.ssh/config")
        (create-lockfiles nil))
    (with-current-buffer (find-file file)
      (goto-char (point-max))
      (when (> (buffer-size) 10)
        (insert "\n##### content new added #####\n\n"))
      (insert "Host *
  ControlMaster auto
  ControlPath ~/.ssh/master-%C
  ControlPersist no\n
Host github.com
  ProxyCommand nc -v -x 127.0.0.1:11180 %h %p")
      (pop-to-buffer (current-buffer)))))

(defun:when sl/generate-nat-traverse-cmd (executable-find "cygrunsrv") (name vps inner)
  (interactive (list (read-string "Please input [SERVICE_NAME]: ")
                     (read-string "Please Input [PUBLIC:PORT]: ")
                     (read-string "Please Input [INNER:PORT]: ")))
  (let ((vps-arr (split-string vps ":")))
    (insert (format "cygrunsrv -I ah_%s -p /usr/bin/autossh -a \"-M 0 -o ServerAliveInterval=30 -o ServerAliveCountMax=3 -o ExitOnForwardFailure=yes -i ~/.ssh/id_rsa -NR %s:%s root@%s\" -y tcpip"
                    name (cl-second vps-arr) inner (cl-first vps-arr)))))


;;; OS

(defun sl/edit-hosts-file ()
  "Edit hosts file in Linux/Windows."
  (interactive)
  (let ((post (lambda (buf)
                (with-current-buffer buf
                  (when (= (point) 1)
                    (re-search-forward "^[^#]\\{3\\}" nil t)
                    (beginning-of-line)))))
        h1 h2)
    (cond
     ((file-exists-p (setq h1 "/etc/hosts"))
      (funcall post (find-file (concat "/sudo::" h1))))
     ((file-exists-p (setq h2 "C:/Windows/system32/drivers/etc/hosts"))
      (funcall post (find-file-read-only h2))
      (start-process-shell-command (format-time-string "host-%s") nil (concat "wsudo notepad " h2)))
     (t (user-error "No hosts file found for current system")))))

(defun:when sl/systemd-new-unit (executable-find "systemctl") ()
  "Snippet for new systemd unit file."
  (interactive)
  (let* ((dir (expand-file-name
               (read-directory-name "Location: " "/etc/systemd/system/" nil t)))
         (default-directory (if (string-match-p "^/home/" dir) dir
                              (format "/sudo::%s" dir)))
         (name (format "%s.service" (cl-gensym "systemd-unit-"))))
    (unless (file-exists-p default-directory)
      (user-error "Maybe `%s' is not available" default-directory))
    (with-current-buffer (get-buffer-create name)
      (insert "[Unit]
Description=unit 1
After=network.target network-online.target
#Wants/After/Before=docker.service\n
[Service]
#User/Group=vip
#Type=simple/forking/oneshot/notify/dbus/idle
#Environment=PS1=1
ExecStart=/usr/bin/x
#ExecReload=/bin/kill -HUP $MAINPID
#ExecStartPre/ExecStartPost/ExecStop/ExecStopPost=
#Restart/RestartSec/PIDFile/TimeoutSec/KillMode\n
[Install]
WantedBy=multi-user.target")
      (systemd-mode)
      (pop-to-buffer (current-buffer)))))

(defun:when sl/windows-install-scoop-or-chocolatey IS-WIN ()
  (interactive)
  (with-current-buffer (pop-to-buffer "*scoop-or-chocolatey-installation*")
    (erase-buffer)
    (insert "# Scoop Install

## Setup
Set-ExecutionPolicy RemoteSigned -scope CurrentUser;
$env:SCOOP='C:\\Scoop'
[Environment]::SetEnvironmentVariable('SCOOP', $env:SCOOP, 'User')

## Install
iwr -useb get.scoop.sh | iex
#### if error, add hosts:
199.232.68.133 raw.githubusercontent.com
#### or use mirror like:
iwr -useb https://gitee.com/RubyKids/scoop-cn/raw/master/install.ps1 | iex

## [OPT] Config
scoop install aria2
scoop config aria2-enabled true

## Install
scoop update
scoop install git
scoop install sudo

## Extra Repos
scoop bucket known
scoop bucket add extras
scoop update



# Chocolatey Install

setx ChocolateyInstall C:\\Choco
Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

choco config set proxy http://127.0.0.1:1081
cinst ag dropbox ccleaner vcredist2012 sysinternals jdk8 -y
")
    (powershell-mode)
    (im:make-buffer-buriable)
    (goto-char (point-min))))

(defun:when sl/kill-system-process (executable-find "ps") ()
  (interactive)
  (pdd-let* ((lines (pdd-exec [ps auxwe] :as 'line))
             (line (completing-read "Process to kill: " (cdr (await lines)) nil t))
             (strs (split-string line))
             (proc-id (cadr strs))
             (proc-name (car (last strs))))
    (when (y-or-n-p (format "Kill process: %s ?" proc-name))
      (pdd-exec `(kill -9 ,proc-id)
        :done (lambda (_) (message "DONE."))
        :fail (lambda (r) (message "[Fail] %s" r))))))


;;; Misc

(defun sl/youtube-dl-url (&optional url)
  "Run \\='youtube-dl' over the URL. If URL is nil, use URL at point."
  (interactive (list
                (let ((up (thing-at-point-url-at-point)))
                  (or up (read-string "youtube url: ")))))
  (if (zerop (length url))
      (user-error "URL not found")
    (require 'eshell)
    (let ((eshell-buffer-name "*youtube-dl*")
          (directory (seq-find (lambda (dir)
                                 (and (file-directory-p dir) (expand-file-name dir)))
                               '("~/temp") ".")))
      (eshell)
      (when (eshell-interactive-process) (eshell t))
      (eshell-interrupt-process)
      (insert (format " cd '%s' && yt-dlp " directory) url)
      (eshell-send-input))))
