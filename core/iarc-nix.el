;;; -*- lexical-binding: t -*-

(defun:when si/systemd-new-unit (executable-find "systemctl") ()
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
