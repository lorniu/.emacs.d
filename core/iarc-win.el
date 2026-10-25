;;; -*- lexical-binding: t -*-

(defun si/emacs-generate-integration-configs-for-windows ()
  "Generate `Integrate-With-Emacs.reg' for Windows Explorer integration."
  (interactive)
  (let* ((escape (lambda (path &optional quote)
                   (replace-regexp-in-string "/" (if quote "\\" "\\\\") path t t)))
         (clientw (funcall escape (expand-file-name "emacsclientw.exe" invocation-directory)))
         (runemacs (funcall escape (expand-file-name "runemacs.exe" invocation-directory)))
         (server (funcall escape (expand-file-name "server" server-auth-dir) t))
         (target-dir (locc "Initial"))
         (target-ps1 (expand-file-name "1-Integrate-With-Emacs.ps1" target-dir))
         (target-reg (expand-file-name "2-Integrate-With-Emacs.reg" target-dir))
         (path-bin (file-truename "c:/apps/bin"))
         (coding-system-for-write 'gbk))

    ;; Ensure directory
    (make-directory target-dir t)

    ;; Generate ps1 file
    (with-temp-file target-ps1
      (insert
       (format "
Write-Host '[1/3] Set MSYS2_PATH_TYPE...'
[Environment]::SetEnvironmentVariable('MSYS2_PATH_TYPE','inherit','User')

Write-Host '[2/3] Set EMACS_SERVER_FILE...'
[Environment]::SetEnvironmentVariable('EMACS_SERVER_FILE','%s','User')

Write-Host '[3/3] Create alias.eee to emacsclientw.exe...'
winls \"MSYSTEM=UCRT64 %s\" \"%s\\\\eee\" -f -0 \"MSYSTEM=UCRT64 %s\"

Write-Host
Write-Host '[SUCCESS] PRESS ANY KEY TO QUIT!'
[Console]::ReadKey($true)"
               server clientw (funcall escape path-bin) runemacs)))

    ;; Generate reg file
    (with-temp-file target-reg
      (insert
       "Windows Registry Editor Version 5.00\n\n"

       "; set default alternate editor\n\n"
       (format "[HKEY_CURRENT_USER\\SOFTWARE\\GNU\\Emacs]\n\"ALTERNATE_EDITOR\"=\"%s\"\n\n"
               runemacs)

       "; open-with-emacs\n\n"
       (cl-loop for sub in '("*" "Directory" "Directory\\Background")
                concat (format "[HKEY_CLASSES_ROOT\\%s\\Shell\\Emacs]\n@=\"使用 %sEmacs 编辑\"\n\"Icon\"=\"%s\"\n\n"
                               sub (if (string-match-p "Background" sub) "" "&") clientw)
                concat (format "[HKEY_CLASSES_ROOT\\%s\\Shell\\Emacs\\Command]\n@=\"\\\"%s\\\" -n \\\"%s\\\"\"\n\n"
                               sub clientw (if (string-match-p "Background" sub) "%V" "%1")))

       "; integrate with org-protocol\n\n"
       (format "[HKEY_CLASSES_ROOT\\org-protocol]
@=\"URL:Org Protocol\"\n\"URL Protocol\"=\"\"\n
[HKEY_CLASSES_ROOT\\org-protocol\\shell]
[HKEY_CLASSES_ROOT\\org-protocol\\shell\\open]
[HKEY_CLASSES_ROOT\\org-protocol\\shell\\open\\command]
@=\"\\\"%s\\\" \\\"%%1\\\"\"\n\n" clientw)

       "; shadow Edit when right click on file\n\n"
       (cl-loop for item in '("batfile" "cmdfile" "regfile" "JSFile" "JSEFile" "VBSFile" "VBEFile" "WSFFile" "Windows.XamlDocument" "Windows.Xbap" "Paint.Picture" "PBrush" "RDP.File" "group_wab_auto_file" "Applications\\notepad.exe" "Applications\\mspaint.exe" "SystemFileAssociations\\text" "SystemFileAssociations\\image" "SystemFileAssociations\\.ps1" "SystemFileAssociations\\.psd1" "SystemFileAssociations\\.psm1")
                concat (format "[HKEY_CURRENT_USER\\Software\\Classes\\%s\\shell\\edit]\n" item))
       "\"LegacyDisable\"=\"\"\n\n"

       "; shadow open-in-new-window when right click on folder\n\n"
       "[HKEY_CURRENT_USER\\Software\\Classes\\Folder\\shell\\opennewwindow]\n\"MUIVerb\"=\"在新窗口打开\"\n\n"

       "; disable ctrl-space and ctrl-shift\n\n"
       "[-HKEY_CURRENT_USER\\Control Panel\\Input Method\\Hot Keys\\00000010]\n"
       "[-HKEY_CURRENT_USER\\Control Panel\\Input Method\\Hot Keys\\00000070]"))

    ;; Manually Step
    (find-file target-dir)
    (run-with-timer 0.3 nil (lambda ()
                              (message "Saved in `%s'!\n
1. Make sure `%s' is on the system PATH.
2. Execute the two files to config the system." target-dir path-bin)))))
