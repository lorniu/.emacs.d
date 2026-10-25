;;; -*- lexical-binding: t -*-

(defun si/emacs-generate-integration-configs-for-windows ()
  "Generate `Integrate-With-Emacs.reg' for Windows Explorer integration."
  (interactive)
  (let* ((escape (lambda (path &optional quote)
                   (replace-regexp-in-string "/" (if quote "\\" "\\\\") path t t)))
         (clientw (funcall escape (expand-file-name "emacsclientw.exe" invocation-directory)))
         (runemacs (funcall escape (expand-file-name "runemacs.exe" invocation-directory)))
         (server (funcall escape (expand-file-name "server" server-auth-dir) t))
         (target (locc "Integrate-With-Emacs.reg"))
         (coding-system-for-write 'gbk))

    ;; Compact env vars
    (dolist (env `(("EMACS_SERVER_FILE" . ,server)
                   ("MSYSTEM" . "UCRT64")
                   ("MSYS2_PATH_TYPE" . "inherit")))
      (call-process "setx" nil nil nil (car env) (cdr env)))

    ;; Generate reg file
    (with-current-buffer (find-file-noselect target)
      (erase-buffer)
      (insert
       "Windows Registry Editor Version 5.00\n\n"

       ;; [alias.eee.exe]
       (format "[HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\eee.exe]\n@=\"%s\"\n\n"
               runemacs)

       ;; [default-alternate-editor]
       (format "[HKEY_CURRENT_USER\\SOFTWARE\\GNU\\Emacs]\n\"ALTERNATE_EDITOR\"=\"%s\"\n\n"
               runemacs)

       ;; [open-with-emacs]
       (cl-loop for sub in '("*" "Directory" "Directory\\Background")
                concat (format "[HKEY_CLASSES_ROOT\\%s\\Shell\\Emacs]\n@=\"使用 %sEmacs 编辑\"\n\"Icon\"=\"%s\"\n\n"
                               sub (if (string-match-p "Background" sub) "" "&") clientw)
                concat (format "[HKEY_CLASSES_ROOT\\%s\\Shell\\Emacs\\Command]\n@=\"\\\"%s\\\" -n \\\"%s\\\"\"\n\n"
                               sub clientw (if (string-match-p "Background" sub) "%V" "%1")))

       ;; [org-protocol]
       (format "[HKEY_CLASSES_ROOT\\org-protocol]
@=\"URL:Org Protocol\"\n\"URL Protocol\"=\"\"\n
[HKEY_CLASSES_ROOT\\org-protocol\\shell]
[HKEY_CLASSES_ROOT\\org-protocol\\shell\\open]
[HKEY_CLASSES_ROOT\\org-protocol\\shell\\open\\command]
@=\"\\\"%s\\\" \\\"%%1\\\"\"\n\n" clientw)

       ;; [shadow-stock-context-items]
       (cl-loop for item in '("batfile" "cmdfile" "regfile" "JSFile" "JSEFile" "VBSFile" "VBEFile" "WSFFile" "Windows.XamlDocument" "Windows.Xbap" "Paint.Picture" "PBrush" "RDP.File" "group_wab_auto_file" "Applications\\notepad.exe" "Applications\\mspaint.exe" "SystemFileAssociations\\text" "SystemFileAssociations\\image" "SystemFileAssociations\\.ps1" "SystemFileAssociations\\.psd1" "SystemFileAssociations\\.psm1")
                concat (format "[HKEY_CURRENT_USER\\Software\\Classes\\%s\\shell\\edit]\n" item))
       "\"LegacyDisable\"=\"\"\n\n"
       "[HKEY_CURRENT_USER\\Software\\Classes\\Folder\\shell\\opennewwindow]\n\"MUIVerb\"=\"在新窗口打开\"\n\n"

       ;; [input-method-hot-keys]
       "[-HKEY_CURRENT_USER\\Control Panel\\Input Method\\Hot Keys\\00000010]\n"
       "[-HKEY_CURRENT_USER\\Control Panel\\Input Method\\Hot Keys\\00000070]")
      (save-buffer))

    ;; Manually Step
    (find-file target)
    (when (yes-or-no-p (format "Saved in `%s'! Open in explorer?" target))
      (w32-shell-execute "open" (file-name-directory target)))))
