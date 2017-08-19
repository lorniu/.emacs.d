;;;==============================
;;;   external | miscellaneous...
;;;==============================


(defun im/initialize-emacs-for-win ()
  "Initialization ContextMenu for Windows, etc:
1. execute this function, and import the generated `.reg` file to os;
2. put the symlink of `runemacs.exe` with arg `--daemon` to `shell:start`
3. put the symlink of `emacsclientw.exe` with arg `-n -c -a ""` to Desktop "
  (interactive)
  (unless (eq system-type 'windows-nt) (error "NO-WINNT, this is useless!"))

  (flet ((win-path (n path) (replace-regexp-in-string "/" (if (> n 1) "\\\\" "\\") (file-truename path) t t)))
    (let* ((emacs-home (win-path 2 (car (split-string exec-directory "libexec"))))
           (reg-file    (concat emacs-home "OpenWithEmacs.reg"))
           (reg-string  "Windows Registry Editor Version 5.00\n
[HKEY_CLASSES_ROOT\\*\\shell\\emacs]\n@=\"Edit &With GNU Emacs\"\n
[HKEY_CLASSES_ROOT\\*\\shell\\emacs\\Command]\n@=\"\\\"%s\\\" -n -na \\\"%s\\\" \\\"%%1\\\"\" ")
           (buffer-file-coding-system 'gbk))

      ;; SETENV: emacs-server-file
      (start-process "a" "f" "setx" "EMACS_SERVER_FILE"
         (win-path 1 (concat server-auth-dir "server")))

      ;; Generate ContextMenu.reg    
      (with-temp-file reg-file
        (insert (format reg-string
                        (concat emacs-home "bin\\\\" "emacsclientw.exe")
                        (concat emacs-home "bin\\\\" "runemacs.exe"))))

      ;; Import manually please.
      (if (yes-or-no-p (format "已保存为 %s。跳转？" reg-file))
        (w32-shell-execute "open" emacs-home)))))




;;; WALK DEMO
(defmacro im/with-directory-buffers(dir filter &rest form)
  "walk the directory, operate each with current buffer."
  (declare (indent defun))
  (let ((file (gensym)))
    `(save-window-excursion
       (dolist (,file (directory-files-recursively ,dir (or ,filter "*")))
         (with-current-buffer (find-file ,file)
           (unwind-protect (progn ,@form)
             (if (buffer-modified-p)
                 (save-buffer)
               (kill-buffer)))))
       (message "\n||| FINISHED |||\n") 1)))

(defun im/refactor-encoding-in-directory ()
  "emacs style batch mode / change coding under the dir"
  (dolist (file (directory-files-recursively "/var/www/" ".*\.rb"))
    (with-current-buffer (find-file file)
      (when (or (eq buffer-file-coding-system 'chinese-gbk-dos)
                (eq buffer-file-coding-system 'chinese-gbk-unix))
        (set-buffer-file-coding-system 'utf-8)
        (save-buffer))
      (kill-buffer))))


;;;
;;; third party and unused BEGIN HERE
;;;
;; save or restore window configurations.
(defvar winstack-stack '()
  "A Stack holding window configurations. Use `winstack-push' and `winstack-pop' to modify it.")
(defun winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (first winstack-stack))
           (compare-window-configurations (first winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
           (message (concat "pushed " (number-to-string (length (window-list (selected-frame)))) " frame config")))))
(defun winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))
;;;
;;; third party and unused END HERE






(provide 'imsilly)

;;; imsilly.el ends here
