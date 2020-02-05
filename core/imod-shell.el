;;; imod-shell.el --- Shells -*- lexical-binding: t -*-

;; - 2021.05.17, remove `vterm` completely. It's so annoying.

;;; Code:

(x term
   :init
   (setenv "TERM" "xterm-256color")

   :defer-config
   (defun my-term-send-forward-word () (interactive) (term-send-raw-string "\ef"))
   (defun my-term-send-backward-word () (interactive) (term-send-raw-string "\eb"))
   (define-key term-raw-map [C-left] 'my-term-send-backward-word)
   (define-key term-raw-map [C-right] 'my-term-send-forward-word)

   (defun:hook term-mode-hook ()
     (goto-address-mode 1))

   (defun:after term-handle-exit$ (&rest _)
     (use-local-map (let ((map (make-sparse-keymap))) (define-key map "q" #'kill-current-buffer) map))))

(x shell
   :init
   (when (and IS-WIN (executable-find "bash"))
     ;; use bash.exe as default process if possible
     (setenv "PS1" "\\u@\\h \\w $ ")
     (setq explicit-shell-file-name (executable-find "bash"))
     (setq explicit-bash.exe-args '("--login" "-i")))

   (defun:hook shell-mode-hook ()
     (setq-local corfu-auto-delay 1.2)
     (when (and IS-WIN ;; cmd/powershell on windows, should be gbk encoding
                (not (string-match-p "bash" (or explicit-shell-file-name ""))))
       (im/local-encoding 'cp936-dos))
     (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))

   :defer-config
   (defun:around sh-set-shell$ (orig-fun &rest args)
     "Dont show message: Indentation setup for shell type bash"
     (cl-letf (((symbol-function 'message) #'ignore))
       (apply orig-fun args))))

(x eshell/i
   :init
   (setq eshell-history-size 1024
         eshell-hist-ignoredups t
         eshell-aliases-file (loce "share/eshell.alias")
         comint-scroll-show-maximum-output nil
         eshell-scroll-show-maximum-output nil
         eshell-destroy-buffer-when-process-dies t)

   ;; Visual Commands
   (setq eshell-visual-commands '("vim" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"))
   (setq eshell-visual-subcommands '(("sudo" "vi" "visudo")
                                     ("git" "log" "diff" "show" "reflog")
                                     ("sbt")))
   (setq eshell-visual-options '(("git" "--amend")))

   ;; Detach
   (defun:hook eshell-mode-hook/detach ()
     (if (not (executable-find "dtach"))
         (cl-loop for key in '("C-c C-z" "C-<return>" "S-<return>")
                  with fn = (lambda () (interactive) (message "Please ensure 'dtach' installed."))
                  do (define-key eshell-mode-map (kbd key) fn))
       (require 'eshell-detach)
       (define-key eshell-mode-map (kbd "C-c C-z") 'eshell-detach-stop)
       (define-key eshell-mode-map (kbd "S-<return>") 'eshell-detach-send-input)
       (define-key eshell-mode-map (kbd "C-<return>") 'eshell-detach-attach)))

   ;; Patch (make `git log` and others work normally)
   (defun:around eshell-term-sentinel$avoid-sudden-quit (f &rest arg)
     (let (eshell-destroy-buffer-when-process-dies) (apply f arg)))

   :defer-config
   (with-eval-after-load 'em-hist
     (define-key eshell-hist-mode-map [(meta ?s)] nil))

   (with-eval-after-load 'esh-mode
     (define-key eshell-mode-map (kbd "C-r") 'eshell/h)
     (define-key eshell-mode-map (kbd "C-c M-o") (lambda () (interactive) (recenter-top-bottom 0))))

   (defun eshell/! (&rest args)
     "Combination eshell and native-shell-command !"
     (let ((cmd (eshell-flatten-and-stringify args)))
       (shell-command-to-string cmd)))

   (defun eshell/h ()
     (interactive)
     (require 'em-hist)
     (let* ((start-pos (save-excursion (eshell-bol) (point)))
            (end-pos (point))
            (input (buffer-substring-no-properties start-pos end-pos))
            (hists (delete-dups (when (> (ring-size eshell-history-ring) 0)
                                  (ring-elements eshell-history-ring))))
            (command (completing-read "Command: " hists nil nil nil nil (if (cl-plusp (length input)) input (car hists)))))
       (setf (buffer-substring start-pos end-pos) command)
       (end-of-line)))

   (defun eshell/ssh (&rest args)
     "Secure shell."
     (let ((cmd (eshell-flatten-and-stringify (cons "ssh" args)))
           (display-type (framep (selected-frame))))
       (cond
        ((and (eq display-type 't) (getenv "STY"))
         (send-string-to-terminal (format "\033]83;screen %s\007" cmd)))
        (t (apply 'eshell-exec-visual (cons "ssh" args)))))))



(defun im/popup-shell (&optional _)
  (interactive "P")
  (if (eq major-mode 'shell-mode)
      (bury-buffer)
    (let ((curr-dir default-directory))
      (shell "+shell+")
      (ring-insert comint-input-ring
                   (concat "cd " (shell-quote-argument curr-dir))))))

(defun im/popup-eshell (&optional arg)
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (bury-buffer)
    (let ((last-file (or dired-directory (buffer-file-name))))
      (eshell arg)
      (if last-file (eshell-add-to-dir-ring (file-name-directory last-file))))))

(defun im/popup-xshell ()
  (interactive)
  (let ((type (completing-read "Shell: " '(powershell system-default) nil t)))
    (if (string= type "powershell")
        (powershell)
      (let ((explicit-shell-file-name (getenv "SHELL")))
        (shell "+shell+")))))

(defun im/popup-system-terminal ()
  (interactive)
  (if ic/system-terminal
      (let ((d (or default-directory "~/")))
        (start-process-shell-command (format "sys-terminal-%s" d) nil (format ic/system-terminal d))
        (message "OK."))
    (user-error "Please config `ic/system-terminal' first.")))

(provide 'imod-shell)

;;; imod-shell.el ends here
