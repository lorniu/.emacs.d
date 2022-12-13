;;; imod-shell.el --- Shells -*- lexical-binding: t -*-

;; - 2021.05.17, remove `vterm` completely. It's so annoying.
;; - 2022.11.29, maybe `eat` + `eshell` is what I wanted!

;;; Code:

(x eat
   "Eat EShell."
   :ref ("https://codeberg.org/akib/emacs-eat")
   :if IS-LINUX
   :init
   (setq eat-enable-blinking-text t
         eat-enable-yank-to-terminal t
         eat-enable-kill-from-terminal t
         eat-kill-buffer-on-exit t)

   (add-hook 'eshell-first-time-mode-hook #'eat-eshell-visual-command-mode)
   (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode)

   :defer-config
   (cl-defmethod im/mode-action ((m (eql 'eat-mode)))
     (if eat--semi-char-mode (eat-char-mode) (eat-semi-char-mode)))
   (cl-defmethod im/mode-action ((m (eql 'eshell-mode)))
     (if eat--eshell-semi-char-mode (eat-eshell-char-mode) (eat-eshell-semi-char-mode)))
   (define-key eat-char-mode-map [f1] nil)
   (define-key eat-char-mode-map [f12] nil)
   (define-key eat-eshell-char-mode-map [f1] nil)
   (define-key eat-eshell-char-mode-map [f12] nil))

(x eshell/i
   :init
   (setq eshell-history-size 1024
         eshell-hist-ignoredups t
         eshell-aliases-file (loce "share/eshell.alias")
         comint-scroll-show-maximum-output nil
         eshell-scroll-show-maximum-output nil
         eshell-destroy-buffer-when-process-dies t)
   :defer-config
   ;; (setq eshell-visual-commands ...)

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
       (end-of-line))))



(x term
   "Use EShell + Eat instead."
   :init (setenv "TERM" "xterm-256color"))

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
     "Dont show messages: Indentation setup for shell type bash"
     (cl-letf (((symbol-function 'message) #'ignore))
       (apply orig-fun args))))



(defun im/toggle-eat-buffer ()
  "Toggle show the *eat* buffer."
  (interactive)
  (im/hide-or-show-buffer "*eat*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (eat (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))))

(defun im/toggle-eshell-buffer ()
  "Toggle show the *EShell* buffer."
  (interactive)
  (im/hide-or-show-buffer "*eshell*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (call-interactively 'eshell))))

(defun im/popup-xshell ()
  (interactive)
  (let* ((vertico-sort-function nil)
         (type (completing-read "Open: " '(eat term shell eshell powershell system-default) nil t)))
    (pcase (intern type)
      ('eat (eat (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))
      ('shell (call-interactively 'shell))
      ('eshell (call-interactively 'eshell))
      ('powershell (call-interactively 'powershell))
      ('term (call-interactively 'term))
      ('system-default (im/popup-system-terminal)))))

(defun im/popup-system-terminal ()
  "Open system terminal."
  (interactive)
  (if ic/system-terminal
      (let ((d (or default-directory "~/")))
        (start-process-shell-command (format "sys-terminal-%s" d) nil (format ic/system-terminal d))
        (message "OK."))
    (user-error "Please config `ic/system-terminal' first.")))

(provide 'imod-shell)

;;; imod-shell.el ends here
