;;; -*- lexical-binding: t -*-

;; - 2021.05.17, remove `vterm` completely. It's so annoying.
;; - 2022.11.29, maybe `eat` + `eshell` is what I want!

;;; Code:

(defvar im.eshell-extra-aliases nil)

(xzz eshell
  :config
  (setopt eshell-history-size 1024
          eshell-hist-ignoredups t
          eshell-aliases-file (loce "share/eshell.alias")
          comint-scroll-show-maximum-output nil
          eshell-scroll-show-maximum-output nil
          ;; eshell-visual-commands ...
          eshell-destroy-buffer-when-process-dies t)

  (defun:after eshell-read-aliases-list//extra (&rest _)
    (when (and im.eshell-extra-aliases (not (get 'im.eshell-extra-aliases 'done)))
      (setopt eshell-command-aliases-list (append im.eshell-extra-aliases eshell-command-aliases-list))
      (put 'im.eshell-extra-aliases 'done t)))

  (with-eval-after-load 'em-hist
    (define-key eshell-hist-mode-map [(meta ?s)] nil))

  (with-eval-after-load 'esh-mode
    (define-key eshell-mode-map (kbd "C-r") 'eshell/h)
    (define-key eshell-mode-map (kbd "C-c M-o") (lambdi () (recenter-top-bottom 0))))

  (defun:hook eshell-mode-hook/capf ()
    (add-hook 'completion-at-point-functions 'im:eshell-cd-competion-at-point nil 'local)))

(defun eshell/! (&rest args)
  "Combination eshell and native-shell-command !"
  (let ((cmd (eshell-flatten-and-stringify args)))
    (shell-command-to-string cmd)))

(defun eshell/h ()
  (interactive)
  (require 'em-hist)
  (let* ((start-pos (save-excursion (beginning-of-line) (point)))
         (end-pos (point))
         (buf (current-buffer))
         (input (buffer-substring-no-properties start-pos end-pos))
         (hists (progn
                  (when (> (ring-size eshell-history-ring) 0)
                    (setq eshell-history-ring
                          (ring-convert-sequence-to-ring
                           (delete-dups (ring-elements eshell-history-ring)))))
                  (cl-loop for e in (ring-elements eshell-history-ring) for i from 0
                           collect (propertize e 'ring-idx i))))
         (ret (minibuffer-with-setup-hook
                  (lambda ()
                    (use-local-map (make-composed-keymap nil (current-local-map)))
                    (local-set-key (kbd "C-k")
                                   (lambda ()
                                     (interactive)
                                     (when-let* ((idx (get-pos-property 0 'ring-idx (im:completion-compat :current))))
                                       (when (with-current-buffer buf (ring-remove eshell-history-ring idx))
                                         (im:completion-compat :delete))))))
                (completing-read "Command: " (im:completion-table hists)
                                 nil nil nil nil (if (cl-plusp (length input)) input (car hists))))))
    (delete-region start-pos end-pos)
    (insert ret)))

(defun im:eshell-cd-competion-at-point ()
  (when (eobp)
    (let ((beg (save-excursion (beginning-of-line) (point))) (end (point)))
      (when (string-match-p "^cd[^ ]*$" (buffer-substring beg end))
        (let* ((sortfn (lambda (x y)
                         (let ((x1 (elt x 0)) (y1 (elt y 0)))
                           (if (equal x1 y1)
                               (string< (cl-subseq x 1) (cl-subseq y 1))
                             (< (pcase x1 (?~ 0) (?/ 200) (_ x1))
                                (pcase y1 (?~ 0) (?/ 200) (_ y1)))))))
               (normfn (lambda (dirs &optional type sortp)
                         (require 'flymake-proc)
                         (cl-loop for dir in dirs
                                  if (and dir (file-exists-p dir)
                                          (not (cl-member dir (list "~/" default-directory) :test #'flymake-proc--same-files)))
                                  collect (let ((d (abbreviate-file-name (file-name-as-directory dir))))
                                            (if type (propertize d 'type type) d))
                                  into lst
                                  finally (return (if sortp (sort lst sortfn) lst)))))
               (bufdirs (cl-loop for buf in (buffer-list)
                                 for dir = (with-current-buffer buf default-directory)
                                 if (get-buffer-window buf) collect dir into highs
                                 else collect dir into lows
                                 finally (return (cons highs lows))))
               (others (list (loce "") (locc) org-directory agenda-directory
                             im.workdir im.srcdir im.downdir (if IS-WIN (getenv "USERPROFILE"))))
               (collection (cl-remove-duplicates
                            (append (funcall normfn (car bufdirs) "c")
                                    (when-let* ((d (project-root (project-current)))) (funcall normfn (list d) "p"))
                                    (funcall normfn (cdr bufdirs) "b" t)
                                    (funcall normfn others nil t))
                            :from-end t :test #'string=)))
          (list (+ beg 2) end
                (lambda (input pred action)
                  (if (eq action 'metadata)
                      `(metadata (display-sort-function . ,#'identity)
                                 (annotation-function . (lambda (c) (concat "        " (or (get-pos-property 1 'type c) "-")))))
                    (complete-with-action action collection input pred)))
                :exit-function (lambda (_c s)
                                 (when (equal s 'finished)
                                   (save-excursion (beginning-of-line) (forward-char 2) (insert " "))))))))))

(xzz eat
  :ref ("https://codeberg.org/akib/emacs-eat")
  :if IS-LINUX
  :config
  (setopt eat-enable-blinking-text t
          eat-enable-yank-to-terminal t
          eat-enable-kill-from-terminal t
          eat-kill-buffer-on-exit t)
  (define-key eat-char-mode-map [f1] nil)
  (define-key eat-char-mode-map [f12] nil)
  (define-key eat-eshell-char-mode-map [f1] nil)
  (define-key eat-eshell-char-mode-map [f12] nil))

(when IS-LINUX
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (cl-defmethod im:local-assist ((_ (eql 'eat-mode)))
    (if eat--semi-char-mode (eat-char-mode) (eat-semi-char-mode)))

  (cl-defmethod im:local-assist ((_ (eql 'eshell-mode)))
    (if eat--eshell-semi-char-mode (eat-eshell-char-mode) (eat-eshell-semi-char-mode))))



(xzz term
  "Use EShell + Eat instead."
  :init (setenv "TERM" "xterm-256color"))

(xzz shell
  :config
  (when (and IS-WIN (executable-find "bash"))
    ;; use bash.exe as default process if possible
    (setenv "PS1" "\\u@\\h \\w $ ")
    (setopt explicit-shell-file-name (executable-find "bash"))
    (setopt explicit-bash.exe-args '("--login" "-i")))

  (defun:hook shell-mode-hook ()
    (setq-local corfu-auto-delay 1.2)
    (when (and IS-WIN ;; cmd/powershell on windows, should be gbk encoding
               (not (string-match-p "bash" (or explicit-shell-file-name ""))))
      (im/local-encoding 'cp936-dos)))

  (defun:around sh-set-shell (orig-fun &rest args)
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
  (let ((type (completing-read "Open: "
                               (im:completion-table
                                '(eat term shell eshell powershell system-default))
                               nil t)))
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
  (if im.system-terminal
      (let ((d (or default-directory "~/")))
        (start-process-shell-command (format "sys-terminal-%s" d) nil (format im.system-terminal d))
        (message "OK."))
    (user-error "Please config `im.system-terminal' first")))
