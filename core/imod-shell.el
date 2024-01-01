;;; -*- lexical-binding: t -*-

;; - 2021.05.17, remove `vterm` completely. It's so annoying.
;; - 2022.11.29, maybe `eat` + `eshell` is what I want!
;; - 2026.06.20, eat -> ghostel

;;; Code:

(xzz shell
  :config
  (when (and IS-WIN (executable-find "bash"))
    ;; use bash.exe as default process if possible
    (setenv "PS1" "\\u@\\h \\w $ ")
    (setq explicit-shell-file-name (executable-find "bash"))
    (setq explicit-bash.exe-args '("--login" "-i")))

  (defun:hook shell-mode-hook ()
    (setq-local corfu-auto-delay 1.2)
    (when (and IS-WIN ;; cmd/powershell on windows, should be gbk encoding
               (not (string-match-p "bash" (or explicit-shell-file-name ""))))
      (im/local-encoding 'cp936-dos)))

  (defun:around sh-set-shell (orig-fun &rest args)
    "Dont show messages: Indentation setup for shell type bash"
    (cl-letf (((symbol-function 'message) #'ignore))
      (apply orig-fun args))))

(xzz term
  :init (setenv "TERM" "xterm-256color"))

(defvar im.eshell-extra-aliases nil)

(xzz eshell
  :config
  (setq eshell-history-size 1024
        eshell-hist-ignoredups t
        eshell-aliases-file (loce "share/eshell.alias")
        comint-scroll-show-maximum-output nil
        eshell-scroll-show-maximum-output nil
        ;; eshell-visual-commands ...
        eshell-destroy-buffer-when-process-dies t)

  (defun:after eshell-read-aliases-list//extra (&rest _)
    (when (and im.eshell-extra-aliases (not (get 'im.eshell-extra-aliases 'done)))
      (setq eshell-command-aliases-list (append im.eshell-extra-aliases eshell-command-aliases-list))
      (put 'im.eshell-extra-aliases 'done t)))

  (with-eval-after-load 'em-hist
    (define-key eshell-hist-mode-map [(meta ?s)] nil))

  (with-eval-after-load 'esh-mode
    (define-key eshell-mode-map (kbd "C-r") 'eshell/h)
    (define-key eshell-mode-map (kbd "C-c M-o") (lambda () (interactive) (recenter-top-bottom 0))))

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



(xzz ghostel
  :if (not IS-WIN)
  :ref "dakra/ghostel"
  :bind (:map ghostel-semi-char-mode-map
              ("C-k"  . (lambda ()
                          (interactive)
                          (kill-ring-save (point) (line-end-position))
                          (ghostel-send-key "k" "ctrl")))
              ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
              ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
              ("M-<backspace>" . ghostel-backward-kill-word))
  :init
  (add-hook 'ghostel-mode-hook #'ghostel-ime-mode)
  (add-hook 'after-init-hook #'ghostel-compile-global-mode)
  (add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)
  (defalias 'eshell/v 'eshell/ghostel)
  :config
  (setq ghostel-buffer-name "*vterm*")
  (setq ghostel-buffer-name-function nil)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))



(defun im/toggle-vterm-buffer ()
  "Toggle show the *vterm* buffer, here is ghostel."
  (interactive)
  (if IS-WIN
      (call-interactively #'im/toggle-eshell-buffer)
    (im/hide-or-show-buffer (lambda (b) (eq (buffer-local-value 'major-mode b) 'ghostel-mode))
      (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
        (ghostel)))))

(defun im/toggle-eshell-buffer ()
  "Toggle show the *eshell* buffer."
  (interactive)
  (im/hide-or-show-buffer "*eshell*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (call-interactively 'eshell))))

(defun im/open-system-terminal ()
  "Open system terminal."
  (interactive)
  (if im.system-terminal
      (let ((d (or default-directory "~/")))
        (start-process-shell-command (format "sys-terminal-%s" d) nil (format im.system-terminal d))
        (message "OK."))
    (user-error "Please config `im.system-terminal' first")))

(defun im/terminal ()
  (interactive)
  (let ((type (completing-read "Open: "
                               (im:completion-table '(ghostel system-default eshell powershell shell term))
                               nil t)))
    (pcase (intern type)
      ('ghostel (call-interactively 'ghostel))
      ('shell (call-interactively 'shell))
      ('eshell (call-interactively 'eshell))
      ('powershell (call-interactively 'powershell))
      ('term (call-interactively 'term))
      ('system-default (im/open-system-terminal)))))
