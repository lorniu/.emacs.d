;;; -*- lexical-binding: t -*-

;;; Code:

(prog1 :helpers
  (defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
    (display-buffer-at-bottom buffer alist)
    (select-window (get-buffer-window buffer))
    (with-current-buffer buffer (view-mode 1)))
  (cl-macrolet ((ln:make-fn--display-buffer-in-direction-or- (other)
                  (let ((fname (intern (format "%%display-buffer-in-direction-or-%s" other)))
                        (dname (intern (format "display-buffer-%s" other))))
                    `(defun ,fname (buffer alist)
                       (if (and (> (frame-width) 120)
                                (null (window-in-direction 'right))
                                (null (window-in-direction 'left)))
                           (display-buffer-in-direction buffer alist)
                         (,dname buffer alist))))))
    (ln:make-fn--display-buffer-in-direction-or- at-bottom)
    (ln:make-fn--display-buffer-in-direction-or- below-selected)))

(setq display-buffer-alist
      `(("\\*Compile-Log\\*"            ; regexp to filter buffer-name
         (display-buffer-reuse-window)  ; functions to deal with display
         (window-height . 0.3)          ; parameters to pass to functions above
         (window-width . 0.3))

        ("\\*[cC]ompilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom))

        ("\\*sly-macroexpansion\\*\\|\\*Pp Macroexpand Output\\*"
         (display-buffer-reuse-window %display-buffer-in-direction-or-below-selected)
         (direction . right))

        ("\\*Youdao Dictionary\\*\\|\\*Help\\*\\|\\*Shell Command Output\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (reusable-frames . t))

        ("\\*\\(e?shell[-+]?\\|PowerShell\\|Python\\)\\*\\|[-+]\\(shell\\)[-+]"
         (display-buffer-same-window)
         (reusable-frames . t))

        ("\\*Async Shell Command\\*"
         (%display-buffer-at-bottom-follows-with-quit)
         (window-height . 0.3))

        ("magit: "
         (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom)
         (direction . right)
         (window-height . 0.35))

        ("\\*org-roam\\*"
         (display-buffer-in-direction)
         (direction . right)
         (window-width . 0.33)
         (window-height . fit-window-to-buffer))))


;;; Window

(x winner
   :init (winner-mode 1)
   :config
   (define-key winner-mode-map [C-left] 'winner-undo)
   (define-key winner-mode-map [C-right] 'winner-redo))

(defun ln/swap-windows ()
  (interactive)
  (if (> (length (window-list)) 2)
      (user-error "Two many windows, can not swap")
    (window-swap-states)))

(defun ln/transpose-window-layout ()
  "Switch between vertical and horizontal split. It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun ln/other-window-with-special ()
  (interactive)
  (cond
   (t (call-interactively #'other-window))))

(transient-define-prefix ln/transient-windows () ; C-x w
  :transient-non-suffix 'transient--do-exit
  [:hide
   (lambda () t)
   ("0"  "a"  delete-windows-on)
   ("1"  "b"  delete-other-windows)
   ("2"  "c"  split-root-window-below)
   ("3"  "d"  split-root-window-right)
   ("B"  "e"  balance-windows-area)
   ("h"  "f"  shrink-window-horizontally :transient t)
   ("l"  "g"  enlarge-window-horizontally :transient t)
   ("j"  "h"  shrink-window :transient t)
   ("k"  "i"  enlarge-window :transient t)
   ("n"  "l"  winner-undo :transient t)
   ("o"  "m"  ln/other-window-with-special)]
  [[("s" "swap"         ln/swap-windows)]
   [("x" "transpose"    ln/transpose-window-layout)]
   [("b" "balance"      balance-windows)]
   [("d" "dedicate"     toggle-window-dedicated)]
   [("F" "follow-mode"  follow-mode)]
   [("p" (lambda () (!tdesc "n p" "winner")) winner-redo :format "%d" :transient t)]])


;;; Toggle show

(defmacro ln/hide-or-show-buffer (buffer-predicate buffer-show-form &rest buffer-init-form)
  "Helper macro: hide or show buffer window."
  (declare (indent 1))
  `(let* ((it (cl-find-if ,(cond ((and (consp buffer-predicate) (equal (car buffer-predicate) 'lambda)) buffer-predicate)
                                 ((symbolp buffer-predicate) buffer-predicate)
                                 (t `(lambda (b) (string-equal (buffer-name b) ,buffer-predicate))))
                          (buffer-list))))
     (cond ((and ,(not (null buffer-init-form)) (null it))
            ,@buffer-init-form)
           ((and it (get-buffer-window it))
            (save-excursion (ignore-errors (delete-window (get-buffer-window it)))))
           (t ,buffer-show-form))))

(defun ln/toggle-messages-buffer()
  "Toggle show the *Messages* buffer."
  (interactive)
  (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom)
                                 (direction . right)
                                 (window-height . 0.4)
                                 (window-width . 0.4)))))
    (ln/hide-or-show-buffer "*Messages*" (call-interactively 'view-echo-area-messages))))

(defun ln/toggle-scratch-buffer (&optional ielmp)
  "Toggle show the *Scratch* buffer."
  (interactive "P")
  (ln/hide-or-show-buffer "*scratch*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom)
                                   (direction . right)
                                   (window-height . 0.4)))))
      (scratch-buffer))))

(defun ln/toggle-ielm-buffer (&optional ielmp)
  "Toggle show the *ielm* buffer."
  (interactive "P")
  (ln/hide-or-show-buffer "*ielm*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom)
                                   (direction . right)))))
      (ielm))))

(defun ln/toggle-gnus ()
  "Toggle show the *Group* buffer."
  (interactive)
  (ln/hide-or-show-buffer "*Group*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window display-buffer-in-direction)
                                   (direction . right)))))
      (display-buffer it))
    (require 'gnus-start)
    (user-error "You should start GNUS first")))

(defun ln/toggle-common-lisp-dev-buffer ()
  "Toggle show SLIME/SLY buffer."
  (interactive)
  (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-below-selected)
                                 (direction . right)))))
    (ln/hide-or-show-buffer (lambda (b) (string-match-p "^\\*sl.*-m?repl.*" (buffer-name b)))
      (pop-to-buffer it)
      (condition-case nil
          (call-interactively ln-lisp-dev)
        (error (%lisp/devenv-nofound))))))

(defun ln/toggle-diagnostics-buffer()
  "Show the error messages for flycheck or flymake."
  (interactive)
  (cond ((and (boundp 'flycheck-mode) flycheck-mode)
         (let ((buf flycheck-error-list-buffer))
           (call-interactively 'flycheck-list-errors)
           (select-window (get-buffer-window buf))))
        ((and (boundp 'flymake-mode) flymake-mode)
         (let ((buf (flymake--diagnostics-buffer-name)))
           (call-interactively 'flymake-show-diagnostics-buffer)
           (select-window (get-buffer-window buf))))
        (t (message "Nothing to do, check flycheck or flymake toggled?"))))


;;; Toggle fullscreen

(defvar ln:fullscreen-alist nil
  "Alist to save window configurations used to toggle fullscreen layout.
Item should be (win-id-list window-configuration buffers-info updated-at) style.")

(defun ln/toggle-layout (&optional arg)
  "Toggle fullscreen easily. Recommended binding to C-x 1 or F11."
  (interactive "P")
  (if (minibufferp) (exit-minibuffer))
  (cl-labels
      ((win-ids ()
         (let (ids) ; use win-ids to distinguish different layouts
           (walk-window-tree (lambda (w) (push (ln:window-id w) ids)))
           ids))
       (save-layout ()
         (let ((win-ids (win-ids)) wc)
           (setq ln:fullscreen-alist
                 (cl-remove-if (lambda (c) (equal (car c) win-ids)) ln:fullscreen-alist)) ; ensure update
           (setq wc (list win-ids
                          (current-window-configuration)
                          (mapconcat (lambda (w) (buffer-name (window-buffer w))) (window-list) ", ")
                          (time-to-seconds)))
           (setq ln:fullscreen-alist (cons wc ln:fullscreen-alist))))
       (switch-layout ()
         (cl-labels
             ((wc-item (id-string) ; get the saved layout
                (assoc (mapcar #'string-to-number (split-string id-string ", ")) ln:fullscreen-alist))
              (ids-str (ids)       ; concatenate ids as the key
                (mapconcat #'number-to-string ids ", "))
              (sort-fn (wcs)       ; sort by created/updated time
                (sort wcs (lambda (x y) (not (time-less-p (cadddr (wc-item x)) (cadddr (wc-item y))))))))
           (let* ((win-ids (win-ids))
                  (items (mapcar (lambda (wc)
                                   (let ((idstr (ids-str (car wc))))
                                     (when (equal win-ids (car wc))
                                       (setq idstr (propertize idstr 'face 'font-lock-comment-face)))
                                     (cons
                                      (concat idstr
                                              (propertize
                                               (concat
                                                (make-string (- 30 (length idstr)) ? )
                                                (format "%-30s" (format-seconds "%Y %D %H %M %z%S" (float-time (time-subtract (time-to-seconds) (cadddr wc)))))
                                                (caddr wc))
                                               'face 'font-lock-comment-face))
                                      wc)))
                                 ln:fullscreen-alist))
                  (keys (lambda ()
                          (use-local-map (make-composed-keymap nil (current-local-map)))
                          (local-set-key (kbd "C-k") (lambda ()
                                                       (interactive)
                                                       (when-let* ((current (ln:completion-compat :current)))
                                                         (setq ln:fullscreen-alist
                                                               (cl-remove-if (lambda (item) (equal (cadr (assoc current items)) (car item))) ln:fullscreen-alist))
                                                         (throw 'fullscreen-minibuffer 'del))))
                          (local-set-key (kbd "C-S-k") (lambda ()
                                                         (interactive)
                                                         (setq ln:fullscreen-alist nil)
                                                         (throw 'fullscreen-minibuffer 'clear)))))
                  (choosen (if (length= items 1)
                               (car items)
                             (minibuffer-with-setup-hook keys
                               (catch 'fullscreen-minibuffer
                                 (unless items (user-error "Nop"))
                                 (completing-read "Layout: "
                                                  (lambda (input pred action)
                                                    (if (eq action 'metadata)
                                                        `(metadata (category . toggle-fullscreen)
                                                                   (display-sort-function . ,#'sort-fn))
                                                      (complete-with-action action items input pred)))
                                                  nil t nil nil (car items)))))))
             (cond ((eq choosen 'del) (ln/toggle-layout 'reopen))
                   ((eq choosen 'clear) (message "Clear done."))
                   (t (if-let* ((wc (cadr (cdr (assoc choosen items))))) (set-window-configuration wc))))))))
    ;; single window: try to choose and switch to another layout
    (if (equal (selected-window) (next-window))
        (if (zerop (length ln:fullscreen-alist))
            (user-error "No saved window configuration found")
          (switch-layout))
      (if arg
          ;; call with prefix: switch layout only
          (progn
            (save-layout)
            (switch-layout))
        ;; multiple windows: add/update before delete others windows
        (save-layout)
        (let ((ignore-window-parameters t))
          (delete-other-windows))))))
