;;; -*- lexical-binding: t -*-

;; Interactive Keyboard Macros: press `C-x q' when recording.

;;; Code:

(defun im/kmacros (arg)
  "Keyboard macros. All in one."
  (interactive "p")
  (require 'consult-kmacro)
  (let* ((cs (consult-kmacro--candidates)) ns
         (prop (lambda (c) (propertize (format "%s" c) 'face 'font-lock-function-name-face)))
         (cmdp (lambda (s) (member (intern s) ns))))
    (mapatoms (lambda (m) (when (kmacro-keyboard-macro-p m) (push m ns))))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((map (make-composed-keymap nil (current-local-map))))
            (define-key map (kbd "C-w") ; copy kmacro/definition
                        (lambda ()
                          (interactive)
                          (when-let* ((cand (im:completion-compat :current)))
                            (with-temp-buffer
                              (if (funcall cmdp cand)
                                  (insert-kbd-macro (intern cand) t)
                                (insert cand))
                              (kill-ring-save (point-min) (point-max))
                              (when (funcall cmdp cand)
                                (delete-region (point-min) (progn (goto-char (point-min)) (search-forward "(kmacro \"") (point)))
                                (delete-region (progn (goto-char (point-max)) (search-backward "\"") (point)) (point-max)))
                              (throw 'miniquit `(exit . ,(concat "Copied success: " (funcall prop (buffer-string)))))))))
            (define-key map (kbd "C-r") ; name kmacro
                        (lambda ()
                          (interactive)
                          (when-let* ((cand (im:completion-compat :current)))
                            (let ((km (or (consult--lookup-candidate cand cs) (user-error "You should select a kmacro without named")))
                                  (name (read-string "Name for this kbd macro: " "km/")))
                              (if (string-equal name "") (error "No command name given"))
                              (setq name (intern name))
                              (and (fboundp name) (not (kmacro-keyboard-macro-p name))
                                   (error "Function %s is already defined and not a keyboard macro" name))
                              (fset name km)
                              (put name 'kmacro t)
                              (throw 'miniquit `(exit . ,(concat "Named kmacro as: " (funcall prop name))))))))
            (define-key map (kbd "C-o") ; open/edit kmacro
                        (lambda ()
                          (interactive)
                          (when-let* ((cand (im:completion-compat :current)))
                            (cond ((funcall cmdp cand) (throw 'miniquit `(edit . ,(intern cand))))
                                  ((= (im:completion-compat :index) 0) (throw 'miniquit `(edit)))
                                  (t (user-error "Can't edit current one"))))))
            (use-local-map map)))
      (let* ((completion-ignore-case t)
             (minibuffer-allow-text-properties t)
             (cands (append
                     (and (car cs) (list (propertize (car cs) 'face 'font-lock-variable-name-face)))
                     (cdr cs)
                     (mapcar prop ns)))
             (ret (catch 'miniquit (completing-read "Keyboard macros (C-w/C-r/C-o): "
                                                    (lambda (input pred action)
                                                      (if (eq action 'metadata)
                                                          `(metadata (display-sort-function . ,#'identity)
                                                                     (annotation-function . ,(lambda (cand) (get-text-property 0 'consult-kmacro--annotation cand))))
                                                        (complete-with-action action cands input pred)))
                                                    nil t nil 'consult-kmacro--history))))
        (if (consp ret)
            (if (eq (car-safe ret) 'edit)
                ;; edit kbd macro
                (if-let* ((s (cdr ret)))
                    (cl-letf (((symbol-function 'read-command) (lambda (&rest _) s)))
                      (edit-named-kbd-macro))
                  (edit-last-kbd-macro))
              ;; message if necessary
              (when (stringp (cdr-safe ret))
                (message (cdr ret))))
          (if (funcall cmdp ret)
              ;; call named kbd macro
              (let ((sym (intern ret)))
                (funcall sym arg)
                (kmacro-push-ring)
                (setq last-kbd-macro (kmacro--keys (symbol-function sym))))
            ;; call kbd macro in ring
            (if (equal ret (car cs))
                (call-last-kbd-macro arg)
              (let ((macro (consult--lookup-candidate ret cs)))
                (funcall (kmacro macro) arg)
                (kmacro-push-ring)
                (setq last-kbd-macro (kmacro--keys macro))))))))))

;; Don't push last-kbd-maro to ring if it has named
(defun:around kmacro-push-ring//avoid-push-when-named-kmacro-exists (fn &rest args)
  (unless (and (null (car args))
               (catch 'ret (mapatoms
                            (lambda (m)
                              (when (and (kmacro-keyboard-macro-p m) (equal (kmacro--keys (symbol-function m)) last-kbd-macro))
                                (throw 'ret t))))))
    (apply fn args)))

;; Undo with only one-step; avoid completion break the input
(dolist (fn '(kmacro-call-macro kmacro-exec-ring-item dot-mode-execute apply-macro-to-region-lines))
  (advice-add fn :around (lambda (fn &rest args) (let ((corfu-auto-delay 99)) (with-undo-amalgamate (apply fn args))))))


;;; Persistence

(defalias 'km/normaize-gradle
  (kmacro "M-m C-s ' C-b <C-backspace> <C-backspace> C-s ' C-s C-s <C-backspace> <C-backspace> <C-backspace> <C-backspace> : C-s ' C-s RET <C-backspace> <C-backspace> <C-backspace> <C-backspace> : M-m"))

(defalias 'km/sqlplus-wrap-rownum
  (kmacro "M-m s e l e c t SPC * SPC f r o m SPC ( SPC C-e M-SPC ) SPC w h e r e SPC r o w n u m SPC < SPC 5"))

(defalias 'km/wrap-kmacro-with-definition
  (kmacro "M-m ( d e f a l i a s SPC ' k m / n a m e SPC ( k m a c r o SPC \" C-e \" ) ) M-m M-f C-f"))
