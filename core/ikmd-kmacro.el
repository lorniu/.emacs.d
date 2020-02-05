;;; ikmd-kmacro.el --- Keymacros -*- lexical-binding: t -*-

;;; Code:

(dolist (fn '(kmacro-call-macro
              kmacro-exec-ring-item
              dot-mode-execute
              apply-macro-to-region-lines))
  (advice-add fn :around #'block-undo-advice))

(defmacro xm (name keys)
  (declare (indent 1))
  `(fset ',name (lambda (&optional arg)
                  "keyboard macro"
                  (interactive "p")
                  (kmacro-exec-ring-item `(,,(if (stringp keys) `(kbd ,keys) keys) 0 "%d") arg))))

(defun name-and-insert-last-kbd-macro (macro-name)
  (interactive
   (list (intern (completing-read
                  "Insert kbd macro (name): " obarray
                  (lambda (elt) (and (fboundp elt) (or (stringp (symbol-function elt)) (vectorp (symbol-function elt)) (get elt 'kmacro))))))))
  (fset macro-name last-kbd-macro)
  (insert (format "(fset '%s\n      (kbd \"%s\"))\n"
                  (prin1-to-string macro-name)
                  (key-description (symbol-function macro-name)))))

(defmacro read-insert (&rest args) `(insert (read-string ,@args)))


;;; keymacros

(xm k/normaize-gradle
  "M-m C-s ' C-b <C-backspace> <C-backspace> C-s ' C-s C-s <C-backspace> <C-backspace> <C-backspace> <C-backspace> : C-s ' C-s RET <C-backspace> <C-backspace> <C-backspace> <C-backspace> : M-m")

(xm k/sqlplus-wrap-rownum
  "C-a s e l e c t SPC * SPC f r o m SPC ( SPC C-e C-b SPC ) SPC w h e r e SPC r o w n u m SPC < SPC 5")

(provide 'ikmd-kmacro)

;;; ikmd-kmacro.el ends here
