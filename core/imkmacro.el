;;; imkmacro.el --- Keybinds
;;; Commentary:

;;; Code:


(defmacro xm (name keys)
  (declare (indent 1))
  `(fset ',name (lambda (&optional arg) "keyboard macro" (interactive "p")
                  (save-undo (kmacro-exec-ring-item `(,,(if (stringp keys) `(kbd ,keys) keys) 0 "%d") arg)))))

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

(xm km/normaize-gradle
  "M-m C-s ' C-b <C-backspace> <C-backspace> C-s ' C-s C-s <C-backspace> <C-backspace> <C-backspace> <C-backspace> : C-s ' C-s RET <C-backspace> <C-backspace> <C-backspace> <C-backspace> : M-m")

;;; fake commands

(defun km/generate-nat-traverse (name vps inner)
  (interactive (list (read-string "Please input [SERVICE_NAME]: ")
                     (read-string "Please Input [PUBLIC:PORT]: ")
                     (read-string "Please Input [INNER:PORT]: ")))
  (let ((vps-arr (split-string vps ":")))
    (insert (format "cygrunsrv -I ah_%s -p /usr/bin/autossh -a \"-M 0 -o ServerAliveInterval=30 -o ServerAliveCountMax=3 -o ExitOnForwardFailure=yes -i ~/.ssh/id_rsa -NR %s:%s root@%s\" -y tcpip"
                    name (second vps-arr) inner (first vps-arr)))))

(provide 'imkmacro)

;;; imkmacro.el ends here.
