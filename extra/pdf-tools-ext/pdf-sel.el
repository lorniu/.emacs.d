;;; pdf-sel.el --- Double click to choose current word -*- lexical-binding: t -*-

;;; Code:

(defvar pdf-sel-mode-map nil
  "Keymap for `pdf-sel-mode'.")

(setq pdf-sel-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [double-mouse-1] 'pdf-sel-mouse)
        map))

(define-minor-mode pdf-sel-mode
  "\\<pdf-sel-mode-map>Just binding \\[pdf-sel-mouse] to `pdf-sel-mouse'.
`pdf-sel-mouse' selects the text at point and copies it to `kill-ring'."
  :keymap pdf-sel-mode-map)

(defvar pdf-view-active-region) ;; defined in "pdf-view.el"

(defun pdf-sel-mouse (ev)
  "Select word at mouse event EV and copy it to `kill-ring'."
  (interactive "@e")
  (let* ((posn (event-start ev))
         (xy (posn-object-x-y posn))
         (size (pdf-view-image-size))
         (page (pdf-view-current-page))
         (x (/ (car xy) (float (car size))))
         (y (/ (cdr xy) (float (cdr size)))))
    (setq pdf-view-active-region (pdf-info-getselection page (list x y x y) 'word))
    (pdf-view-display-region pdf-view-active-region)
    (kill-new (pdf-info-gettext page (list x y x y) 'word))))

(provide 'pdf-sel)

;;; pdf-sel.el ends here
