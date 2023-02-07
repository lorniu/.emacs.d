;;; face-font.el --- Font -*- lexical-binding: t -*-

;;; Code:

;;; Utils

(defun f/find-font (&rest names)
  "Find the proper font."
  (interactive)
  (let* ((ns (if (stringp (car names)) names (car names)))
         (fonts (sort (delete-dups (mapcar (lambda (f) (decode-coding-string f 'utf-8)) (font-family-list))) 'string-lessp))
         (fonts-propertized (mapcar (lambda (f) (propertize f 'face `(:family ,f))) fonts)))
    (cond ((and (stringp (car ns)) (string= "" (car ns))) nil)
          ((or (null ns) (called-interactively-p 'any))
           (completing-read "Font: " fonts-propertized nil t nil nil
                            (plist-get (font-face-attributes (font-at (min (point) (- (point-max) 1)))) :family)))
          (t (catch 'ret
               (if (stringp ns) (setq ns (list ns)))
               (let (rs-font)
                 (dolist (name ns)
                   (setq rs-font (or (seq-find (lambda (font) (string-equal name font)) fonts)
                                     (seq-find (lambda (font) (string-match-p (format ".*%s.*" name) font)) fonts)))
                   (if rs-font (throw 'ret rs-font)))))))))

(defun f/princ-fonts (regexp)
  "Print the suitable fonts."
  (interactive (list (read-string "Regexp to filter fonts: ")))
  (when (cl-plusp (length regexp))
    (if-let (fs (cl-delete-if-not (lambda (f) (string-match-p regexp f))
                                  (cl-delete-duplicates (font-family-list) :test #'string-equal)))
        (message "--- %s ---\n%s" (length fs) (mapconcat #'identity fs "\n"))
      (message "No font found [%s]" regexp))))

(defun f/set-line-height (num-of-pixes)
  "Add extra height(pixes) to line height."
  (interactive (list (read-number "Extra pixes between lines: " line-spacing)))
  (setq-default line-spacing (if (< num-of-pixes 1) nil num-of-pixes)))

(defun f/set-mode-line-height (n)
  (interactive (list (if current-prefix-arg 0
                       (read-number "Set hight of mode line to: "
                                    (let ((mh (face-attribute 'mode-line :height)))
                                      (if (equal mh 'unspecified)
                                          (let ((dh (face-attribute 'default :height)))
                                            (if (equal dh 'unspecified) 100 dh))
                                        mh))))))
  (if (<= n 0)
      (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))
    (set-face-attribute 'mode-line nil :height n)
    (set-face-attribute 'mode-line-inactive nil :height n)
    (message "Set current mode line height to %d" n)))

;;; Global fonts

(defmacro f/font-and-height (font &optional height)
  (let ((ft `(f/find-font (unless (called-interactively-p 'any) ,font))))
    (if height
        `(list ,ft ,`(if (called-interactively-p 'any)
                         (read-number "Font Height: " (face-attribute 'default :height))
                       ,height))
      ft)))

(defun f/font-default (&optional font height)
  "Set system default font."
  (interactive)
  (if (consp font) (setq height (cdr font) font (car font)))
  (pcase-let ((`(,ft ,fh) (f/font-and-height font height))
              (args '(default nil)))
    (if ft (setq args (append args (list :family ft))))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (apply 'set-face-attribute args)
    (if (called-interactively-p 'any) (message "Set default font: %s" args) args)))

(defun f/font-unicode (&optional font height type)
  "Set the font of Unicode symbols, like 'han for Chinese."
  (interactive)
  (pcase-let* ((`(,ft ,fh) (f/font-and-height font height))
               (type (or type 'unicode))
               (args (if ft (list :family ft))))
    (if (and fh (> fh 0)) (setq args (append args (list :size (/ fh 10.0)))))
    (set-fontset-font "fontset-default" type (apply 'font-spec args))
    (if (called-interactively-p 'any) (message "Set unicode font: %s" args) args)))

(defun f/font-emoji (&optional font)
  (interactive)
  (let ((ft (f/font-and-height font)))
    (with-over
     (set-fontset-font "fontset-default" '(#x1f000 . #x1faff) (font-spec :family ft))
     (if (called-interactively-p 'any) (message "Set emoji font: %s" ft)))))

;;; Font for spcific face

(defvar f/font-face-height 0)
(defvar f/font-face-family (f/get :font-mono)) ; Mono as default
(defvar f/font-face-extras '())

(defun f/font-of-face (&rest face-or-faces)
  "Set face font globally"
  (interactive (list (read-face-name "Face: " (or (face-at-point t t) "default"))))
  (unless face-or-faces (setq face-or-faces '(default)))
  (pcase-let* ((`(,ft ,fh) (f/font-and-height f/font-face-family f/font-face-height))
               (args (list :family ft)))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (if f/font-face-extras (setq args (append args f/font-face-extras)))
    (dolist (face face-or-faces)
      (setq args (append `(,face nil) args))
      (apply 'set-face-attribute args))
    (when (called-interactively-p 'any)
      (message "%s global to: %s" face-or-faces args))))

(defun f/font-of-face-buffer-local (&rest face-or-faces)
  "Set face font buffer local"
  (interactive (list (read-face-name "Face: " (or (face-at-point t t) "default"))))
  (unless face-or-faces (setq face-or-faces '(default)))
  (pcase-let* ((`(,ft ,fh) (f/font-and-height f/font-face-family f/font-face-height))
               (args (list :family ft)))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (if f/font-face-extras (setq args (append args f/font-face-extras)))
    (dolist (face face-or-faces)
      (setq args (cons face args))
      (apply 'face-remap-add-relative args))
    (when (called-interactively-p 'any)
      (message "%s local to: %s" face-or-faces args))))

(provide 'face-font)

;;; face-font.el ends here
