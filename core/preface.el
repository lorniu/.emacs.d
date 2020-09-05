;;; preface.el --- Faces load early -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun ff/get (item &optional default)
  (let* ((faces-default (list :title "hi" :theme nil :font nil :font-unicode nil :font-mono '("Ubuntu Mono" "隶书")))
         (faces-merged (plist-merge faces-default ic/faces)))
    (or (plist-get faces-merged item) default)))


;;; Font Utils

(defun ff/find-font (&rest names)
  "Find the proper font in NAMES."
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

(defun ff/princ-fonts (regexp)
  "Print the suitable fonts."
  (interactive (list (read-string "Regexp to filter fonts: ")))
  (when (plusp (length regexp))
    (if-let ((fs (cl-delete-if-not (lambda (f) (string-match-p regexp f))
                                   (cl-delete-duplicates (font-family-list) :test #'string-equal))))
        (message "--- %s ---\n%s" (length fs) (mapconcat #'identity fs "\n"))
      (message "No font found [%s]" regexp))))

(defun ff/set-line-height (num-of-pixes)
  "Add extra height(pixes) to line height."
  (interactive (list (read-number "Extra pixes between lines: ")))
  (setq-default line-spacing (if (< num-of-pixes 1) nil num-of-pixes)))

;; global fonts

(defun ff/font-default (&optional font height)
  "Set system default font."
  (interactive)
  (if (consp font) (setq height (cdr font) font (car font)))
  (let ((ft (ff/find-font (unless (called-interactively-p 'any) font)))
        (fh (if (called-interactively-p 'any) (read-number "Font Height: " ff/font-face-height) height))
        (args '(default nil)))
    (if ft (setq args (append args (list :family ft))))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (apply 'set-face-attribute args)
    (if (called-interactively-p 'any) (message "Set default font: %s" args) args)))

(defun ff/font-unicode (&optional font height type)
  "Set the font of Unicode symbols, like 'han for Chinese."
  (interactive)
  (let* ((type (or type 'unicode))
         (ft (ff/find-font (unless (called-interactively-p 'any) font)))
         (fh (if (called-interactively-p 'any) (read-number "Font Height: " ff/font-face-height) height))
         (args))
    (if ft (setq args (append args (list :family ft))))
    (if (and fh (> fh 0)) (setq args (append args (list :size (/ fh 10.0)))))
    (set-fontset-font "fontset-default" type (apply 'font-spec args))
    (if (called-interactively-p 'any) (message "Set unicode font: %s" args) args)))

;; face or buffer-local font, especially for Monospace scene.

(defvar ff/font-face-height 0)
(defvar ff/font-face-family (ff/get :font-mono)) ; Mono as default
(defvar ff/font-face-extras '())

(defun ff/font-face (&rest face-or-faces)
  "Set face font globally"
  (interactive (list (read-face-name "Face: " (or (face-at-point t t) "default"))))
  (unless face-or-faces (setq face-or-faces '(default)))
  (let* ((ft (ff/find-font (unless (called-interactively-p 'any) ff/font-face-family)))
         (fh (if (called-interactively-p 'any) (read-number "Font Height: " ff/font-face-height) ff/font-face-height))
         (args (list :family ft)))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (if ff/font-face-extras (setq args (append args ff/font-face-extras)))
    (dolist (face face-or-faces)
      (setq args (append `(,face nil) args))
      (apply 'set-face-attribute args))
    (when (called-interactively-p 'any)
      (message "%s global to: %s" face-or-faces args))))

(defun ff/font-face-buffer-local (&rest face-or-faces)
  "Set face font buffer local"
  (interactive (list (read-face-name "Face: " (or (face-at-point t t) "default"))))
  (unless face-or-faces (setq face-or-faces '(default)))
  (let* ((ft (ff/find-font (unless (called-interactively-p 'any) ff/font-face-family)))
         (fh (if (called-interactively-p 'any) (read-number "Font Height: " ff/font-face-height) ff/font-face-height))
         (args (list :family ft)))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (if ff/font-face-extras (setq args (append args ff/font-face-extras)))
    (dolist (face face-or-faces)
      (setq args (cons face args))
      (apply 'face-remap-add-relative args))
    (when (called-interactively-p 'any)
      (message "%s local to: %s" face-or-faces args))))


;;; Windows

(when IS-WIN
  (setq default-frame-alist
        (append
         `((title  . ,(ff/get :title))
           (alpha  . ,(ff/get :alpha 100))
           (menu-bar-lines . 0)
           (tool-bar-lines . 0)
           (scroll-bar . nil)
           (vertical-scroll-bars . nil)
           (cursor-type  . box)
           (cursor-color . "red"))
         (cl-case (ff/get :frame)
           (max '((fullscreen . maximized)))
           (otherwise `((top    . ,(ff/get :top 50))
                        (left   . ,(ff/get :left 50))
                        (width  . ,(ff/get :width 80))
                        (height . ,(ff/get :height 35))))))))


;;; Linux/BSD

(when IS-LINUX
  (setq default-frame-alist
        `((title  . ,(ff/get :title))
          (height . ,(ff/get :height 35))
          (width  . ,(ff/get :width 110))
          (alpha  . ,(ff/get :alpha 100))
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (scroll-bar . nil)
          (vertical-scroll-bars . nil))))


;;; macOS

(when IS-MAC
  (setq default-frame-alist
        `((menu-bar-lines . 100)
          (vertical-scroll-bars . nil))))


(provide 'preface)

;;; preface.el ends here
