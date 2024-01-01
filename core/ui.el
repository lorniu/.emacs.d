;;; -*- lexical-binding: t -*-

;; Config global faces:
;;
;;   (setq im.faces
;;         `((aaa :title "imfine"
;;                :font "LXGW WenKai Mono 17"
;;                :font-cn "SimSun-9:bold:lang=zh"
;;                :font-emoji "twemoji"
;;                :height 200
;;                :line-spacing 0.1
;;                :mode-line-height 120
;;                :theme zero-dark
;;                :bound max or (x y nil h)
;;                :alpha 88
;;                :hook (lambda() ...))
;;           (bbb* :...))) ; label with [.*~-] as suffix is used as default
;;
;;   (setq im.faces '(:height 200 :font "xxx")) ; for simple
;;
;; Notice:
;;
;;   - :height 200 is slow, prefer use :font "xxx-SIZE" instead
;;

;;; Code:

(defcustom im.faces nil
  "((aaa  :title x :font FontName-Size :alpha 98 :bound max :theme c)
    (bbb* :font-cn SimSun :height 200))"
  :type 'list
  :group 'imfine)

(defvar f/current--face nil)

(defvar f/current--fontset nil)

(defvar f/params--backup nil)

(defun f/get (item &optional default)
  (when (and im.faces (atom (car im.faces)))
    (setq im.faces (list (cons 'default im.faces))))
  (let ((faces (cdr (or (and (string-match-p "^[0-9]+$" (concat (getenv-internal "face")))
                             (nth (string-to-number (getenv-internal "face")) im.faces))
                        (cl-find-if (lambda (item)
                                      (string-match-p "[.*~-]$" (format "%s" (car item))))
                                    im.faces)
                        (car im.faces)))))
    (cond
     ((or (null item) (eq item 'def)) faces)
     ((not (keywordp item)) (cdr (assoc item im.faces)))
     (t (let* ((faces-default (list :title nil :theme nil :font nil))
               (faces-merged (im:plist-merge faces-default faces))
               (value (or (plist-get faces-merged item) default)))
          (pcase item
            (:alpha
             (cons 'alpha-background (or value 100)))
            (:bound
             (pcase value
               ('max '((fullscreen . maximized)))
               (_ (cl-loop for x in '(width height top left) for y in value
                           if y collect (cons x y)))))
            ((or :font :font-cn :font-emoji)
             (if (and value (font-info value)) value))
            (_ value)))))))

(defun f/read-attributes (attrs)
  (let* ((fas (if (facep attrs) (face-all-attributes 'default) attrs))
         (old (read-string "Face attributes: " (substring (prin1-to-string fas) 1 -1))))
    (read (format "(%s)" old))))

(defun f/mode-line-height (n)
  (if (and (numberp n) (<= n 0))
      (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))
    (set-face-attribute 'mode-line nil :height n)
    (set-face-attribute 'mode-line-inactive nil :height n)))

;; Commands

(defun f/setup (param value)
  "Setup frame parameters and others interactively."
  (interactive (let* ((nps '(alpha alpha-background line-spacing font left-fringe right-fringe))
                      (cps '(background-color foreground-color mouse-color border-color cursor-color))
                      (others '(*preset* mode-line-height height weight cursor-type theme indent-width/offset))
                      (group (lambda (p f)
                               (if f p
                                 (cond ((member p '("*preset*" "theme")) "global")
                                       ((member p '("indent-width/offset")) "buffer")
                                       (t "frame")))))
                      (param (intern (completing-read "Setup: "
                                                      (lambda (input pred action)
                                                        (if (eq action 'metadata) `(metadata (group-function . ,group))
                                                          (complete-with-action action (append nps cps others) input pred)))
                                                      nil t nil 'f/setup--history)))
                      (origin (frame-parameter nil param))
                      (value (cond
                              ((memq param '(font)))
                              ((string-match-p "-color$" (symbol-name param))
                               (read-color (format "%s: " param)))
                              ((memq param nps)
                               (read-number (format "New value of %s: " param) origin))
                              ((memq param cps)
                               (read-string (format-prompt "New value of %s" origin param) origin))
                              ((eq param 'cursor-type)
                               (intern (completing-read (format-prompt "New value of %s" (frame-parameter nil 'cursor-type) param)
                                                        '(t nil box hollow bar hbar))))
                              ((eq param 'height)
                               (read-number (format "New value of %s: " param) (face-attribute 'default :height)))
                              ((eq param 'weight)
                               (intern (completing-read (format-prompt "New value of %s" (frame-parameter nil 'weight) param)
                                                        (im:completion-table '(thin extra-light light semi-light normal medium semi-bold bold ultra-bold heavy ultra-heavy)))))
                              ((eq param '*preset*)
                               (intern (completing-read "Preset: " im.faces #'cdr t)))
                              ((eq param 'mode-line-height)
                               (if current-prefix-arg 0
                                 (read-number "Set hight of mode line to: "
                                              (let ((mh (face-attribute 'mode-line :height)))
                                                (if (equal mh 'unspecified)
                                                    (let ((dh (face-attribute 'default :height)))
                                                      (if (equal dh 'unspecified) 100 dh))
                                                  mh))))))))
                 (list param value)))
  (pcase param
    ('*preset*
     (let ((preset (f/get (or value 'def))))
       (cl-loop for (k . v) in f/params--backup do (set-frame-parameter nil k v))
       (f/mode-line-height 'unspecified)
       (when-let* ((theme (plist-get preset :theme)))
         (mapc #'disable-theme custom-enabled-themes)
         (load-theme theme t))
       (cl-loop for (k v) on preset by #'cddr do
                (pcase k
                  ((or :theme :title :font :font-cn :font-emoji))
                  (:alpha (f/setup 'alpha-background v))
                  (:height (set-face-attribute 'default nil k v))
                  (:mode-line-height (f/mode-line-height v))
                  (_ (f/setup (intern (substring (symbol-name k) 1)) v))))
       (set-frame-font (or (plist-get preset :font) (face-attribute 'default :font)))
       (let ((fontset (face-attribute 'default :fontset)))
         (set-fontset-font fontset 'han (plist-get preset :font-cn))
         (set-fontset-font fontset 'emoji (plist-get preset :font-emoji)))))
    ('font (call-interactively #'f/frame-font))
    ('theme (call-interactively #'consult-theme))
    ('indent-width/offset (call-interactively #'im/set-indent-width-or-offset))
    ((or 'height 'weight)
     (set-face-attribute 'default nil (intern (format ":%s" param)) value))
    ('mode-line-height (f/mode-line-height value))
    (_ (set-frame-parameter nil param value))))

(defun f/local-theme (theme &optional frame)
  "Active THEME only for specific FRAME. To be improved."
  (interactive (list (intern (completing-read "Load custom theme: "
                                              (mapcar #'symbol-name
				                                      (custom-available-themes))))
                     (selected-frame)))
  (unless (eq theme 'standard-light)
    (f/local-theme 'standard-light))
  (require-theme (intern (format "%s-theme" theme)))
  (pcase-dolist (`(,type ,face _ ,val) (get theme 'theme-settings))
    (let (face-attrs)
      (when (and (eq type 'theme-face)
	             (not (eq 0 (setq face-attrs (face-spec-choose val frame 0)))))
        (ignore-errors (face-spec-set-2 face frame face-attrs))))))

;; Font Commands, for different scopes.

(defun f/find-font (&optional regexp all current not-required)
  "Find font with name matching REGEXP.
With current-prefix-arg non-nil, find the font name only."
  (interactive)
  (require 'face-remap)
  (let* ((interactivep (called-interactively-p 'interactive))
         (briefp (and interactivep current-prefix-arg))
         (fonts (x-list-fonts "*"))
         (families (mapcar (lambda (f) (cons f (plist-get (font-face-attributes f) :family))) fonts)))
    (when briefp
      (setq fonts (delete-dups (mapcar #'cdr families))))
    (when regexp
      (setq fonts (cl-remove-if-not (lambda (f) (string-match-p regexp f)) fonts)))
    (if (or interactivep (and (null regexp) (null all)))
        (let* ((completion-ignore-case t)
               (current (find-font
                         (apply #'font-spec
                                (if current
                                    (if (facep current)
                                        (font-face-attributes (face-font current))
                                      current)
                                  (if (consp buffer-face-mode-face)
                                      buffer-face-mode-face
                                    (font-face-attributes (frame-parameter nil 'font)))))))
               (family (format "%s" (font-get current :family)))
               (mlen (apply #'max (mapcar #'length fonts)))
               (fonts (mapcar (lambda (f)
                                (cons f (ignore-errors (file-name-directory (aref (font-info f) 12)))))
                              (cl-sort fonts (if current-prefix-arg
                                                 #'string<
                                               (lambda (x y)
                                                 (cond ((equal family (cdr (assoc x families))))
                                                       ((equal family (cdr (assoc y families))) nil)
                                                       (t (string-lessp x y))))))))
               (group (lambda (f flag)
                        (if flag f (cdr (assoc f fonts)))))
               (annotation (lambda (f)
                             (format "%s%s"
                                     (make-string (max 0 (- mlen (length f))) ? )
                                     (propertize "你好，Hello_Emacs,123~10oO.liLI"
                                                 'face `(:foreground "skyblue" ,@(font-face-attributes f))))))
               (table (lambda (input _ action)
                        (pcase action
                          ('metadata `(metadata (group-function . ,group)
                                                (annotation-function . ,annotation)
                                                (display-sort-function . ,#'identity)))
                          (`(boundaries . _) nil)
                          (_ (let* ((str (minibuffer-contents))
                                    (prefix (and (string-match "^\\(/[^ ]*\\)?" str) (match-string 1 str)))
                                    (pred (lambda (f)
                                            (or (seq-empty-p prefix)
                                                (and (not (seq-empty-p (cdr f)))
                                                     (string-match-p (substring prefix 1) (cdr f))))))
                                    (completion-regexp-list (remove prefix completion-regexp-list))) ; /prefix only work for style `orderless'
                               (complete-with-action action fonts input pred))))))
               (font (completing-read "Font ([/win] PATTERN): "
                                      table nil (not not-required) nil nil
                                      (unless not-required (if briefp family (font-xlfd-name current))))))
          (when interactivep
            (kill-new font)
            (unless current-prefix-arg (kill-new (plist-get (font-face-attributes font) :family)))
            (message "%s" font))
          font)
      (if all fonts (car fonts)))))

(defun f/global-font (font)
  "Alias of `set-frame-font', set FONT for all frames."
  (interactive (list (f/find-font)))
  (set-frame-font font current-prefix-arg t))

(defun f/frame-font (font)
  "Alias of `set-frame-font', set FONT for current frame."
  (interactive (list (f/find-font)))
  (set-frame-font font current-prefix-arg)
  (let ((f/current--fontset (face-attribute 'default :fontset)))
    (call-interactively #'f/fontset-font)))

(defun f/face-font (face font &optional fontset frame)
  "Alias of `set-face-font', set font/fontset for specific FACE.
With current-prefix-arg non-nil, edit before commit."
  (interactive (let* ((face (or f/current--face
                                (read-face-name "Set face: " (face-at-point t))))
		              (font (f/find-font nil nil face))
                      (fontsets (let ((ls (fontset-list))
                                      (cr (face-attribute face :fontset)))
                                  (if (ignore-errors (fontset-name-p cr))
                                      (cl-delete-duplicates (cons cr ls) :from-end t)
                                    ls)))
                      (fontset (completing-read "Fontset (for Non-ASCII): "
                                                (im:completion-table fontsets))))
	             (list face font fontset)))
  (if (seq-empty-p fontset)
      (setq fontset nil)
    (if (fontset-name-p fontset)
        (unless (string-match-p "^fontset-" fontset)
          (setq fontset (aref (x-decompose-font-name fontset) 11)))
      (unless (string-match "^\\(?:fontset-\\)?\\([a-z][a-z0-9_-]*\\)$" fontset)
        (user-error "Wrong fontset name: %s" fontset))
      (create-fontset-from-ascii-font font nil (match-string 1 fontset))
      (unless (string-prefix-p "fontset-" fontset)
        (setq fontset (concat "fontset-" fontset)))
      (let ((f/current--fontset (query-fontset fontset)))
        (call-interactively #'f/fontset-font))))
  (set-face-attribute face frame :font 'unspecified :fontset 'reset :family 'unspecified)
  (let ((fas `(:font ,font)) (fs (format "%s" face)))
    (if fontset (setq fas (append fas `(:fontset ,fontset))))
    (if current-prefix-arg (setq fas (f/read-attributes fas)))
    (if fas (apply #'set-face-attribute face frame fas))
    (message "Set font of %s to %s"
             (propertize (if (string-prefix-p "loc-face-" fs) "local buffer" fs) 'face 'bold)
             (propertize (substring (prin1-to-string fas) 1 -1) 'face 'success))))

(defun f/buffer-font (face)
  "Alias of `buffer-face-set', set font/fontset for current buffer.
With current-prefix-arg non-nil, edit before commit."
  (interactive (list (let* ((existp
                             (and buffer-face-mode-face
                                  (string-match-p "^loc-face-" (symbol-name buffer-face-mode-face))))
                            (f/current--face
                             (unless existp
                               (make-face (intern (concat "loc-face-" (md5 (buffer-name))))))))
                       (when f/current--face
                         (apply #'set-face-attribute f/current--face nil
                                (font-face-attributes (face-attribute 'default :font)))
                         (call-interactively #'f/face-font))
                       f/current--face)))
  (setq-local buffer-face-mode-face face)
  (buffer-face-mode t)
  (unless face (message "Buffer-mode-face disabled.")))

(defun f/fontset-font (fontset charset font)
  "Alias of `set-fontset-font', set FONT of FONTSET for specific CHARSET."
  (interactive (let ((fs (or f/current--fontset
                             (completing-read "Fontset to set: " (fontset-list) nil t
                                              nil nil (face-attribute 'default :fontset))))
                     (rg (ignore-errors (read (completing-read "Range to set: " '(chinese han unicode emoji symbol cjk-misc))))))
                 (list fs rg (when rg (f/find-font nil nil nil t)))))
  (unless (or (null charset) (stringp charset))
    (if (seq-empty-p font) (setq font nil))
    (if (eq charset 'chinese)
        (cl-loop for r in '(han cjk-misc) do (set-fontset-font fontset r font))
      (set-fontset-font fontset charset font))
    (when (called-interactively-p 'any)
      (message "Set %s in %s to: %s"
               (propertize (format "%s" charset) 'face 'bold)
               (propertize (aref (x-decompose-font-name fontset) 11) 'face 'bold)
               (if font (propertize font 'face 'success))))))

(defun f/region-font (font)
  "Wrapper of `put-text-property', set FONT for current region.
With current-prefix-arg non-nil, edit before commit."
  (interactive (list (f/find-font)))
  (if (use-region-p)
      (let ((fas (if (ignore-errors (font-info font)) (font-face-attributes font) font)))
        (if current-prefix-arg (setq fas (f/read-attributes fas)))
        (add-text-properties (region-beginning) (region-end)
                             `(face ,fas font-lock-face ,fas)))
    (user-error "No region found")))

(imload 'ui-themes)
(imload 'ui-modeline)


;;; Reset

(setq f/params--backup
      (cl-loop for k in '(alpha alpha-background cursor-color cursor-type)
               collect (cons k (frame-parameter nil k))))

(setq menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)

(setq-default frame-title-format
              (f/get :title `((:eval (cond ((equal (elt (buffer-name) 0) ? ) "Nix") (t "%b"))))))


;;; Windows

(when IS-WIN
  (setq default-frame-alist
        `((menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (scroll-bar . nil)
          (vertical-scroll-bars . nil)
          ,(f/get :alpha) ,@(f/get :bound '(80 35)))))


;;; Linux/BSD

(when IS-LINUX
  (setq default-frame-alist
        `((menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (scroll-bar . nil)
          (vertical-scroll-bars . nil)
          ,(f/get :alpha) ,@(f/get :bound '(110 35)))))

(when IS-NG
  (setf (alist-get 'menu-bar-lines default-frame-alist) 0)
  (unless (f/get :theme) (setf (plist-get (cdar im.faces) :theme) 'origin))
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambdi () (scroll-down 1)))
  (global-set-key [mouse-5] (lambdi () (scroll-up 1))))


;;; macOS

(when IS-MAC
  (setq default-frame-alist
        `((menu-bar-lines . 100)
          (tool-bar-lines . 0)
          (vertical-scroll-bars . nil)
          ,(f/get :alpha) ,@(f/get :bound '(100 45)))))


;;; Others from custom

(when-let* ((ft (f/get :font)))
  (set-face-attribute 'default nil :font ft))

(when-let* ((ft (f/get :font-cn)))
  (cl-loop for r in '(han cjk-misc) do (set-fontset-font "fontset-default" r ft)))

(when-let* ((ft (f/get :font-emoji)))
  (set-fontset-font "fontset-default" 'emoji ft))

(cl-loop for (k v) on (f/get 'def) by #'cddr
         if (eq k :height) do (set-face-attribute 'default nil :height v)
         if (eq k :mode-line-height) do (f/mode-line-height v)
         unless (memq k '(:font :font-cn :font-emoji :mode-line-height :title :hook :theme :alpha :bound))
         do (set-frame-parameter nil (intern (substring (symbol-name k) 1)) v))

(ignore-errors (funcall (f/get :hook)))
