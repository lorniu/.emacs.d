;;; face.el --- Faces and locale -*- lexical-binding: t -*-

;; Config global faces:
;;
;;   (setq ic/faces
;;         `(:theme 'zero-dark/wombat
;;           :font '("JetBrain Mono" . 140)
;;           :font-unicode '("Sarasa Mono SC" . nil)
;;           :title "TITLE-TO-SHOW"))
;;
;; Config some faces demo (or via customize-face):
;;
;;   (with-over
;;    (set-face-attribute 'mode-line nil :height 140 :font "Ubuntu Mono")
;;    (set-face-attribute 'mode-line-inactive nil :height 140 :font "Ubuntu Mono"))

;; Recommend: ttf-jetbrains-mono / ttf-ubuntu-font-family / nerd-fonts-fira-code
;; Code: Dejavu/Monaco/Consolas/FiraCode/Noto/SouceCodePro/Inconsolata/Droid/Ubuntu
;; Rank: Helvetica/Garamond/Frutiger/Bodoni/Futura/Times/Optima/Lucida
;; Handwriting (Win): Axure Handwriting + 方正鲁迅简体/陈代明粉笔字
;; Emoji: Noto Color Emoji
;; Chinese: adobe-source-han-serif-cn-fonts/adobe-source-han-sans-cn-fonts/ttf-arphic-ukai/opendesktop-fonts/wqy-zenhei/ttf-ms-win10-zh_cn

;;; Code:

(defcustom ic/faces nil
  "(:title a :font (cons b 120) :theme c)"
  :type 'list)

(defun f/get (item &optional default)
  (let* ((faces-default (list :title nil :theme nil :font nil :font-unicode nil :font-mono '("Ubuntu Mono" "隶书") :font-emoji "Emoji"))
         (faces-merged (plist-merge faces-default ic/faces)))
    (or (plist-get faces-merged item) default)))

(setq custom--inhibit-theme-enable nil) ; make face work in emacs28
(setq menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)


;;; Font Utils

(defun f/find-font (&rest names)
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

(defun f/princ-fonts (regexp)
  "Print the suitable fonts."
  (interactive (list (read-string "Regexp to filter fonts: ")))
  (when (cl-plusp (length regexp))
    (if-let ((fs (cl-delete-if-not (lambda (f) (string-match-p regexp f))
                                   (cl-delete-duplicates (font-family-list) :test #'string-equal))))
        (message "--- %s ---\n%s" (length fs) (mapconcat #'identity fs "\n"))
      (message "No font found [%s]" regexp))))

(defun f/set-line-height (num-of-pixes)
  "Add extra height(pixes) to line height."
  (interactive (list (read-number "Extra pixes between lines: ")))
  (setq-default line-spacing (if (< num-of-pixes 1) nil num-of-pixes)))

;; global fonts

(defun f/font-default (&optional font height)
  "Set system default font."
  (interactive)
  (if (consp font) (setq height (cdr font) font (car font)))
  (let ((ft (f/find-font (unless (called-interactively-p 'any) font)))
        (fh (if (called-interactively-p 'any) (read-number "Font Height: " f/font-face-height) height))
        (args '(default nil)))
    (if ft (setq args (append args (list :family ft))))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (apply 'set-face-attribute args)
    (if (called-interactively-p 'any) (message "Set default font: %s" args) args)))

(defun f/font-unicode (&optional font height type)
  "Set the font of Unicode symbols, like 'han for Chinese."
  (interactive)
  (let* ((type (or type 'unicode))
         (ft (f/find-font (unless (called-interactively-p 'any) font)))
         (fh (if (called-interactively-p 'any) (read-number "Font Height: " f/font-face-height) height))
         (args))
    (if ft (setq args (append args (list :family ft))))
    (if (and fh (> fh 0)) (setq args (append args (list :size (/ fh 10.0)))))
    (set-fontset-font "fontset-default" type (apply 'font-spec args))
    (if (called-interactively-p 'any) (message "Set unicode font: %s" args) args)))

(defun f/font-emoji (&optional font)
  (interactive)
  (let* ((ft (f/find-font (unless (called-interactively-p 'any) font))))
    (with-over
     (set-fontset-font "fontset-default" '(#x1f000 . #x1faff) (font-spec :family ft))
     (if (called-interactively-p 'any) (message "Set emoji font: %s" ft)))))

;; face or buffer-local font, especially for Monospace scene.

(defvar f/font-face-height 0)
(defvar f/font-face-family (f/get :font-mono)) ; Mono as default
(defvar f/font-face-extras '())

(defun f/font-face (&rest face-or-faces)
  "Set face font globally"
  (interactive (list (read-face-name "Face: " (or (face-at-point t t) "default"))))
  (unless face-or-faces (setq face-or-faces '(default)))
  (let* ((ft (f/find-font (unless (called-interactively-p 'any) f/font-face-family)))
         (fh (if (called-interactively-p 'any) (read-number "Font Height: " f/font-face-height) f/font-face-height))
         (args (list :family ft)))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (if f/font-face-extras (setq args (append args f/font-face-extras)))
    (dolist (face face-or-faces)
      (setq args (append `(,face nil) args))
      (apply 'set-face-attribute args))
    (when (called-interactively-p 'any)
      (message "%s global to: %s" face-or-faces args))))

(defun f/font-face-buffer-local (&rest face-or-faces)
  "Set face font buffer local"
  (interactive (list (read-face-name "Face: " (or (face-at-point t t) "default"))))
  (unless face-or-faces (setq face-or-faces '(default)))
  (let* ((ft (f/find-font (unless (called-interactively-p 'any) f/font-face-family)))
         (fh (if (called-interactively-p 'any) (read-number "Font Height: " f/font-face-height) f/font-face-height))
         (args (list :family ft)))
    (if (and fh (> fh 0)) (setq args (append args (list :height fh))))
    (if f/font-face-extras (setq args (append args f/font-face-extras)))
    (dolist (face face-or-faces)
      (setq args (cons face args))
      (apply 'face-remap-add-relative args))
    (when (called-interactively-p 'any)
      (message "%s local to: %s" face-or-faces args))))


;;; Title

(setq frame-title-format
      (if-let ((ttl (f/get :title))) ttl
        `((:eval "%b"))))


;;; Windows

(when IS-WIN
  (setq default-frame-alist
        (append
         `((alpha  . ,(f/get :alpha 100))
           (menu-bar-lines . 0)
           (tool-bar-lines . 0)
           (scroll-bar . nil)
           (vertical-scroll-bars . nil)
           (cursor-type  . box)
           (cursor-color . "red"))
         (cl-case (f/get :frame)
           (max '((fullscreen . maximized)))
           (otherwise `((top    . ,(f/get :top 50))
                        (left   . ,(f/get :left 50))
                        (width  . ,(f/get :width 80))
                        (height . ,(f/get :height 35)))))))
  ;; English
  (f/font-default (f/get :font (cons "Consolas" 0)))
  ;; Emoji
  (f/font-emoji (f/get :font-emoji))
  ;; Unicode
  (f/font-unicode (f/get :font-unicode "Microsoft YaHei"))
  ;; Scale
  (setq face-font-rescale-alist (f/get :rescale '(("simsun" . 1) ("隶书" . 1))))
  ;; Key
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (global-set-key [C-wheel-up]   'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Linux/BSD

(when IS-LINUX
  (setq default-frame-alist
        `((height . ,(f/get :height 35))
          (width  . ,(f/get :width 110))
          (alpha  . ,(f/get :alpha 100))
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (scroll-bar . nil)
          (vertical-scroll-bars . nil))))

(when IS-NG
  (setf (alist-get 'menu-bar-lines default-frame-alist) 0)
  (unless (f/get :theme) (plist-put ic/faces :theme 'origin))
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambdai (scroll-down 1)))
  (global-set-key [mouse-5] (lambdai (scroll-up 1))))

(when IS-LINUX-G
  ;; English
  (f/font-default (f/get :font (cons "" 0)))
  ;; Emoji
  (f/font-emoji (f/get :font-emoji))
  ;; Unicode
  (f/font-unicode (f/get :font-unicode '("Source Han Sans CN")))
  ;; Key
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (global-set-key [C-mouse-4] 'text-scale-increase)
  (global-set-key [C-mouse-5] 'text-scale-decrease))


;;; MacOS

(when IS-MAC
  (setq default-frame-alist
        (append
         `((menu-bar-lines . 100)
           (tool-bar-lines . 0)
           (vertical-scroll-bars . nil))
         (cl-case (f/get :frame)
           (max '((fullscreen . maximized)))
           (otherwise `((top    . ,(f/get :top 0))
                        (left   . ,(f/get :left 0))
                        (width  . ,(f/get :width 100))
                        (height . ,(f/get :height 45)))))))
  ;; English
  (f/font-default (f/get :font (cons "" 0)))
  ;; Key
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (global-set-key [C-wheel-up] 'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Theme/Faces

(x nano-theme
   :ref "rougier/nano-theme"
   :init
   (setq nano-light-highlight "#eeeeee"))

(x gruvbox-theme
   :ref "greduan/emacs-theme-gruvbox")

(x modus-themes
   :ref "protesilaos/modus-themes"
   :init
   (setq modus-themes-mode-line '(1 borderless)))

(x zero-dark-theme
   :ref "NicolasPetton/zerodark-theme"
   :init
   (defvar zero-dark-use-paddings-in-mode-line 3))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
(defvar my-dedicate-mode-line
  '(:eval (if (window-dedicated-p) (propertize " 🮻" 'face 'font-lock-warning-face) "")))

;; custom buffer-name on mode-line
(setq-default mode-line-buffer-identification
              (list
               (propertize "%12b"       ; buffer-name
                           'face 'mode-line-buffer-id
                           'help-echo '(format "%s" (or (buffer-file-name) default-directory))
                           'mouse-face 'mode-line-highlight
                           'local-map mode-line-buffer-identification-keymap)))
(define-key mode-line-buffer-identification-keymap [mode-line mouse-1] #'im/yank-current-full-name)
(define-key mode-line-buffer-identification-keymap [mode-line mouse-3] #'im/yank-current-dir-and-buffer-name)

;; mode-line
(setq-default mode-line-format
              `("%e"
                (:eval my-dedicate-mode-line)  ; dedicated
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote) display (min-width (5.0)))
                ,(if IS-G 'mode-line-frame-identification "  ")
                (:eval project-mode-line)
                mode-line-buffer-identification ; buffer-name
                " " mode-line-position          ; positions
                ,(if IS-G `(vc-mode vc-mode))   ; vcs/git
                "  " mode-line-modes " "        ; modeline
                mode-line-misc-info
                mode-line-end-spaces))
(defun resume-mode-line () (interactive) (setq mode-line-format (default-value 'mode-line-format)))

;; theme
(aif (f/get :theme) (load-theme it t))


;;; Scroll

(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-step 1
      hscroll-margin 1)

(x pixel-scroll
   :if (>= emacs-major-version 29)
   :init
   (setq pixel-scroll-precision-large-scroll-height 40)
   (setq pixel-scroll-precision-interpolation-factor 8.0)
   (pixel-scroll-precision-mode 1))

(provide 'face)

;;; face.el ends here
