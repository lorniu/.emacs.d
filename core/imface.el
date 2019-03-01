;;; imface.el --- Faces And Encoding, ETC.
;;; Commentary:

;;; Code:

(defvar im/mono-buffer-height 108)
(defvar im/probe-mono-fonts '("Ubuntu Mono" "隶书"))  ;; pacman -S ttf-ubuntu-font-family

(defvar im/win-font "Consolas/106")
(defvar im/win-font-cn "微软雅黑 light")
(defvar im/win-frame-alist '(30 640 85 35 100) "(Top Left Width Height Alpha)")

(defvar im/nix-font-cn "Source Han Sans CN")
(defvar im/nix-frame-alist '(110 100) "(Height Alpha")

(menu-bar-mode 0)
(tool-bar-mode 0)


;;; Helpers

(defun im/find-ft (&rest names)
  "Find the proper font in NAMES."
  (catch 'ret
    (if (listp (car names))
        (setq names (car names)))
    (let ((fonts (font-family-list)) res-font)
      (dolist (name names)
        (setq rs-font (seq-find (lambda (font) (string-match-p (format ".*%s.*" name) font)) fonts))
        (if rs-font (throw 'ret rs-font))))))


;;; Windows

(env-windows
 ;; frame
 (setq default-frame-alist
       `((title  . "νερό")
         (top    . ,(nth 0 im/win-frame-alist))
         (left   . ,(nth 1 im/win-frame-alist))
         (width  . ,(nth 2 im/win-frame-alist))
         (height . ,(nth 3 im/win-frame-alist))
         (alpha  . ,(nth 4 im/win-frame-alist))
         (tool-bar-lines . 0)
         (scroll-bar . nil)
         (vertical-scroll-bars . nil)
         (cursor-type  . box)
         (cursor-color . "red")))
 ;; font
 (cl-multiple-value-bind (font height) (split-string im/win-font "/")
   (set-face-attribute 'default nil :font font :height (if height (string-to-number height) 100)))
 (set-fontset-font "fontset-default" 'unicode im/win-font-cn)
 (setq face-font-rescale-alist '(("simsun" . 1) ("隶书" . 1)))
 ;; key
 (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
 (global-set-key [C-wheel-up]   'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Linux

(env-ng
 (load-theme 'origin t)
 (xterm-mouse-mode)
 (global-set-key [mouse-4] (lambdai (scroll-down 1)))
 (global-set-key [mouse-5] (lambdai (scroll-up 1))))

(env-linux-g
 (scroll-bar-mode 0)
 ;; font
 (setq default-frame-alist
       `((height . ,(nth 0 im/nix-frame-alist))
         (alpha  . ,(nth 1 im/nix-frame-alist))))
 (set-fontset-font "fontset-default" 'unicode im/nix-font-cn)
 ;; key
 (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
 (global-set-key [C-mouse-4] 'text-scale-increase)
 (global-set-key [C-mouse-5] 'text-scale-decrease))


;;; Mac OS

(env-macos
 (menu-bar-mode 1)
 (scroll-bar-mode 0)

 (global-set-key [C-wheel-up] 'text-scale-increase)
 (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Fix Width

(defun im/make-face-mono (&rest face-or-faces)
  "Modify FACE-OR-FACES to make it use mono font family."
  (dolist (face face-or-faces)
    (set-face-attribute face nil :family (im/find-ft im/probe-mono-fonts))))

(defun im/set-buffer-mono-font (&optional font-height)
  "Set current buffer to use mono font."
  (interactive "sFont Height: ")
  (let ((face (im/find-ft im/probe-mono-fonts)) face-plist)
    (if (null face)
        (message "No proper mono fonts found")
      (setq face-plist `(:family ,face))
      (setq font-height (or font-height im/mono-buffer-height))
      (if (stringp font-height) (setq font-height (string-to-number font-height)))
      (if (and font-height (> font-height 0))
          (nconc face-plist `(:height ,font-height)))
      (setq buffer-face-mode-face face-plist)
      (buffer-face-mode))))

(defun im/set-char-widths??? (alist)
  "Change return of 'string-width', maybe."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width (car pair))
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

;; (im/set-char-widths??? `((1 . ((#x00 . #xFFFF)))))


;;; Encoding

(set-locale-environment   "utf-8")
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)

(env-windows
 ;; generic encoding
 (set-language-environment "chinese-gbk")
 (prefer-coding-system 'utf-8)

 ;; specified encoding
 (setq file-name-coding-system 'gbk)
 (set-terminal-coding-system 'gbk)
 (modify-coding-system-alist 'process "*" 'gbk)

 (defun im/cp936-encoding ()
   (set-buffer-file-coding-system 'gbk)
   (set-buffer-process-coding-system 'gbk 'gbk))

 (add-hook 'shell-mode-hook 'im/cp936-encoding))


;;; Miscellaneous

(setq-default
 mode-line-format
 '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification
   mode-line-buffer-identification
   "   " mode-line-position
   mode-line-modes mode-line-misc-info mode-line-end-spaces (vc-mode vc-mode)))

(setq sentence-end-double-space nil)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table)))
      '( ?， ?。 ?！ ?； ?？ ?： ?/ ))

(provide 'imface)

;;; imface.el ends here
