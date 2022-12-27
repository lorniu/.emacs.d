;;; face.el --- Faces and locale -*- lexical-binding: t -*-

;; Recommend: ttf-jetbrains-mono / ttf-ubuntu-font-family / nerd-fonts-fira-code
;; Code: Dejavu/Monaco/Consolas/FiraCode/Noto/SouceCodePro/Inconsolata/Droid/Ubuntu
;; Rank: Helvetica/Garamond/Frutiger/Bodoni/Futura/Times/Optima/Lucida
;; Handwriting (Win): Axure Handwriting + 方正鲁迅简体/陈代明粉笔字
;; Emoji: Noto Color Emoji
;; Chinese: adobe-source-han-serif-cn-fonts/adobe-source-han-sans-cn-fonts/ttf-arphic-ukai/opendesktop-fonts/wqy-zenhei/ttf-ms-win10-zh_cn

;;; Code:

(setq custom--inhibit-theme-enable nil) ; make face work in emacs28
(setq menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)


;;; Windows

(when IS-WIN
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
