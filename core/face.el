;;; face.el --- Faces -*- lexical-binding: t -*-

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
  :type 'list
  :group 'imfine)

(defun f/get (item &optional default)
  (let* ((faces-default (list :title nil :theme nil :font nil :font-unicode nil :font-mono '("Ubuntu Mono" "隶书") :font-emoji "Emoji"))
         (faces-merged (im:plist-merge faces-default ic/faces)))
    (or (plist-get faces-merged item) default)))

(setq custom--inhibit-theme-enable nil) ; make face work in emacs28
(setq menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)

(require 'face-font)
(require 'face-modeline)


;;; Title

(setq frame-title-format
      (or (f/get :title)
          `((:eval (cond
                    ((equal (elt (buffer-name) 0) ? ) "Nix")
                    (t "%b"))))))


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
  ;; Font Scale
  (setq face-font-rescale-alist (f/get :rescale '(("simsun" . 1) ("隶书" . 1)))))


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
  (f/font-unicode (f/get :font-unicode '("Source Han Sans CN"))))


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
  (f/font-default (f/get :font (cons "" 0))))


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

;; theme
(if-let (it (f/get :theme)) (load-theme it t))


;;; Scroll

(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-step 1
      hscroll-margin 1)

(when IS-G
  (global-set-key [C-wheel-up]
                  (lambda ()
                    (interactive)
                    (require 'face-remap)
                    (let ((text-scale-mode-step 1.02))
                      (text-scale-increase 1))))
  (global-set-key [C-wheel-down]
                  (lambda ()
                    (interactive)
                    (require 'face-remap)
                    (let ((text-scale-mode-step 1.02))
                      (text-scale-decrease 1))))
  (global-set-key [C-mouse-3]      (lambda () (interactive) (text-scale-mode -1)))
  (global-set-key [C-down-mouse-3] (lambda () (interactive) (text-scale-mode -1)))
  ;; This affect both scroll and scale
  (setq mouse-wheel-progressive-speed nil))

(x pixel-scroll
   :if (>= emacs-major-version 29)
   :init
   (setq pixel-scroll-precision-large-scroll-height 40)
   (setq pixel-scroll-precision-interpolation-factor 8.0)
   (pixel-scroll-precision-mode 1))

(provide 'face)

;;; face.el ends here
