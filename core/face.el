;;; face.el --- Faces and locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Recommend: ttf-jetbrains-mono / ttf-ubuntu-font-family / nerd-fonts-fira-code
;; Code: Dejavu/Monaco/Consolas/FiraCode/Noto/SouceCodePro/Inconsolata/Droid/Ubuntu
;; Rank: Helvetica/Garamond/Frutiger/Bodoni/Futura/Times/Optima/Lucida
;; Handwriting (Win): Axure Handwriting + 方正鲁迅简体/陈代明粉笔字
;; Chinese: adobe-source-han-serif-cn-fonts/adobe-source-han-sans-cn-fonts/ttf-arphic-ukai/opendesktop-fonts/wqy-zenhei/ttf-ms-win10-zh_cn

(setq custom--inhibit-theme-enable nil) ; make face work in emacs28
(setq menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)


;;; Windows

(when IS-WIN
  ;; English
  (ff/font-default (ff/get :font (cons "Consolas" 0)))
  ;; Chinese
  (ff/font-unicode (ff/get :font-unicode "Microsoft YaHei"))
  ;; Scale
  (setq face-font-rescale-alist (ff/get :rescale '(("simsun" . 1) ("隶书" . 1))))
  ;; Key
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (global-set-key [C-wheel-up]   'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Linux/BSD

(when IS-NG
  (setf (alist-get 'menu-bar-lines default-frame-alist) 0)
  (unless (ff/get :theme) (plist-put ic/faces :theme 'origin))
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambdai (scroll-down 1)))
  (global-set-key [mouse-5] (lambdai (scroll-up 1))))

(when IS-LINUX-G
  ;; English
  (ff/font-default (ff/get :font (cons "" 0)))
  ;; Chinese
  (ff/font-unicode (ff/get :font-unicode '("Source Han Sans CN")))
  ;; Key
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (global-set-key [C-mouse-4] 'text-scale-increase)
  (global-set-key [C-mouse-5] 'text-scale-decrease))


;;; MacOS

(when IS-MAC
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (global-set-key [C-wheel-up] 'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))


;;; Faces

(defvar zero-dark-use-paddings-in-mode-line 3)
(defreference theme-zero-dark "NicolasPetton/zerodark-theme")

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                ,(if IS-G 'mode-line-frame-identification "  ")
                (:eval project-mode-line)
                mode-line-buffer-identification
                (" " mode-line-position)
                ,(if IS-G `(vc-mode vc-mode))
                ("  " mode-line-modes " ")
                mode-line-misc-info
                mode-line-end-spaces))

(aif (ff/get :theme) (load-theme it t))


;;; Encoding

(set-locale-environment   "utf-8")
(prefer-coding-system     'gb2312)
(prefer-coding-system     'cp936)
(prefer-coding-system     'utf-16)
(prefer-coding-system     'utf-8-unix)

(defun im/local-encoding (&optional encoding)
  "Reset local system encoding, default is CP936."
  (interactive)
  (let ((encoding (or encoding 'cp936-dos)))
    (when (called-interactively-p 'any)
      (setq encoding (read-coding-system "Choose charset: " 'utf-8)))
    (set-buffer-file-coding-system encoding)
    (ignore-errors
      (set-process-coding-system (get-buffer-process (current-buffer)) encoding encoding))
    (message "Changed local coding to %s." encoding)))

(when IS-WIN
  ;; generic encoding
  (set-language-environment "chinese-gbk")
  (prefer-coding-system 'utf-8)

  ;; process global encoding
  (setq process-coding-system-alist '(("what?" utf-8 . utf-8)))

  ;; specified encoding
  (setq file-name-coding-system 'cp936-dos)
  (set-terminal-coding-system 'cp936-dos))


;;; Syntax/Indent etc

(setq sentence-end-double-space nil)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(mapc (lambda (c) (modify-syntax-entry c "." (standard-syntax-table))) '( ?， ?。 ?！ ?； ?？ ?： ?/ ))


(provide 'face)

;;; face.el ends here
