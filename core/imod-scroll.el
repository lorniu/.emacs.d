;;; -*- lexical-binding: t -*-

;; eye: https://github.com/jdtsmith/ultra-scroll

;;; Code:

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
  (global-set-key [C-mouse-3]      (lambdi () (text-scale-mode -1)))
  (global-set-key [C-down-mouse-3] (lambdi () (text-scale-mode -1)))
  ;; This affect both scroll and scale
  (setopt mouse-wheel-progressive-speed nil))

(xzz pixel-scroll
  :init (pixel-scroll-precision-mode 1)
  :config
  (setopt pixel-scroll-precision-large-scroll-height 40
          pixel-scroll-precision-interpolation-factor 8.0))
