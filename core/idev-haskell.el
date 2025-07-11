;;; -*- lexical-binding: t -*-

;; Plans:
;;
;; 1) Intero, IDE-like, based on Stack:
;;
;;    pacman -S stack
;;
;;    Intero will take a long time to initialize for the first time, download a lot!
;;
;; 2) Dante. Lightweight version of stack.
;;
;;    Change to Dante 20180513, saved 3G space
;;
;; 3) eglot + haskell-language-server
;;
;;    ghcup install hls (haskell-language-server-wrapper)
;;
;; Summary: hls first, use dante when no hls installed.
;;

;;; Code:

(xzz haskell
  :ref ("LSP Server: https://github.com/haskell/haskell-language-server/releases")
  :diminish interactive-haskell-mode
  :config
  (setopt haskell-tags-on-save nil
          haskell-process-log t
          haskell-process-show-debug-tips nil
          haskell-process-type 'auto
          haskell-process-suggest-remove-import-lines nil)

  (defun:hook haskell-mode-hook ()
    (interactive-haskell-mode))

  (define-key haskell-interactive-mode-map (kbd "C-c M-o") 'haskell-interactive-mode-clear)
  (define-key haskell-interactive-mode-map (kbd "C-a") 'haskell-interactive-mode-beginning))
