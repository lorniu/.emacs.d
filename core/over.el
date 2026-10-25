;;; -*- lexical-binding: t -*-

;;; Code:

(xzz gcmh
  :diminish gcmh-mode
  :init (gcmh-mode 1)
  :config
  ;; Ask AI to provide the recommended values
  ;; (setq gcmh-low-cons-threshold  (* 16 1024 1024)
  ;;       gcmh-high-cons-threshold (* 512 1024 1024)
  ;;       gcmh-idle-delay 5)
  )

(provide 'over)
