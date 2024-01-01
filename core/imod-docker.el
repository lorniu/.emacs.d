;;; -*- lexical-binding: t -*-

;;; Code:

(xzz kubernetes
  :ref "kubernetes-el/kubernetes-el"
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))
