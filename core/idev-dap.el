;;; -*- lexical-binding: t -*-

;; Debug Adapter Protocol (DAP)
;;
;;   https://microsoft.github.io/debug-adapter-protocol/
;;

;;; Code:

(x dape
   :ref "svaante/dape"
   :config
   (setopt dape-debug t
           dape-buffer-window-arrangement 'right))
