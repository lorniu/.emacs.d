;;; imod-container.el --- Docker/K8S -*- lexical-binding: t -*-

;; docker dockerfile-mode
;; Kubernetes

;;; Code:

(x kubernetes
   :ref "kubernetes-el/kubernetes-el"
   :commands (kubernetes-overview)
   :init
   (setq kubernetes-poll-frequency 3600
         kubernetes-redraw-frequency 3600))

(provide 'imod-container)

;;; imod-container.el ends here
