;;; imdraft.el --- Something may be changed later -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Multiple Modes

;; MMM-Mode
;; Poly-Mode
;; edit-indirect

(x edit-indirect/+
   :ref "Fanael/edit-indirect")


;;; Enhance completion

(x embark/+
   "Now not in mepa. Update manually. 20200801. v0.6."
   :ref ("oantolin/embark"
         "embark source: https://raw.githubusercontent.com/oantolin/embark/master/embark.el")
   :init
   ;; (require 'embark)
   )

;; (with-eval-after-load 'selectrum
;;   (add-to-list 'selectrum-minibuffer-bindings '("M-o" . (lambda () (interactive) (embark-act) (embark-keymap-help))))
;;   (add-to-list 'selectrum-minibuffer-bindings '("C-c C-o" . embark-occur)))


(provide 'imdraft)

;;; imdraft.el ends here
