;;; imod-edit+.el --- Edit Enhanced -*- lexical-binding: t -*-

;; - Remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; - remove `iedit', as `auto-highlight-symbol' is amazing. Hack it and enjoy it! 2018-07-03

;;; Code:

(catch 'sudo
  (cl-loop with sudo-face
           = (lambda ()
               (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
                 (setq header-line-format (propertize " Edit as ROOT! " 'face '(:foreground "white" :background "red3")))))
           for hook in '(find-file-hook dired-mode-hook)
           do (add-hook hook sudo-face)
           finally
           (defun su ()
             "Edit file as Super User."
             (interactive)
             (let ((pt (point)) (old-buf (current-buffer))
                   (buf-name (expand-file-name (or buffer-file-name default-directory))))
               (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
               (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
                 (find-file buf-name))
               (kill-buffer-if-not-modified old-buf)
               (goto-char pt)))))

(x vundo
   :ref "casouri/vundo"
   :bind (("C-x u" . vundo)
          (vundo-mode-map
           ("l" . vundo-forward)
           ("h" . vundo-backward)
           ("j" . vundo-next)
           ("k" . vundo-previous))))

(x ahs
   :bind ((ahs-mode-map
           ("M-p" . ahs-backward)
           ("M-n" . ahs-forward)
           ("M-r" . ahs-change-range)))
   :init
   (setq ahs-idle-interval 0.3
         ahs-case-fold-search nil
         ahs-exclude '(( ruby-mode . "\\_<\\(end\\)\\_>" )
                       ( prog-mode . "\\_<\\(t\\|nil\\)\\_>" )))
   (require 'ahs)
   (global-ahs-mode 1))

(x multiple-cursors/x
   :ref "magnars/multiple-cursors.el")

(x vlf
   "Open huge file, Part by Part."
   :ref "m00natic/vlfi"
   :init
   (require 'vlf-setup)

   (add-to-list 'vlf-forbidden-modes-list 'doc-view-mode-maybe)
   (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))

(x expand-region/i
   :ref "magnars/expand-region.el"
   :init
   (setq expand-region-contract-fast-key ".")
   (setq expand-region-show-usage-message nil)
   :commands (er/expand-region))

(x ztree
   "Use `ztree-diff' to diff directories."
   :ref "fourier/ztree"
   :defer-config
   (setq ztree-draw-unicode-lines t)
   (setq ztree-diff-additional-options nil) ; '("-w" "-i")
   (add-to-list 'ztree-diff-filter-list "^~"))

(provide 'imod-edit+)

;;; imod-edit+.el ends here
