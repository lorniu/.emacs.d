;;; imod-edit+.el --- Edit Enhanced -*- lexical-binding: t -*-

;; - Remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; - remove `iedit', as `auto-highlight-symbol' is amazing. Hack it and enjoy it! 2018-07-03

;;; Code:

(x vundo
   :ref "casouri/vundo"
   :bind (("C-x u" . vundo)
          :map vundo-mode-map
          ("l" . vundo-forward)
          ("h" . vundo-backward)
          ("j" . vundo-next)
          ("k" . vundo-previous)))

(x ahs
   :ref "elp-revive/auto-highlight-symbol"
   :bind ( :map ahs-mode-map
           ("M-p" . ahs-backward)
           ("M-n" . ahs-forward)
           ("M-r" . ahs-change-range))
   :init
   (setq ahs-idle-interval 0.3
         ahs-case-fold-search nil
         ahs-exclude '(( ruby-mode . "\\_<\\(end\\)\\_>" )
                       ( prog-mode . "\\_<\\(t\\|nil\\)\\_>" )))
   (require 'ahs)
   (global-ahs-mode 1))

(x expand-region
   :ref "magnars/expand-region.el"
   :init
   (setq expand-region-contract-fast-key ".")
   (setq expand-region-show-usage-message nil)
   :config
   ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/20#issuecomment-1352675350
   (defun treesit-mark-bigger-node ()
     (when (treesit-available-p)
       (let* ((root (treesit-buffer-root-node))
              (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
              (node-start (treesit-node-start node))
              (node-end (treesit-node-end node)))
         ;; Node fits the region exactly. Try its parent node instead.
         (when (and (= (region-beginning) node-start) (= (region-end) node-end))
           (when-let ((node (treesit-node-parent node)))
             (setq node-start (treesit-node-start node)
                   node-end (treesit-node-end node))))
         (set-mark node-end)
         (goto-char node-start))))
   (add-to-list 'er-try-expand-list 'treesit-mark-bigger-node)
   :commands (er-expand-region))

(x vlf
   "Open huge file, Part by Part."
   :ref "m00natic/vlfi"
   :init
   (require 'vlf-setup)

   (add-to-list 'vlf-forbidden-modes-list 'doc-view-mode-maybe)
   (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))

(catch 'sudo
  (cl-loop with sudo-face
           = (lambda ()
               (when (string-equal (file-remote-p (or buffer-file-name default-directory) 'user) "root")
                 (setq header-line-format (propertize " Edit as ROOT! " 'face '(:foreground "white" :background "red3")))))
           for hook in '(find-file-hook dired-mode-hook)
           do (add-hook hook sudo-face)
           finally
           (defun sudo ()
             "Edit file as Super User."
             (interactive)
             (if IS-WIN
                 (if-let (f (buffer-file-name))
                     (start-process-shell-command (format-time-string "sudo-%s") nil (concat "wsudo notepad " f))
                   (user-error "No buffer-file found"))
               (let ((pt (point)) (old-buf (current-buffer))
                     (buf-name (expand-file-name (or buffer-file-name default-directory))))
                 (setq buf-name (or (file-remote-p buf-name 'localname) (concat "/sudo::" buf-name)))
                 (cl-letf (((symbol-function 'server-buffer-done) (lambda (__ &optional _) nil)))
                   (find-file buf-name))
                 (kill-buffer-if-not-modified old-buf)
                 (goto-char pt))))))

(provide 'imod-edit+)

;;; imod-edit+.el ends here
