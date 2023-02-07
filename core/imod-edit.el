;;; -*- lexical-binding: t -*-

;; - Remove `multiple-cursors', it is boring. use `iedit' instead. 2018-07-01
;; - remove `iedit', as `auto-highlight-symbol' is amazing. Hack it and enjoy it! 2018-07-03

;;; Code:

(xzz vundo
  :ref "casouri/vundo"
  :bind (("C-x u" . vundo)
         :map vundo-mode-map
         ("l" . vundo-forward)
         ("h" . vundo-backward)
         ("j" . vundo-next)
         ("k" . vundo-previous)))

(xzz ahs/e
  :ref "elp-revive/auto-highlight-symbol"
  :bind ( :map ahs-mode-map
          ("M-p" . ahs-backward)
          ("M-n" . ahs-forward)
          ("M-r" . ahs-change-range))
  :config
  (setopt ahs-idle-interval 0.3
          ahs-case-fold-search nil
          ahs-exclude '(( ruby-mode . "\\_<\\(end\\)\\_>" )
                        ( prog-mode . "\\_<\\(t\\|nil\\)\\_>" )))
  (global-ahs-mode 1))

(xzz expand-region
  :ref "magnars/expand-region.el"
  :commands (er-expand-region)
  :config
  (setopt expand-region-contract-fast-key "."
          expand-region-show-usage-message nil)
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/20#issuecomment-1352675350
  (defun treesit-mark-bigger-node ()
    (when (treesit-available-p)
      (let* ((root (treesit-buffer-root-node))
             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
          (when-let* ((node (treesit-node-parent node)))
            (setq node-start (treesit-node-start node)
                  node-end (treesit-node-end node))))
        (set-mark node-end)
        (goto-char node-start))))
  (add-to-list 'er-try-expand-list 'treesit-mark-bigger-node))
