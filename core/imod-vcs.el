;;; imod-vcs.el --- Version Control -*- lexical-binding: t -*-

;; - vc-annotate vs magit-blame
;; - vc-region-history
;; - C-x v l
;; - vc-resolve-conflicts

;; git config --global core.preloadindex true   # default since v2.1
;; git config --global core.fscache true        # default since v2.8
;; git config --global gc.auto 256

;;; Code:

(x vc
   :init
   (setq vc-handled-backends '(SVN Git Hg)))

(x git-auto-commit
   :commands (im/git-commit im/git-commit-and-push))

(x magit/i
   :ref "magit/magit"
   :if (executable-find "git")

   :init
   (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
   (setq magit-wip-mode-lighter "")
   (modify-coding-system-alist 'process "git" 'utf-8)

   :defer-config
   (transient-append-suffix 'magit-log "i" '("w" "Wip" magit-wip-log-current))
   (transient-append-suffix 'magit-log "-A" '("-m" "Omit merge commits" "--no-merges"))
   (transient-append-suffix 'magit-pull "-r" '("-a" "Autostash" "--autostash"))
   (transient-append-suffix 'magit-fetch "-p" '("-t" "Fetch all tags" ("-t" "--tags")))
   (magit-wip-mode +1)
   (magit-auto-revert-mode -1)

   ;; mark the ORIG_HEAD commit in log buffer
   (defun:after magit-refresh-buffer$orig-head (&rest _args)
     (when-let ((ref (if (eq major-mode 'magit-log-mode) (magit-rev-verify "ORIG_HEAD"))))
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward (concat "^" (cl-subseq ref 0 7) ".*") nil t)
           (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
             (overlay-put ov 'face '(:overline "grey"))))))))

(provide 'imod-vcs)

;;; imod-vcs.el ends here
