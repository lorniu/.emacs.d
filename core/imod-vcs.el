;;; imod-vcs.el --- Version Control -*- lexical-binding: t -*-

;; - vc-annotate vs magit-blame
;; - vc-region-history
;; - C-x v l
;; - vc-resolve-conflicts

;; git config --global core.preloadindex true   # default since v2.1
;; git config --global core.fscache true        # default since v2.8
;; git config --global gc.auto 256

;;; Code:

(setq vc-follow-symlinks t)
(setq vc-handled-backends '(SVN Git Hg))

(x git-auto-commit
   :commands (im/git-commit im/git-commit-and-push))

(x gitignore-templates
   :ref (git "gitignore: xuchunyang/gitignore-templates.el"))

(x magit
   "To use ghub/forge, add something like below in ~/.authinfo:

machine api.github.com login USER^forge password TOKEN
machine gitlab.com/api/v4 login USER^forge password TOKEN
"
   :if (executable-find "git")
   :ref (git
         "magit/magit"
         "magit/ghub"
         "magit/forge"
         "Github API: https://docs.github.com/en/rest"
         "Github Token: https://github.com/settings/tokens")
   :init
   ;; git
   (modify-coding-system-alist 'process "git" 'utf-8)
   ;; magit
   (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
   (setq magit-wip-mode-lighter "")
   ;; forge
   (setq forge-database-file (locc "forge-database.sqlite"))
   (setq forge-post-heading-format "%-25a\t%-15C\t(%c)\n")

   :config
   (transient-append-suffix 'magit-log "i" '("w" "Wip" magit-wip-log-current))
   (transient-append-suffix 'magit-log "-A" '("-m" "Omit merge commits" "--no-merges"))
   (transient-append-suffix 'magit-pull "-r" '("-a" "Autostash" "--autostash"))
   (transient-append-suffix 'magit-fetch "-p" '("-t" "Fetch all tags" ("-t" "--tags")))
   (magit-wip-mode +1)
   (magit-auto-revert-mode -1)

   ;; mark the ORIG_HEAD commit in log buffer
   (defun:after magit-refresh-buffer//orig-head (&rest _args)
     (when-let* ((ref (if (eq major-mode 'magit-log-mode) (magit-rev-verify "ORIG_HEAD"))))
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward (concat "^" (cl-subseq ref 0 7) ".*") nil t)
           (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
             (overlay-put ov 'face '(:overline "grey"))))))))

(defun im/git-init-config ()
  (interactive)
  (let ((name (read-string "Username: " user-full-name))
        (email (read-string "Email: " user-mail-address)))
    (with-temp-buffer
      (shell-command (concat "git config --global github.user " name))
      (shell-command (concat "git config --global user.email " email))
      (shell-command (concat "git config --global user.name " name))
      (shell-command "git config --global alias.co checkout")
      (shell-command "git config --global alias.st status")
      (shell-command "git config --global alias.m submodule")
      (shell-command "git config --global alias.b branch"))
    (message "Save config to `~/.gitconfig' done.")))

(provide 'imod-vcs)

;;; imod-vcs.el ends here
