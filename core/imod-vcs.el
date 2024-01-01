;;; -*- lexical-binding: t -*-

;; - vc-annotate vs magit-blame
;; - vc-region-history
;; - C-x v l
;; - vc-resolve-conflicts

;; git config --global core.preloadindex true   # default since v2.1
;; git config --global core.fscache true        # default since v2.8
;; git config --global gc.auto 256

;;; Code:

(xzz vc
  :config
  (setopt vc-follow-symlinks t
          vc-handled-backends '(SVN Git Hg)))

(xzz git-auto-commit
  :commands (im/git-commit im/git-commit-and-push))

(xzz magit
  "To use ghub/forge, add something like below in ~/.authinfo:

machine api.github.com login USER^forge password TOKEN
machine gitlab.com/api/v4 login USER^forge password TOKEN"
  :if (executable-find "git")
  :ref (git
        "magit/magit"
        "magit/ghub"
        "magit/forge"
        "Github API: https://docs.github.com/en/rest"
        "Github Token: https://github.com/settings/tokens")
  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  ;; encoding
  (modify-coding-system-alist 'process "git" 'utf-8)
  ;; magit
  (setopt magit-wip-mode-lighter "")
  (magit-wip-mode +1)
  (transient-append-suffix 'magit-log   "i"  '("w" "Wip" magit-wip-log-current))
  (transient-append-suffix 'magit-log   "-A" '("-m" "Omit merge commits" "--no-merges"))
  (transient-append-suffix 'magit-pull  "-r" '("-a" "Autostash" "--autostash"))
  (transient-append-suffix 'magit-fetch "-p" '("-t" "Fetch all tags" ("-t" "--tags")))
  ;; forge
  (setopt forge-database-file (locc "forge-database.sqlite")
          forge-post-heading-format "%-25a\t%-15C\t(%c)\n"))

(defun:after magit-refresh-buffer//orig-head (&rest _args)
  ;; mark the ORIG_HEAD commit in log buffer
  (when-let* ((ref (if (eq major-mode 'magit-log-mode) (magit-rev-verify "ORIG_HEAD"))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (cl-subseq ref 0 7) ".*") nil t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'face '(:overline "grey")))))))

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
