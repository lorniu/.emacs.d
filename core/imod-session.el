;;; imod-session.el --- Session Management -*- lexical-binding: t -*-

;; https://www.emacswiki.org/emacs/SessionManagement

;;; Code:

(x desktop
   :init
   (defvar desktop-save-default-dir (locc))
   (defvar desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\)")
   (defvar desktop-modes-not-to-save '(tags-table-mode))
   (defvar desktop-load-locked-desktop t)
   :config
   ;; Window configurations can not be saved and restored yet?
   ;; So can't add `im:fullscreen-alist' here
   (add-to-list 'desktop-globals-to-save 'im:views)
   (setq desktop-path `(,(locc) ".")))

(x savehist
   :init
   (setq savehist-file (locc "history"))
   (setq savehist-ignored-variables '(im:pages
                                      im:rg-search-history
                                      consult--theme-history
                                      consult-org--history
                                      query-replace-history))
   (savehist-mode +1))

(x recentf
   :init
   (setq recentf-max-saved-items 200)
   (setq recentf-exclude '(package-user-dir
                           (expand-file-name package-user-dir)
                           "-autoloads.el$"
                           ".cache"
                           ".cask"
                           "bookmarks"
                           "cache"
                           "ido.*"
                           "persp-confs"
                           "recentf"
                           "undo-tree-hist"
                           "url"
                           "COMMIT_EDITMSG\\'"))

   (defun im/recentf-clear (&optional regexp)
     (interactive (list (read-string "Regexp for flushing from recentf: ")))
     (if (zerop (length regexp))
         (setq recentf-list nil)
       (cl-delete-if (lambda (f) (string-match-p regexp f)) recentf-list))
     (recentf-save-list))

   (recentf-mode 1))

(x winner
   :init (winner-mode 1)
   :config
   (define-key winner-mode-map [C-left] 'winner-undo)
   (define-key winner-mode-map [C-right] 'winner-redo))

(x multisession
   :init
   (setq multisession-directory (locc "multisession")))


;;; Interface

(defun im:desktop-read-dir (&optional write)
  (let* ((proot (project-root (project-current)))
         (dir (if (and proot (file-exists-p (expand-file-name desktop-base-file-name proot))) proot desktop-save-default-dir)))
    (if write
        (let ((dst (read-directory-name "Save desktop to directory: " dir nil nil)))
          (unless (file-exists-p dst) (make-directory dst t))
          dst)
      (read-directory-name "Read desktop from directory: " dir desktop-save-default-dir t))))

(defun im:desktop-save-to (dir)
  (desktop-save dir)
  (message (concat "Saved to " (propertize dir 'face 'font-lock-string-face) ", done!")))

(transient-define-prefix im/transient-desktop ()
  [:hide
   (lambda () t)
   ("f  "  "1"  (lambda () (interactive) (desktop-read (im:desktop-read-dir))))
   ("C-f"  "2"  (lambda () (interactive) (desktop-read (im:desktop-read-dir))))
   ("C-w"  "3"  (lambda () (interactive) (im:desktop-save-to (im:desktop-read-dir t))))
   ("o  "  "4"  (lambda () (interactive) (desktop-change-dir desktop-save-default-dir)))
   ("C-o"  "5"  (lambda () (interactive) (desktop-change-dir desktop-save-default-dir)))
   ("C-d"  "6"  (lambda () (interactive) (im:desktop-save-to desktop-save-default-dir)))
   ("k  "  "7"  desktop-clear)]
  [[("w" (lambda () (!tdesc "f w" "Find/Save..."))      (lambda () (interactive) (im:desktop-save-to (im:desktop-read-dir t))) :format "%d")]
   [("c" "Change..."                                    (lambda () (interactive) (desktop-change-dir (im:desktop-read-dir))))]
   [("d" (lambda () (!tdesc "o d" "Load/Save Default")) (lambda () (interactive) (im:desktop-save-to desktop-save-default-dir)) :format "%d")]
   [("v" (lambda () (!tdesc "k v" "Clear/Revert"))      desktop-revert :format "%d")]])

(provide 'imod-session)

;;; imod-session.el ends here
