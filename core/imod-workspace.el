;;; -*- lexical-binding: t -*-

;;; Code:

(x treemacs
   :ref "Alexander-Miller/treemacs"
   :bind ( :map treemacs-mode-map
           ([f2]             . treemacs-rename-file)
           ("D"              . treemacs-delete-file)
           ("m"              . im/assist-treemacs-mode)
           ("C-x d"          . treemacs-create-dir)
           ("C-x f"          . treemacs-create-file)
           ("C-x C-d"        . treemacs-create-dir)
           ("C-x C-f"        . treemacs-create-file)
           ("C-c C-u"        . treemacs-goto-parent-node)
           ([down-mouse-1]   . im:treemacs-leftclick-smart-action))
   :config
   (setopt treemacs-follow-after-init t)
   (treemacs-fringe-indicator-mode 'only-when-focused))

(defun im:treemacs-dired-current ()
  "Open current path in `dired'."
  (interactive)
  (if-let* ((path (treemacs--prop-at-point :path)))
      (dired (if (file-directory-p path)
                 path
               (file-name-directory path)))
    (user-error "No file at point")))

(defun im:treemacs-leftclick-smart-action (event)
  "Open or expand only when click on the path name."
  (interactive "e")
  (when (eq 'down-mouse-1 (elt event 0))
    (let ((point (cadr (elt event 1)))
          (path (treemacs--prop-at-point :path)))
      (if (and path (ignore-errors (string-match-p "[[:alnum:]._-]" (buffer-substring point (+ 1 point)))))
          (progn (setf (elt event 0) 'double-mouse-1)
                 (treemacs-doubleclick-action event))
        (treemacs-leftclick-action event)))))

(defun im/treemacs-follow ()
  (interactive)
  (require 'treemacs)
  (treemacs--follow)
  (unless (equal (treemacs-current-visibility) 'visible)
    (treemacs))
  (select-window (treemacs-get-local-window)))

(defun:after treemacs--init//disable-follow-after-init (&rest _)
  (treemacs-follow-mode -1))

(defun:around treemacs--create-imenu-index-function//ignore-errors (f &rest args)
  "Supress error: which-func-ff-hook error: (wrong-type-argument arrayp nil)."
  (ignore-errors (apply f args)))

(defun:override treemacs--setup-mode-line//clickable ()
  (setopt mode-line-format
          `((:propertize
             "Treemacs: "
             face font-lock-property-name-face
             mouse-face warning
             help-echo "Click to close"
             local-map (keymap (mode-line keymap (mouse-1 . treemacs-kill-buffer))))
            (:eval (treemacs-workspace->name (treemacs-current-workspace))))))

(transient-define-prefix im/assist-treemacs-mode ()
  [:hide
   (lambda () t)
   ("i" "0" treemacs-hide-gitignored-files-mode)]
  [["Basic"
    ("o"        "Externally"       treemacs-visit-node-in-external-application)
    ("R"        "Rename File"      treemacs-rename-file)
    ("D"        "Delete File"      treemacs-delete-file)
    ("!"        "Run Command"      treemacs-run-shell-command-for-current-node)
    ("F"        "Collapse All"     im/smart-folding-all)]
   ["Toggle"
    ("f  "      "Follow Mode"      treemacs-follow-mode)
    ("a  "      "File Watch Mode"  treemacs-filewatch-mode)
    ("h  "      (lambda () (!tdesc "i h" "gitignore/dotfiles")) treemacs-toggle-show-dotfiles :format " %d")
    ""
    ("M-<UP>"   "Project Up"       treemacs-move-project-up)]
   ["Project"
    ("p a"      "Add project"      treemacs-add-project-to-workspace)
    ("p r"      "Rename project"   treemacs-rename-project)
    ("p d"      "Remove Project"   treemacs-remove-project-from-workspace)
    ""
    ("M-<DOWN>" "Project Down"     treemacs-move-project-down)]
   ["Workspace"
    ("w a"      "Create workspace" treemacs-create-workspace)
    ("w r"      "Rename workspace" treemacs-rename-workspace)
    ("w s"      "Switch workspace" treemacs-switch-workspace )
    ("w e"      "Edit workspace"   treemacs-edit-workspaces)
    ("w d"      "Delete workspace" treemacs-remove-workspace)]])
