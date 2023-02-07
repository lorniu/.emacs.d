;;; imod-display.el --- Display and window -*- lexical-binding: t -*-

;;; Code:

(setq display-buffer-alist
      `(("\\*Compile-Log\\*"            ; regexp to filter buffer-name
         (display-buffer-reuse-window)  ; functions to deal with display
         (window-height . 0.3)          ; parameters to pass to functions above
         (window-width . 0.3))

        ("\\*[cC]ompilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom))

        ("\\*Messages\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (reusable-frames . t))

        ("\\*sly-macroexpansion\\*\\|\\*Pp Macroexpand Output\\*"
         (display-buffer-reuse-window %display-buffer-in-direction-or-below-selected)
         (direction . right))

        ("\\*Youdao Dictionary\\*\\|\\*Help\\*\\|\\*Shell Command Output\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (reusable-frames . t))

        ("\\*\\(e?shell[-+]?\\|PowerShell\\|Python\\)\\*\\|[-+]\\(shell\\)[-+]"
         (display-buffer-same-window)
         (reusable-frames . t))

        ("\\*Async Shell Command\\*"
         (%display-buffer-at-bottom-follows-with-quit)
         (window-height . 0.3))

        ("\\*org-roam\\*"
         (display-buffer-in-direction)
         (direction . right)
         (window-width . 0.33)
         (window-height . fit-window-to-buffer))))

(defmacro im:make-fn--display-buffer-in-direction-or- (other)
  (let ((fname (intern (format "%%display-buffer-in-direction-or-%s" other)))
        (dname (intern (format "display-buffer-%s" other))))
    `(defun ,fname (buffer alist)
       (if (and (> (frame-width) 120)
                (null (window-in-direction 'right))
                (null (window-in-direction 'left)))
           (display-buffer-in-direction buffer alist)
         (,dname buffer alist)))))

(im:make-fn--display-buffer-in-direction-or- below-selected)
(im:make-fn--display-buffer-in-direction-or- at-bottom)

(defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
  (display-buffer-at-bottom buffer alist)
  (select-window (get-buffer-window buffer))
  (with-current-buffer buffer (view-mode 1)))


;;; Toggle show

(defmacro im/hide-or-show-buffer (buffer-predicate buffer-show-form &rest buffer-init-form)
  "Helper macro: hide or show buffer window."
  (declare (indent 1))
  `(let* ((it (cl-find-if ,(cond ((and (consp buffer-predicate) (equal (car buffer-predicate) 'lambda)) buffer-predicate)
                                 ((symbolp buffer-predicate) buffer-predicate)
                                 (t `(lambda (b) (string-equal (buffer-name b) ,buffer-predicate))))
                          (buffer-list))))
     (cond ((and ,(not (null buffer-init-form)) (null it))
            ,@buffer-init-form)
           ((and it (get-buffer-window it))
            (save-excursion (ignore-errors (delete-window (get-buffer-window it)))))
           (t ,buffer-show-form))))

(defun im/toggle-messages-buffer()
  "Toggle show the *Messages* buffer."
  (interactive)
  (im/hide-or-show-buffer "*Messages*" (call-interactively 'view-echo-area-messages)))

(defun im/toggle-scratch-buffer (&optional ielmp)
  "Toggle show the *Scratch* buffer."
  (interactive "P")
  (im/hide-or-show-buffer "*scratch*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (scratch-buffer))))

(defun im/toggle-ielm-buffer (&optional ielmp)
  "Toggle show the *ielm* buffer."
  (interactive "P")
  (im/hide-or-show-buffer "*ielm*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-at-bottom) (direction . right)))))
      (ielm))))

(defun im/toggle-gnus ()
  "Toggle show the *Group* buffer."
  (interactive)
  (im/hide-or-show-buffer "*Group*"
    (let ((display-buffer-alist '(("*" (display-buffer-reuse-window display-buffer-in-direction) (direction . right)))))
      (display-buffer it))
    (user-error "You should start GNUS first")))

(defun im/toggle-common-lisp-dev-buffer ()
  "Toggle show SLIME/SLY buffer."
  (interactive)
  (let ((display-buffer-alist '(("*" (display-buffer-reuse-window %display-buffer-in-direction-or-below-selected) (direction . right)))))
    (im/hide-or-show-buffer (lambda (b) (string-match-p "^\\*sl.*-m?repl.*" (buffer-name b)))
      (pop-to-buffer it)
      (condition-case nil
          (call-interactively ic/lisp-dev)
        (error (%lisp/devenv-nofound))))))

(defun im/toggle-diagnostics-buffer()
  "Show the error messages for flycheck or flymake."
  (interactive)
  (cond ((and (boundp 'flycheck-mode) flycheck-mode)
         (let ((buf flycheck-error-list-buffer))
           (call-interactively 'flycheck-list-errors)
           (select-window (get-buffer-window buf))))
        ((and (boundp 'flymake-mode) flymake-mode)
         (let ((buf (flymake--diagnostics-buffer-name)))
           (call-interactively 'flymake-show-diagnostics-buffer)
           (select-window (get-buffer-window buf))))
        (t (message "Nothing to do, check flycheck or flymake toggled?"))))


;;; Window

(x ace-window
   "Can `M-x windmove-swap-states-default-keybindings' then use S-M-arrow to swap."
   :config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defun im/swap-windows ()
  (interactive)
  (if (> (length (window-list)) 2)
      (ace-swap-window)
    (window-swap-states)))

(defun im/transpose-window-layout ()
  "Switch between vertical and horizontal split. It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun im/other-window-with-special ()
  (interactive)
  (cond
   ((equal (treemacs-current-visibility) 'visible)
    (let ((treemacs-select-when-already-in-treemacs 'goto-next))
      (treemacs-select-window)))
   (t (call-interactively #'other-window))))


;;; Pin window

(defvar im:pinned-windows nil)

(defun im:window-id (&optional window)
  ;; Is there a simpler way to get the window id?
  (unless window
    (setq window (selected-window)))
  (let ((ws (format "%s" window)))
    (string-match "^#<window \\([0-9]+\\)" ws)
    (string-to-number (match-string 1 ws))))

(defun im:pin-window-format ()
  (let ((pinned (cl-find (im:window-id) im:pinned-windows)))
    (set-window-dedicated-p nil pinned)
    (when pinned
      '(:propertize (:eval (if IS-G " 📌 " " [PIN] "))
                    local-map (keymap (mode-line keymap (mouse-1 . (lambda (e)
                                                                     (interactive "e")
                                                                     (im/toggle-pin-window (caadr e))
                                                                     (force-mode-line-update)))))
                    help-echo (cl-loop for p in (window-parameters)
                                       for s = (format "%s" p)
                                       collect (cl-subseq s 0 (min 60 (length s))) into rs
                                       finally (return
                                                (format "dedicate-flag: %s\n\n%s"
                                                        (window-dedicated-p)
                                                        (mapconcat #'identity rs "\n"))))))))

(defun im/toggle-pin-window (&optional window)
  "Pin current window.

- Avoid other buffers displayed in it, ie, dedicated.
- Avoid `delete-other-windows' and `other-window' affect it
"
  (interactive (list (get-buffer-window)))
  (let* ((id (im:window-id window))
         (pinned (cl-find id im:pinned-windows)))
    (set-window-parameter window 'no-other-window (not pinned))
    (set-window-parameter window 'no-delete-other-windows (not pinned))
    (if pinned
        (setq im:pinned-windows (cl-remove id im:pinned-windows))
      (push id im:pinned-windows))
    (pcase major-mode
      ('dired-mode (dired-hide-details-mode (if pinned -1 1))))))


;;; Toggle fullscreen

(defvar im:fullscreen-alist nil
  "Alist to save window configurations used to toggle fullscreen layout.
Item should be (win-id-list window-configuration buffers-info updated-at) style.")

(defun im/toggle-fullscreen (&optional arg)
  "Toggle fullscreen easily. Recommended binding to C-x 1 or F11."
  (interactive "P")
  (if (minibufferp) (exit-minibuffer))
  (cl-labels
      ((win-ids ()
         (let (ids) ; use win-ids to distinguish different layouts
           (walk-window-tree (lambda (w) (push (im:window-id w) ids)))
           ids))
       (save-layout ()
         (let ((win-ids (win-ids)) wc)
           (setq im:fullscreen-alist
                 (cl-remove-if (lambda (c) (equal (car c) win-ids)) im:fullscreen-alist)) ; ensure update
           (setq wc (list win-ids
                          (current-window-configuration)
                          (mapconcat (lambda (w) (buffer-name (window-buffer w))) (window-list) ", ")
                          (time-to-seconds)))
           (setq im:fullscreen-alist (cons wc im:fullscreen-alist))))
       (switch-layout ()
         (cl-labels
             ((wc-item (id-string) ; get the saved layout
                (assoc (mapcar #'string-to-number (split-string id-string ", ")) im:fullscreen-alist))
              (ids-str (ids)       ; concatenate ids as the key
                (mapconcat #'number-to-string ids ", "))
              (sort-fn (wcs)       ; sort by created/updated time
                (sort wcs (lambda (x y) (not (time-less-p (cadddr (wc-item x)) (cadddr (wc-item y))))))))
           (let* ((win-ids (win-ids))
                  (items (mapcar (lambda (wc)
                                   (let ((idstr (ids-str (car wc))))
                                     (when (equal win-ids (car wc))
                                       (setq idstr (propertize idstr 'face 'font-lock-comment-face)))
                                     (cons
                                      (concat idstr
                                              (propertize
                                               (concat
                                                (make-string (- 30 (length idstr)) ? )
                                                (format "%-30s" (format-seconds "%Y %D %H %M %z%S" (float-time (time-subtract (time-to-seconds) (cadddr wc)))))
                                                (caddr wc))
                                               'face 'font-lock-comment-face))
                                      wc)))
                                 im:fullscreen-alist))
                  (keys (lambda ()
                          (use-local-map (make-composed-keymap nil (current-local-map)))
                          (local-set-key (kbd "C-k") (lambda ()
                                                       (interactive)
                                                       (when-let (current (im:completion-compat :current))
                                                         (setq im:fullscreen-alist
                                                               (cl-remove-if (lambda (item) (equal (cadr (assoc current items)) (car item))) im:fullscreen-alist))
                                                         (throw 'fullscreen-minibuffer 'del))))
                          (local-set-key (kbd "C-S-k") (lambda ()
                                                         (interactive)
                                                         (setq im:fullscreen-alist nil)
                                                         (throw 'fullscreen-minibuffer 'clear)))))
                  (choosen (minibuffer-with-setup-hook keys
                             (catch 'fullscreen-minibuffer
                               (unless items (user-error "Nop"))
                               (completing-read "Layout: "
                                                (lambda (input pred action)
                                                  (if (eq action 'metadata)
                                                      `(metadata (category . toggle-fullscreen)
                                                                 (display-sort-function . ,#'sort-fn))
                                                    (complete-with-action action items input pred)))
                                                nil t nil nil (car items))))))
             (cond ((eq choosen 'del) (im/toggle-fullscreen 1))
                   ((eq choosen 'clear) (message "Clear done."))
                   (t (if-let (wc (cadr (cdr (assoc choosen items)))) (set-window-configuration wc))))))))
    ;; single window: try to choose and switch to another layout
    (if (equal (selected-window) (next-window))
        (if (zerop (length im:fullscreen-alist))
            (user-error "No saved window configuration found")
          (switch-layout))
      (if arg
          ;; call with prefix: switch layout only
          (progn
            (save-layout)
            (switch-layout))
        ;; multiple windows: add/update before delete others windows
        (save-layout)
        (let ((ignore-window-parameters t))
          (delete-other-windows))))))

(defun im/switch-layouts ()
  (interactive)
  (im/toggle-fullscreen 'switch))


;;; Treemacs

(x treemacs
   :ref "Alexander-Miller/treemacs"
   :bind ( :map treemacs-mode-map
           ([f2]             . treemacs-rename-file)
           ("D"              . treemacs-delete-file)
           ("m"              . im/transient-treemacs-mode)
           ("C-x d"          . treemacs-create-dir)
           ("C-x f"          . treemacs-create-file)
           ("C-x C-d"        . treemacs-create-dir)
           ("C-x C-f"        . treemacs-create-file)
           ("C-c C-u"        . treemacs-goto-parent-node)
           ([down-mouse-1]   . im:treemacs-leftclick-smart-action))
   :config
   (setq treemacs-follow-after-init t)
   (treemacs-fringe-indicator-mode 'only-when-focused))

(defun im:treemacs-dired-current ()
  "Open current path in `dired'."
  (interactive)
  (if-let (path (treemacs--prop-at-point :path))
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
  (setq mode-line-format
        `((:propertize
           "Treemacs: "
           face font-lock-property-name-face
           mouse-face font-lock-warning-face
           help-echo "Click to close"
           local-map (keymap (mode-line keymap (mouse-1 . treemacs-kill-buffer))))
          (:eval (treemacs-workspace->name (treemacs-current-workspace))))))

(transient-define-prefix im/transient-treemacs-mode ()
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


;;; Interface

(transient-define-prefix im/transient-windows () ; C-x w
  :transient-non-suffix 'transient--do-exit
  [:hide
   (lambda () t)
   ("0"  "a"  delete-windows-on)
   ("1"  "b"  delete-other-windows)
   ("2"  "c"  split-root-window-below)
   ("3"  "d"  split-root-window-right)
   ("B"  "e"  balance-windows-area)
   ("h"  "f"  shrink-window-horizontally :transient t)
   ("l"  "g"  enlarge-window-horizontally :transient t)
   ("j"  "h"  shrink-window :transient t)
   ("k"  "i"  enlarge-window :transient t)
   ("f"  "j"  im/treemacs-follow)
   ("a"  "k"  treemacs-add-and-display-current-project)
   ("n"  "l"  winner-undo :transient t)
   ("o"  "m"  im/other-window-with-special)]
  [[("s" "swap"         im/swap-windows)]
   [("x" "transpose"    im/transpose-window-layout)]
   [("b" "balance"      balance-windows)]
   [("d" "pin"          im/toggle-pin-window)]
   [("F" "follow-mode"  follow-mode)]
   [("p" (lambda () (!tdesc "n p" "winner")) winner-redo :format "%d" :transient t)]
   [("w" (lambda () (!tdesc "w a f" "treemacs")) treemacs :format "%d")]])

(provide 'imod-display)

;;; imod-display.el ends here
