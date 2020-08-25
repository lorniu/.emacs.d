;;; imccc.el --- Something about completing, just as Selectrum/Ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x selectrum/e
   :ref "raxod502/selectrum"
   :init
   (setq selectrum-count-style 'current/matches)
   (selectrum-mode +1)
   (selectrum-prescient-mode +1)
   (prescient-persist-mode +1)
   (icomplete-mode -1)

   (cl-defun selectrum-str-add-prop (str value &optional (where 'selectrum-candidate-full))
     (add-text-properties 0 (length str) (list where value) str)
     str)
   (cl-defmacro selectrum-make-action ((&rest args) &body body)
     (declare (indent 1))
     `(lambda ()
        (interactive)
        (put 'quit 'error-message "")
        (run-at-time nil nil
                     (lambda (,@args)
                       (put 'quit 'error-message "Quit")
                       (with-demoted-errors "Error: %S"
                         ,@body))
                     ,@(seq-take
                        `((if selectrum--refined-candidates (nth selectrum--current-candidate-index selectrum--refined-candidates))
                          selectrum--refined-candidates
                          (selectrum-get-current-input)
                          selectrum--current-candidate-index)
                        (length args)))
        (abort-recursive-edit))))

(x amx
   "Amx is a fork of Smex"
   "It's an alternative interface for M-x")

(x prescient
   :init
   (setq prescient-save-file (locc "prescient-save.el")))



(defvar selectrum-switch-buffer+ nil)
(defvar selectrum-imenu+ nil)
(defvar selectrum-pages+ nil)
(defvar selectrum-mark-ring+ nil)

(defun im/switch-buffer+ ()
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (candidates (lambda (input)
                       (let* ((cb (window-buffer (minibuffer-selected-window)))
                              (norm-buffer-name (lambda (buffer)
                                                  (with-current-buffer buffer
                                                    (cond ((eq major-mode 'dired-mode)
                                                           (propertize (buffer-name) 'face dired-directory-face))
                                                          ((null buffer-file-name)
                                                           (buffer-name))
                                                          ((string-equal "org" (file-name-extension buffer-file-name))
                                                           (propertize (buffer-name) 'face 'font-lock-builtin-face))
                                                          (t
                                                           (buffer-name))))))
                              (norm-file-name (lambda (file)
                                                (propertize file 'face dired-ignored-face)))
                              (buffers (mapcar (lambda (b) (funcall norm-buffer-name b))
                                               (cl-remove cb (buffer-list))))
                              (files (mapcar (lambda (f) (funcall norm-file-name f))
                                             (seq-difference recentf-list (mapcar #'buffer-file-name (buffer-list)) 'string=)))
                              (candidates ()))
                         (cond ((string-prefix-p " " input)
                                (setq input (string-trim-left input))
                                (setq candidates (cl-remove-if-not (lambda (name) (string-prefix-p " " name)) buffers)))
                               ((string-prefix-p "b " input)
                                (setq input (string-trim (substring input 1)))
                                (setq candidates (cl-remove-if (lambda (name) (string-prefix-p " " name)) buffers)))
                               ((string-prefix-p "f " input)
                                (setq input (string-trim (substring input 1)))
                                (setq candidates files))
                               ((string-prefix-p "p " input)
                                (setq input (string-trim (substring input 1)))
                                (setq candidates
                                      (aif (projectile-project-root)
                                        (append
                                         (cl-remove-if (lambda (name)
                                                         (or (not (projectile-project-buffer-p (get-buffer name) it)) (string-prefix-p " " name)))
                                                       buffers)
                                         (cl-remove-if-not (lambda (f) (string-prefix-p it (expand-file-name f))) files)))))
                               (t
                                (setq candidates (append
                                                  (cl-remove-if (lambda (name) (string-prefix-p " " name)) buffers)
                                                  files))))
                         `((candidates . ,candidates)
                           (input . ,input)))))
         (cand (selectrum-read "Switch to: " candidates
                               :require-match t
                               :history 'selectrum-switch-buffer+
                               :may-modify-candidates t)))
    (when (plusp (length cand))
      (if (member cand recentf-list)
          (find-file cand)
        (switch-to-buffer cand)))))

(defun im/imenu+ ()
  "Choose from `imenu' just like `counsel-imenu'."
  (interactive)
  (require 'imenu)
  (let* ((selectrum-should-sort-p nil)
         (candidates (let* ((imenu-auto-rescan t)
                            (items (imenu--make-index-alist t)))
                       ;; remove *Rescan*
                       (setq items (delete (assoc "*Rescan*" items) items))
                       ;; special mode
                       (when (eq major-mode 'emacs-lisp-mode)
                         (let ((fns (cl-remove-if #'listp items :key #'cdr)))
                           (if fns (setq items (nconc (cl-remove-if #'nlistp items :key #'cdr) `(("Functions" ,@fns)))))))
                       ;; refine
                       (cl-labels ((get-candidates
                                    (alist &optional prefix)
                                    (cl-mapcan
                                     (lambda (elm)
                                       (if (imenu--subalist-p elm)
                                           (get-candidates
                                            (cl-loop for (e . v) in (cdr elm)
                                                     collect (cons e (if (integerp v) (copy-marker v) v)))
                                            (concat prefix (if prefix ".") (car elm)))
                                         (let ((key (concat (if prefix (concat (propertize prefix 'face 'font-lock-keyword-face) ": "))
                                                            (car elm))))
                                           (list (cons key (cons key (if (overlayp (cdr elm)) (overlay-start (cdr elm)) (cdr elm))))))))
                                     alist)))
                         (setq items (get-candidates items)))
                       ;; sort
                       (cl-sort items #'string< :key #'car)))
         (cand (completing-read "Imenu: " (mapcar #'car candidates) nil t nil selectrum-imenu+)))
    (imenu (cdr (cl-find cand candidates :test #'string= :key #'car)))))

(defun im/pages+ ()
  "Fast jump to position just like package `counsel-page'."
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (cands (let ((lst)
                      (delim "^\014")
                      (current-line (lambda ()
                                      (skip-chars-forward " \t\n")
                                      (selectrum-str-add-prop (buffer-substring (point) (line-end-position))
                                                              (number-to-string (point))))))
                  (save-excursion
                    (goto-char (point-min))
                    (save-restriction
	                  (if (and (save-excursion (re-search-forward delim nil t))
		                       (= 1 (match-beginning 0)))
	                      (goto-char (match-end 0)))
	                  (push (funcall current-line) lst)
	                  (while (re-search-forward delim nil t)
	                    (push (funcall current-line) lst))))
                  (nreverse lst)))
         (cand (completing-read "Pages: " cands nil t nil selectrum-pages+)))
    (goto-char (string-to-number cand))
    (recenter-top-bottom 1)))

(defun im/yank-pop+ (&optional arg)
  "Call `yank-pop' with ARG when appropriate, or offer completion."
  (interactive "*P")
  (if arg (yank-pop arg)
    (let* ((old-last-command last-command)
           (selectrum-should-sort-p nil)
           (enable-recursive-minibuffers t)
           (text (completing-read
                  "Yank: "
                  (cl-remove-duplicates
                   kill-ring :test #'string= :from-end t)
                  nil t nil nil))
           ;; Find `text' in `kill-ring'.
           (pos (cl-position text kill-ring :test #'string=))
           ;; Translate relative to `kill-ring-yank-pointer'.
           (n (+ pos (length kill-ring-yank-pointer))))
      (unless (string= text (current-kill n t))
        (error "Could not setup for `current-kill'"))
      ;; Restore `last-command' over Selectrum commands.
      (setq last-command old-last-command)
      ;; Delegate to `yank-pop' if appropriate or just insert.
      (if (eq last-command 'yank)
          (yank-pop n) (insert-for-yank text)))))

(defun im/mark-ring+ ()
  (interactive)
  (if (null (marker-position (mark-marker))) (user-error "No marks currently exist.")
    (let* ((selectrum-should-sort-p nil)
           (cands (save-excursion
                    (cl-loop with window-width = (window-body-width (minibuffer-window))
                             for marker in (cons (mark-marker)
                                                 (cl-remove-duplicates mark-ring :test '= :key 'marker-position))
                             for pos          = (goto-char (marker-position marker))
                             for line-beg-pos = (line-beginning-position)
                             for str-pos      = (- pos line-beg-pos)
                             for line-string  = (buffer-substring line-beg-pos (line-end-position))
                             collect pos          into marker-positions
                             collect line-string  into highlighted-candidates

                             for      line-number = (line-number-at-pos pos t)
                             collect  line-number into line-numbers
                             maximize line-number into max-line-number

                             collect  str-pos into column-numbers
                             maximize str-pos into max-col-number

                             finally return
                             (cl-loop with form = (concat "%0"   (number-to-string (length (number-to-string max-line-number)))
                                                          "d,%0" (number-to-string (length (number-to-string max-col-number)))
                                                          "d: %s")
                                      for marker-pos in marker-positions
                                      for line-num   in line-numbers
                                      for col-num    in column-numbers
                                      for cand       in highlighted-candidates
                                      for str        =  (format form line-num col-num cand)
                                      collect (cons (if (> (length str) window-width)
                                                        (concat (substring str 0 (- window-width 10)) "...")
                                                      str)
                                                    marker-pos)))))
           (cand (completing-read "Go to marker: " cands nil t nil selectrum-mark-ring+)))
      (goto-char (cdr (assoc cand cands))))))



(defvar selectrum-views nil)

(defun im/views+ ()
  "Toggle the window layout with this single command.

You will see the candidates after invoke this command.

Select a candidate can:
- Add the current window configuration to the candidates
- Update the current window configuration
- Toggle to the choosen window layout.

'C-d' to delete current candidate, and 'C-S-d' to delete all.
"
  (interactive)
  (cl-labels
      ((view-exist-p (name) (assoc name selectrum-views))
       (view-config ()
                    "Get the window configuration (layout)"
                    (dolist (w (window-list))
                      (set-window-parameter
                       w 'view-data
                       (with-current-buffer (window-buffer w)
                         (cond (buffer-file-name
                                (list 'file buffer-file-name (point)))
                               ((eq major-mode 'dired-mode)
                                (list 'file default-directory (point)))
                               (t (list 'buffer (buffer-name) (point)))))))
                    (let ((window-persistent-parameters
                           (append window-persistent-parameters (list (cons 'view-data t)))))
                      (current-window-configuration)))
       (restore-view (view)
                     "Restore the window configuration (layout)."
                     (cond ((window-configuration-p view) ; window
                            (set-window-configuration view)
                            (dolist (w (window-list))
                              (with-selected-window w
                                (restore-view (window-parameter w 'view-data)))))
                           ((eq (car view) 'file) ; file
                            (let* ((name (nth 1 view)) buffer)
                              (cond ((setq buffer (get-buffer name))
                                     (switch-to-buffer buffer nil 'force-same-window))
                                    ((file-exists-p name)
                                     (find-file name))))
                            (goto-char (nth 2 view)))
                           ((eq (car view) 'buffer) ; buffer
                            (switch-to-buffer (nth 1 view))
                            (goto-char (nth 2 view))))))
    (let* ((selectrum-should-sort-p nil)
           (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                       (define-key map (kbd "C-d")
                                         (selectrum-make-action (c)
                                           (when (y-or-n-p (format "Delete this item `%s' ? " c))
                                             (setq selectrum-views
                                                   (cl-remove c selectrum-views :test 'string-equal :key 'car)))
                                           (im/views+)))
                                       (define-key map (kbd "C-S-d")
                                         (selectrum-make-action ()
                                           (if (y-or-n-p (format "Clear *ALL* views? "))
                                               (progn (setq selectrum-views nil)
                                                      (message "Clear Done!"))
                                             (message "Nothing Done."))))
                                       map))
           (current (cl-loop for w in (window-list)
                             for b = (window-buffer w)
                             for f = (buffer-file-name b)
                             collect (if f (file-name-nondirectory f) (buffer-name b)) into bs
                             finally (return (cl-loop for item in (sort bs #'string-lessp)
                                                      concat (format " %s" item) into rs
                                                      finally (return (concat "{}" rs))))))
           (views (cl-loop with face = (if (view-exist-p current) 'font-lock-builtin-face `(:underline t :inherit font-lock-builtin-face))
                           for view in selectrum-views
                           unless (string-equal (car view) current) collect (car view) into vs
                           finally (return (cons (propertize current 'face face) vs))))
           (view (completing-read "Views: " views nil nil nil nil current)))
      (cond
       ;; check
       ((not (string-match-p "^{} " (or view "")))
        (message "Error view-name detected."))
       ;; update/add
       ((or (string-equal current view)
            (not (view-exist-p view)))
        (let ((x (assoc view selectrum-views))
              (config (view-config)))
          (if x (setcdr x (list config))
            (push (list (substring-no-properties view) config) selectrum-views))))
       ;; switch
       ((view-exist-p view)
        (let ((inhibit-message t))
          (delete-other-windows)
          (restore-view (cadr (assoc view selectrum-views)))))))))



(defvar selectrum-search-rg-history nil)

(defun im/search-rg+ ()
  "Search like 'counsel-rg'.

Default, search for current directory, if the input begin with 'p ' then
will search current project, if begin with 'o ' then will search org-directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
  (interactive)
  (unless (executable-find "rg")
    (user-error "ripgrep must be installed."))
  (require 'xref)
  (let* (type
         input
         (dir default-directory)
         (word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                   (if (and sym (> 50 (length symn) 3)) symn nil))))
         (command (if (memq system-type '(ms-dos windows-nt))
                      "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R> ."
                    "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"))
         (cands (lambda (in)
                  (let ((msg)
                        (prop (lambda (cs)
                                (mapcar (lambda (c)
                                          (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
                                            (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                                            (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c))
                                          c)
                                        cs))))
                    (cond
                     ;; search current project
                     ((string-prefix-p "p " in)
                      (cond ((not (project-current))
                             (setq msg "This is not in a project."))
                            ((< (length in) 5)
                             (setq msg "Search in current project, input should more than 3."))
                            (t
                             (setq type 'project)
                             (setq dir (cdr (project-current)))
                             (setq in (cl-subseq in 2)))))
                     ;; search org-directory
                     ((string-prefix-p "o " in)
                      (cond ((not (file-exists-p org-directory))
                             (setq msg "Org Directory not exist?"))
                            ((< (length in) 5)
                             (setq msg "Search in org-directory, input should more than 3."))
                            (t
                             (setq type 'org)
                             (setq dir org-directory)
                             (setq in (cl-subseq in 2)))))
                     ;; search current directory
                     (t (if (< (length in) 3)
                            (setq msg "Input should more than 3."))
                        (setq type nil)
                        (setq dir default-directory)))
                    ;; take space in INPUT as .*?
                    ;; take m-space as [[:blank:]]
                    (setq input
                          (replace-regexp-in-string
                           " +" "[[:blank:]]"
                           (replace-regexp-in-string
                            "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2" in)))
                    (if msg
                        (prog1 nil
                          (setq-local selectrum-refine-candidates-function
                                      (lambda (_ __) (list msg))))
                      (kill-local-variable 'selectrum-refine-candidates-function)
                      (let* ((default-directory dir)
                             (cs (split-string
                                  (shell-command-to-string (grep-expand-template command input)) "\n")))
                        `((candidates . ,(funcall prop cs))
                          (input . ,input)))))))
         (cand (let ((selectrum-should-sort-p nil)
                     (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                                 (define-key map (kbd "C-c C-o")
                                                   (selectrum-make-action (c)
                                                     ;; use rg.el to show the results in Occur buffer
                                                     (require 'rg)
                                                     (require 'compile)
                                                     ;; jump to current candidate in the *rg* buffer.
                                                     ;; rg implemented with `compile', so I make it work like below.
                                                     ;; let-bound method not working, unkown reason.
                                                     (let ((old-compilation-finish-functions compilation-finish-functions))
                                                       (setq compilation-finish-functions
                                                             (list
                                                              (lambda (_a _b)
                                                                (unwind-protect
                                                                    (progn
                                                                      (pop-to-buffer (current-buffer))
                                                                      (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                                                                        (let ((file-name (match-string-no-properties 1 c))
                                                                              (line-number (match-string-no-properties 2 c)))
                                                                          (if rg-group-result
                                                                              (progn
                                                                                (re-search-forward (format "^File: %s" file-name) nil t)
                                                                                (re-search-forward (format "^ *%s" line-number) nil t)
                                                                                (re-search-forward input (point-at-eol) t))
                                                                            (re-search-forward (format "%s:%s:" file-name line-number) nil t)
                                                                            (re-search-forward input (point-at-eol) t)))))
                                                                  (setq compilation-finish-functions old-compilation-finish-functions)))))
                                                       ;; dispatch to rg.el search.
                                                       (cond ((eq type 'project) (rg-project input "*"))
                                                             (t                  (rg input "*" dir))))))
                                                 map)))
                 (selectrum-read "rg: " cands
                                 :initial-input word
                                 :may-modify-candidates t
                                 :history 'selectrum-search-rg-history
                                 :require-match t))))
    (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
        (let ((file-name (match-string-no-properties 1 cand))
              (line-number (match-string-no-properties 2 cand)))
          (setq isearch-string input)
          (isearch-update-ring isearch-string t)
          (xref-push-marker-stack) ; use M-, to go back!
          (find-file (expand-file-name file-name dir))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward input (point-at-eol) t)
          (recenter))
      (message "Bad candidate?"))))



(x embark/+
   "Now not in mepa. Update from git. 20200801. v0.6."
   :ref ("oantolin/embark"
         "embark source: https://raw.githubusercontent.com/oantolin/embark/master/embark.el"))


(provide 'imccc)

;; imccc.el ends here
