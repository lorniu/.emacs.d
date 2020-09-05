;;; iccc.el --- Something about completing. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun selectrum-my-history-replace (hist-sym value)
  "Replace origin history ITEM with this new VALUE."
  (set hist-sym (cdr (symbol-value hist-sym)))
  (add-to-history hist-sym value))

(cl-defun selectrum-str-add-prop (str value &optional (where 'selectrum--candidate-full))
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
                       (minibuffer-contents)
                       selectrum--current-candidate-index)
                     (length args)))
     (abort-recursive-edit)))



(x selectrum/e
   :ref "raxod502/selectrum"
   :init
   (setq selectrum-count-style 'current/matches)
   (selectrum-mode +1)
   (selectrum-prescient-mode +1)
   (prescient-persist-mode +1)
   (icomplete-mode -1))

(x prescient
   :init
   (setq prescient-save-file (locc "prescient-save.el")))

(x marginalia
   :ref "minad/marginalia"
   :bind (:map minibuffer-local-map ("M-t" . marginalia-cycle))
   :init (marginalia-mode))

(x consult
   :ref "minad/consult"
   :init
   (setq register-preview-delay 0
         register-preview-function #'consult-register-format)
   (advice-add #'register-preview :override #'consult-register-window)

   :config
   (setq consult-narrow-key "<") ;
   (setq consult-preview-key (list (kbd "C-o")))
   (setq consult-project-root-function (lambda ()
                                         (when-let (project (project-current))
                                           (car (project-roots project)))))
   (setq consult-buffer-sources
         '(consult--source-hidden-buffer
           consult--my-source-buffer
           consult--my-source-erc-buffer
           consult--source-file
           consult--source-bookmark
           consult--source-project-buffer
           consult--source-project-file)))

(x embark
   :ref "oantolin/embark"
   :after (selectrum)
   :bind
   (:map minibuffer-local-map
         (("M-o" . embark-act))
         :map embark-file-map
         (("l"   . vlf)
          ("C-j" . embark-dired-jump)))

   :config
   (setq embark-prompter 'embark-completing-read-prompter))

(x embark-consult/d
   :after (embark consult)
   :hook
   (embark-collect-mode . embark-consult-preview-minor-mode))


;;; Enhanced consult-buffer

(defvar consult--my-source-buffer
  `(:name "Buffer"
          :narrow   ?b
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default  t
          :items
          ,(lambda ()
             (let ((filter (consult--regexp-filter consult-buffer-filter)))
               (seq-remove (lambda (x)
                             (or (with-current-buffer x
                                   (equal major-mode 'erc-mode))
                                 (string-match-p filter x)))
                           (consult--cached-buffer-names))))))

(defvar consult--my-source-erc-buffer
  `(:name "irc/matrix"
          :narrow   ?e
          :category erc
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default  t
          :items
          ,(lambda ()
             (seq-filter (lambda (x)
                           (with-current-buffer x (equal major-mode 'erc-mode)))
                         (consult--cached-buffer-names)))))



(defvar selectrum-pages+ nil)

(defun im/pages+ ()
  "Fast jump to position just like package `counsel-page'."
  (interactive)
  (let* ((selectrum-should-sort nil)
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
    (let* ((selectrum-should-sort nil)
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
(defvar-local selectrum-search-rg-follow nil)

(defun im/search-rg+ ()
  "Search like 'counsel-rg'.

Default, search for current directory, if the input begin with 'p ' then
will search current project, if begin with 'o ' then will search org-directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
  (interactive)
  (unless (executable-find "rg") (user-error "ripgrep must be installed."))
  (require 'xref)
  (require 'grep)
  (let* ((type) (input)
         (selectrum-should-sort nil)
         (dir default-directory)
         (win (get-buffer-window (current-buffer)))
         (word (im/thing-at-region-or-point
                (lambda ()
                  (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                    (if (and sym (> 50 (length symn) 3)) symn nil)))))
         (command (if (memq system-type '(ms-dos windows-nt))
                      "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R> ."
                    "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"))
         (prop-cands (lambda (cands)
                       (cl-loop for c in cands
                                if (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c) do
                                (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                                (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c)
                                collect c)))
         (notify-line (lambda ()
                        (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                          (overlay-put ov 'face '(:background "#aa3344"))
                          (run-with-timer 0.3 nil (lambda () (delete-overlay ov))))))
         (cands (lambda (in)
                  (let (msg)
                    (cond ((string-prefix-p "p " in)  ; current-project
                           (cond ((not (project-current))
                                  (setq msg "This is not in a project."))
                                 ((< (length in) 5)
                                  (setq msg "Search in current project, input should more than 3."))
                                 (t
                                  (setq type 'project)
                                  (setq dir (cdr (project-current)))
                                  (setq in (cl-subseq in 2)))))
                          ((string-prefix-p "o " in)  ; org-directory
                           (cond ((not (file-exists-p org-directory))
                                  (setq msg "Org Directory not exist?"))
                                 ((< (length in) 5)
                                  (setq msg "Search in org-directory, input should more than 3."))
                                 (t
                                  (setq type 'org)
                                  (setq dir org-directory)
                                  (setq in (cl-subseq in 2)))))
                          (t (if (< (length in) 3)  ; current directory
                                 (setq msg "Input should more than 3."))
                             (setq type nil)
                             (setq dir default-directory)))
                    ;; take space in INPUT as .*?
                    ;; take m-space as [[:blank:]]
                    (setq input (replace-regexp-in-string " +" "[[:blank:]]"
                                                          (replace-regexp-in-string
                                                           "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2" in)))
                    (if msg
                        (prog1 nil
                          (setq-local selectrum-refine-candidates-function
                                      (lambda (_ __) (list msg))))
                      (kill-local-variable 'selectrum-refine-candidates-function)
                      (let* ((default-directory dir)
                             (cs (split-string (shell-command-to-string (grep-expand-template command input)) "\n")))
                        `((candidates . ,(funcall prop-cands cs))
                          (input . ,input)))))))
         (handler (lambda (cand &optional no-record)
                    (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                        (let ((file-name (match-string-no-properties 1 cand))
                              (line-number (match-string-no-properties 2 cand)))
                          (unless no-record
                            (xref-push-marker-stack) ; use M-, to go back!
                            (setq isearch-string input)
                            (isearch-update-ring isearch-string t))
                          (find-file (expand-file-name file-name dir))
                          (goto-char (point-min))
                          (forward-line (1- (string-to-number line-number)))
                          (re-search-forward input (point-at-eol) t)
                          (recenter)
                          (funcall notify-line))
                      (message "Bad candidate?"))))
         (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                     (define-key map (kbd "C-o")
                                       (lambda ()
                                         (interactive)
                                         (select-window win)
                                         (funcall handler (nth selectrum--current-candidate-index selectrum--refined-candidates) t)
                                         (select-window (active-minibuffer-window))))
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
    (funcall handler
             (selectrum--read "rg: " cands
                              :initial-input word
                              :history 'selectrum-search-rg-history
                              :require-match t))
    (selectrum-my-history-replace 'selectrum-search-rg-history input)))


(provide 'iccc)

;; iccc.el ends here
