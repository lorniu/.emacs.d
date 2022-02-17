;;; imod-completing.el --- Completing. -*- lexical-binding: t -*-

;; vertico/orderless/consult/embark/marginalia
;; 2021-10-30, migrate selectrum to vertico, and persist to orderless
;; 2021-12-06, migrate company to corfu/cape

;;; Code:

(x corfu
   :ref "minad/corfu"
   :init
   (setq corfu-auto nil)
   (setq corfu-auto-delay 0)
   (setq corfu-auto-prefix 1)
   (setq corfu-quit-at-boundary t)
   (setq corfu-preview-current nil)
   :bind
   ((corfu-map ([tab] . %first-yas-then-complete)))
   :init
   (dolist (m '(prog-mode sqlplus-mode org-mode java-mode sly-mrepl-mode))
     (let ((hook (intern (format "%s-hook" m))))
       (add-hook hook (lambda () (setq-local corfu-auto t)))))
   (corfu-global-mode 1))

(x cape
   :ref "minad/cape"
   :init
   (add-to-list 'completion-at-point-functions #'cape-file)
   (add-to-list 'completion-at-point-functions #'cape-abbrev)
   ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
   ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
   ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
   ;;(add-to-list 'completion-at-point-functions #'cape-line)
   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
   )

(x orderless
   :ref "oantolin/orderless"
   :init
   (setq tab-always-indent 'complete
         completion-styles '(substring orderless)
         completion-category-defaults nil
         completion-category-overrides '((file (styles . (partial-completion)))))
   :require t
   :config
   (advice-add #'completion-all-completions :around #'%completion-flex-way))

(defun %completion-flex-way (fn &rest args)
  (let* ((prefix (car args))
         (orderless-component-separator "[ .]")
         (completion-styles (cond ((string-match-p "^ " (buffer-name (current-buffer))) completion-styles) ; minibuffer
                                  ((and (string-match-p "\\." prefix) (> (length prefix) 2)) '(basic substring orderless))
                                  (t '(basic partial-completion)))))
    (apply fn args)))

(defun %first-yas-then-complete ()
  (interactive)
  (if yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (corfu-complete))))
    (corfu-complete)))



(x vertico
   :ref "minad/vertico"
   :init (vertico-mode))

(x consult/e
   :ref "minad/consult"
   :init
   (setq register-preview-delay 0
         register-preview-function #'consult-register-format)
   (advice-add #'register-preview :override #'consult-register-window)

   :defer-config
   (setq consult-narrow-key "<") ;
   (setq consult-preview-key (list (kbd "C-o")))
   (setq consult-project-root-function (lambda ()
                                         (when-let (project (project-current))
                                           (car (project-roots project)))))
   (setq consult-buffer-sources
         '(consult--source-hidden-buffer
           consult--my-source-buffer
           consult--my-source-erc-buffer
           consult--my-eaf-buffer
           consult--source-bookmark
           consult--source-recent-file
           consult--source-project-buffer
           consult--source-project-recent-file)))

(x embark/e
   :ref "oantolin/embark"
   :bind
   ((minibuffer-local-map
     (("M-o" . embark-act)))
    (embark-file-map
     (("l"   . vlf)
      ("C-j" . embark-dired-jump))))

   :defer-config
   (setq embark-prompter 'embark-completing-read-prompter))

(x embark-consult/d
   :after (embark consult)
   :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

(x marginalia
   :ref "minad/marginalia"
   :bind ((minibuffer-local-map ("M-t" . marginalia-cycle)))
   :init (marginalia-mode))


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
                                   (or
                                    (equal major-mode 'erc-mode)
                                    (equal major-mode 'eaf-mode)))
                                 (string-match-p filter x)))
                           (mapcar #'buffer-name (consult--buffer-query)))))))

(defvar consult--my-source-erc-buffer
  `(:name "irc/matrix"
          :narrow   ?i
          :category erc
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default  t
          :items
          ,(lambda ()
             (seq-filter (lambda (x)
                           (with-current-buffer x (equal major-mode 'erc-mode)))
                         (mapcar #'buffer-name (consult--buffer-query))))))

(defvar consult--my-eaf-buffer
  `(:name "eaf-qt"
          :narrow   ?e
          :category eaf
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default  nil
          :items
          ,(lambda ()
             (seq-filter (lambda (x)
                           (with-current-buffer x (equal major-mode 'eaf-mode)))
                         (mapcar #'buffer-name (consult--buffer-query))))))



(defvar myc-pages+ nil)

(defun im/pages+ ()
  "Fast jump to page position."
  (interactive)
  (consult--read (let (lst (delim "^\014"))
                   (cl-flet ((current-line ()
                               (skip-chars-forward " \t\n")
                               (cons (buffer-substring (point) (line-end-position)) (point))))
                     (save-excursion
                       (goto-char (point-min))
                       (save-restriction
	                     (if (and (save-excursion (re-search-forward delim nil t)) (= 1 (match-beginning 0)))
	                         (goto-char (match-end 0)))
	                     (push (current-line) lst)
	                     (while (re-search-forward delim nil t)
	                       (push (current-line) lst))))
                     (nreverse lst)))
                 :prompt "Pages: "
                 :require-match t
                 :sort nil
                 :history 'myc-pages+
                 :lookup (lambda (_ ps p)
                           (goto-char (cdr (assoc  p ps)))
                           (recenter-top-bottom 1))))



(defvar myc-views nil)

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
  (cl-labels ((view-exist-p (name)
                (assoc name myc-views))
              (view-config ()
                "Get the window configuration (layout)"
                (dolist (w (window-list))
                  (set-window-parameter w 'view-data
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
    (let ((current (cl-loop for w in (window-list)
                            for b = (window-buffer w)
                            for f = (buffer-file-name b)
                            collect (if f (file-name-nondirectory f) (buffer-name b)) into bs
                            finally (return (cl-loop for item in (sort bs #'string-lessp)
                                                     concat (format " %s" item) into rs
                                                     finally (return (concat "{}" rs)))))))
      (consult--read (cl-loop for view in myc-views
                              unless (string-equal (car view) current) collect (car view) into vs
                              with face = (if (view-exist-p current) 'font-lock-builtin-face `(:underline t :inherit font-lock-builtin-face))
                              finally (return (cons (propertize current 'face face) vs)))
                     :prompt "Views: "
                     :keymap (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "C-d")
                                           (myc-make-action (c)
                                             (when (y-or-n-p (format "Delete this item `%s' ? " c))
                                               (setq myc-views (cl-remove c myc-views :test 'string-equal :key 'car)))
                                             (im/views+)))
                               (define-key map (kbd "C-S-d")
                                           (myc-make-action ()
                                             (if (y-or-n-p (format "Clear *ALL* views? "))
                                                 (progn (setq myc-views nil) (message "Clear Done!"))
                                               (message "Nothing Done."))))
                               map)
                     :lookup (lambda (_ __ view)
                               (cond
                                ;; check
                                ((not (string-match-p "^{} " (or view "")))
                                 (message "Error view-name detected."))
                                ;; update/add
                                ((or (string-equal current view)
                                     (not (view-exist-p view)))
                                 (let ((x (assoc view myc-views))
                                       (config (view-config)))
                                   (if x (setcdr x (list config))
                                     (push (list (substring-no-properties view) config) myc-views))))
                                ;; switch
                                ((view-exist-p view)
                                 (let ((inhibit-message t))
                                   (delete-other-windows)
                                   (restore-view (cadr (assoc view myc-views)))))))
                     :sort nil))))



(defvar myc-rg-limit-length 3)

(defvar myc-rg-completion-groups
  '(("c" "current" (lambda (input) (cons 'dir (myc-rg-get-candicates input))))
    ("p" "project" (lambda (input) (if-let ((d (cdr (project-current))))
                                       (cons 'project (myc-rg-get-candicates input d))
                                     (user-error "No project found"))))
    ("n" "notes"   (lambda (input) (cons 'note (myc-rg-get-candicates input org-directory))))))

(defvar myc-rg-search-history nil)

(defvar myc-rg-cands nil "list: type cands normin dir prefix input")

(defmacro myc-rg-show-in-rg.el (cands)
  `(myc-make-action (c)
     ;; use rg.el to show the results in Occur buffer
     (require 'rg)
     (require 'compile)
     ;; jump to current candidate in the *rg* buffer.
     ;; rg implemented with `compile', so I make it work like below.
     ;; let-bound method not working, unkown reason.
     (let ((old-compilation-finish-functions compilation-finish-functions)
           (input (cl-third ,cands)))
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
       (cond ((eq (car ,cands) 'project) (rg-project input "*"))
             (t                          (rg input "*" (cl-fourth ,cands)))))))

(defun myc-rg-get-candicates (input &optional dir)
  (cl-loop with default-directory = (or dir default-directory)
           with cmd = (if (memq system-type '(ms-dos windows-nt))
                          "rg -M 1024 --with-filename --no-heading --line-number --color never -S -e <R> ."
                        "rg -M 1024 --with-filename --no-heading --line-number --color never -S -e <R>")
           with nin = (replace-regexp-in-string " +" (rx space) (replace-regexp-in-string "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2" input))
           with result = (shell-command-to-string (grep-expand-template cmd nin))
           for c in (split-string result "\n")
           if (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
           do (progn
                (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c))
           collect c into cs
           finally (return (list cs nin default-directory))))

(defun myc-rg-make-completion-table (input _pred action)
  (if (equal action 'metadata)
      '(metadata (category . ripgrep) (cycle-sort-function . identity) (display-sort-function . identity))
    (pcase-let* ((input (if (equal (car-safe action) 'boundaries) input (minibuffer-contents-no-properties)))
                 (regexp (format "^\\([%s]\\) +\\(.*\\)" (mapconcat #'car myc-rg-completion-groups "")))
                 (`(,prefix . ,in) (if (string-match regexp input) (cons (match-string 1 input) (match-string 2 input)) (cons nil input)))
                 (too-short-p (< (length in) myc-rg-limit-length)))
      (pcase action
        (`(boundaries . ,suffix)
         `(boundaries . ,(cons (if too-short-p (length input) (if prefix 2 0)) (length suffix))))
        (_ (let ((cands (if too-short-p
                            (list (format "At least %d chars" myc-rg-limit-length))
                          (let* ((config (if prefix (assoc prefix myc-rg-completion-groups) (car myc-rg-completion-groups)))
                                 (result (funcall (caddr config) in)))
                            (setq myc-rg-cands (append result (list prefix in input)))
                            (cadr result)))))
             (complete-with-action action cands "" nil)))))))

(defun im/search-ripgrep+ ()
  "Search with ripgrep.

Default, search for current directory, if the input begin with 'p ' then
will search current project, if begin with 'n ' then will search org-directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
  (interactive)
  (unless (executable-find "rg")
    (user-error "ripgrep must be installed."))
  (require 'grep)
  (require 'xref)
  (require 'consult)
  (let* ((word (im-thing-at-region-or-point
                (lambda ()
                  (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                    (if (and sym (> 50 (length symn) 3)) symn nil)))))
         (current (minibuffer-with-setup-hook
                      (lambda ()
                        (setq myc-rg-cands nil)
                        (let ((map (make-composed-keymap nil (current-local-map))))
                          (define-key map (kbd "C-c C-o") (myc-rg-show-in-rg.el myc-rg-cands))
                          (use-local-map map)))
                    (completing-read "Candidates: " #'myc-rg-make-completion-table nil nil word 'myc-rg-search-history)))
         (input (cl-sixth myc-rg-cands)))
    (if (string-match (format "\\`\\(?:[%s] \\)?\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" (mapconcat #'car myc-rg-completion-groups ""))
                      current)
        (let ((file-name (match-string-no-properties 1 current))
              (line-number (match-string-no-properties 2 current)))
          (xref-push-marker-stack) ; use M-, to go back!
          (setq isearch-string input)
          (isearch-update-ring isearch-string t)
          (find-file (expand-file-name file-name (cl-fourth myc-rg-cands)))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward input (point-at-eol) t)
          (recenter)
          ;; notify line
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'face '(:background "#aa3344"))
            (run-with-timer 0.3 nil (lambda () (delete-overlay ov))))
          (myc-history-replace 'myc-rg-search-history (cl-seventh myc-rg-cands)))
      (message "Bad candidate?"))))



(defun myc-history-replace (hist-sym value)
  "Replace origin history ITEM with this new VALUE."
  (set hist-sym (cdr (symbol-value hist-sym)))
  (add-to-history hist-sym value))

(cl-defmacro myc-make-action ((&rest args) &body body)
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
                     `((consult-vertico--candidate)
                       (minibuffer-contents))
                     (length args)))
     (abort-recursive-edit)))

(provide 'imod-completing)

;; imod-completing.el ends here
