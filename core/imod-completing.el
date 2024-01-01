;;; -*- lexical-binding: t -*-

;; vertico/orderless/consult/marginalia
;; 2021-10-30, migrate selectrum to vertico, and persist to orderless
;; 2021-12-06, migrate company to corfu/cape
;; 2023-03-19, vanilla *Completions* buffer is good enough now. https://robbmann.io/posts/emacs-29-completions/

;;; Code:

;;; Helpers

(defun im:smart-completions-sort (all)
  (pcase (im:completion-metadata-get 'category)
    ('kill-ring all)
    (_ (let ((hist (minibuffer-history-value)))
         (thread-first all
                       (sort (lambda (c1 c2) (< (length c1) (length c2))))
                       (sort (lambda (c1 c2) (> (length (member c1 hist))
                                                (length (member c2 hist))))))))))

(defun im/minibuffer-delete-char ()
  (interactive)
  (unless (and (eolp)
               (when (and (> (point) (minibuffer-prompt-end))
                          (eq 'file (im:completion-metadata-get 'category)))
                 (let ((path (buffer-substring (minibuffer-prompt-end) (point))))
                   (when (string-match-p "\\`~[^/]*/\\'" path)
                     (delete-minibuffer-contents)
                     (insert (expand-file-name path)))
                   (save-excursion
                     (let ((end (point)))
                       (goto-char (1- end))
                       (when (search-backward "/" (minibuffer-prompt-end) t)
                         (delete-region (1+ (point)) end)))))
                 t))
    (call-interactively #'delete-char)))

(defun im/minibuffer-kill-line ()
  (interactive)
  (if (eolp)
      (let ((contents (minibuffer-contents)))
        (delete-minibuffer-contents)
        (unless (string-match-p " URL" (minibuffer-prompt))
          (when (and IS-WIN (string= contents "~/"))
            (insert (file-name-as-directory (getenv "USERPROFILE"))))
          (when (and (eq 'file (im:completion-metadata-get 'category)) (not (string= contents "~/")))
            (insert "~/"))))
    (call-interactively #'kill-line)))

(defun im:capf-functions-of-mode (mode)
  "Get value of `completion-at-point-functions' by MODE."
  (condition-case _
      (with-temp-buffer (funcall mode) completion-at-point-functions)
    (error completion-at-point-functions)))

(defmacro im:with-completions-window (&rest body)
  `(when-let* ((window (or (get-buffer-window "*Completions*" 0)
                           (progn (minibuffer-completion-help) (get-buffer-window "*Completions*" 0)))))
     (with-selected-window window (switch-to-completions) ,@body)))

(cl-macrolet ((make-minibuffer-command (name &rest body)
                `(defun ,(intern (format "im/minibuffer-%s" name)) ()
                   (interactive)
                   (im:with-completions-window ,@body))))
  (make-minibuffer-command next-page            (scroll-up-command))
  (make-minibuffer-command previous-page        (scroll-down-command))
  (make-minibuffer-command end-of-buffer        (goto-char (point-max)))
  (make-minibuffer-command beginning-of-buffer  (goto-char (point-min)))
  (make-minibuffer-command recenter-top-bottom  (recenter-top-bottom)))

(defun im:completion-compat (type)
  "Bridge of different completion-plugins."
  (cond ((get-buffer-window "*Completions*" 0)
         (im:with-completions-window
          (pcase type
            (:index (let ((first (save-excursion (first-completion) (array-current-line))))
                      (- (array-current-line) first)))
            (:total (let ((first (save-excursion (first-completion) (array-current-line))))
                      (- (save-excursion (goto-char (point-max)) (array-current-line)) first -1)))
            (:current (get-text-property (point) 'completion--string))
            (:delete (let ((inhibit-read-only t) (kill-whole-line t))
                       (beginning-of-line) (kill-line) (set-buffer-modified-p nil)))
            ((pred numberp) (let ((first (save-excursion (first-completion) (array-current-line))))
                              (goto-char (point-min))
                              (forward-line (+ first type)))))))
        (vertico-mode
         (pcase type
           (:index (if (> vertico--index -1) vertico--index))
           (:total vertico--total)
           (:current (and (> vertico--index -1) (nth vertico--index vertico--candidates)))
           (:delete (setq vertico--candidates (im:remove-nth vertico--index vertico--candidates))
                    (setq vertico--total (length vertico--candidates)))
           ((pred numberp) (vertico--goto vertico--index))))))

(defun im:completion-metadata-get (what)
  "Return completion for WHAT: category/display-sort-function/etc."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       what))))



(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion)))
      completion-category-defaults nil)

(setq minibuffer-completion-auto-choose t
      completion-show-help nil
      completion-auto-help 'always
      completion-show-inline-help nil
      completion-auto-select t
      completions-format 'one-column
      completions-max-height 10
      completions-detailed t
      completions-sort #'im:smart-completions-sort)

(xzz minibuffer
  :bind ( :map minibuffer-local-map
          ("C-d"      . im/minibuffer-delete-char)
          ("C-k"      . im/minibuffer-kill-line)
          :map minibuffer-local-completion-map
          ("SPC"      . nil)
          ("<down>"   . minibuffer-next-completion)
          ("<up>"     . minibuffer-previous-completion)
          ("C-n"      . minibuffer-next-completion)
          ("C-p"      . minibuffer-previous-completion)
          ("C-v"      . im/minibuffer-next-page)
          ("M-v"      . im/minibuffer-previous-page)
          ("C-l"      . im/minibuffer-recenter-top-bottom)
          ("M->"      . im/minibuffer-end-of-buffer)
          ("M-<"      . im/minibuffer-beginning-of-buffer)
          :map completion-list-mode-map
          ("e"        . switch-to-minibuffer)
          :map completion-in-region-mode-map
          ("<down>"   . minibuffer-next-completion)
          ("<up>"     . minibuffer-previous-completion)
          ("C-n"      . minibuffer-next-completion)
          ("C-p"      . minibuffer-previous-completion)
          ("C-v"      . im/minibuffer-next-page)
          ("M-v"      . im/minibuffer-previous-page)
          ("C-l"      . im/minibuffer-recenter-top-bottom)
          ("M->"      . im/minibuffer-end-of-buffer)
          ("M-<"      . im/minibuffer-beginning-of-buffer))
  :config
  (defun:hook minibuffer-setup-hook ()
    (setq-local truncate-lines nil))
  (defun:hook completion-list-mode-hook ()
    "Custom *Completions* buffer."
    (setq-local display-line-numbers-offset -1)
    (display-line-numbers-mode +1)
    (setq-local mode-line-format nil)))



(xzz vertico
  :ref "minad/vertico"
  :init (vertico-mode 1))

(xzz marginalia
  :ref "minad/marginalia"
  :bind (:map minibuffer-local-map ("M-t" . marginalia-cycle))
  :init (marginalia-mode))

(xzz orderless/e
  :ref "oantolin/orderless")

(define-minor-mode im/smart-completion-styles-mode nil :global t
  (if im/smart-completion-styles-mode
      (defun:around completion-all-completions (fn &rest args)
        (if (or (memq (cadr args) '(im:rg-completion-table)))
            (apply fn args) ; exceptions
          (let* ((prefix (car args))
                 (orderless-component-separator "[ .]")
                 (completion-styles (cond ((string-match-p "^ " (buffer-name (current-buffer))) completion-styles) ; minibuffer
                                          ((and (string-match-p "\\." prefix) (> (length prefix) 2)) '(basic substring orderless))
                                          (t '(basic partial-completion)))))
            (apply fn args))))
    (advice-remove #'completion-all-completions #'imadv:completion-all-completions)))

(im/smart-completion-styles-mode 1)



(xzz corfu
  :ref "minad/corfu"
  :bind (:map corfu-map ([tab] . im:first-yas-then-corfu))
  :init
  (setq corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-preview-current nil)

  (dolist (m '(prog-mode sqlplus-mode org-mode java-mode sly-mrepl-mode typescript-mode))
    (let ((hook (intern (format "%s-hook" m))))
      (add-hook hook (lambda () (setq-local corfu-auto t)))))

  (global-corfu-mode 1)

  (require 'corfu-grouping)
  (require 'corfu-nerd-icons))

(xzz cape
  :ref "minad/cape"
  :init
  (dolist (v (list #'cape-file #'cape-abbrev
                   ;; #'cape-dabbrev #'cape-keyword #'cape-ispell #'cape-dict
                   ;; #'cape-symbol #'cape-line #'cape-sgml #'cape-rfc1345
                   ))
    (add-to-list 'completion-at-point-functions v)))

(defun im:first-yas-then-corfu ()
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



(xzz consult
  :ref "minad/consult"
  :init
  (setq consult-preview-key "C-o"
        register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))



(defun im/pages+ ()
  "Fast jump to page position."
  (interactive)
  (let* ((delim "^\014")
         (width (length (number-to-string (line-number-at-pos (point-max)))))
         (fmt (format "%%%dd: " width))
         (page-header (lambda ()
                        (concat
                         (propertize (format fmt (line-number-at-pos)) 'face 'line-number)
                         (buffer-substring (point) (line-end-position)))))
         pages)
    (save-excursion
      (goto-char (point-min))
      (push (cons (funcall page-header) (point-min)) pages)
      (while (re-search-forward delim nil t)
        (skip-chars-forward " \t\n")
        (push (cons (funcall page-header) (point)) pages)))
    (setq pages (nreverse pages))
    (if (cdr pages) ; more than just the first-page entry
        (let* ((curr (cl-find-if (lambda (p) (<= (cdr p) (point))) (reverse pages)))
               (choice (completing-read "Pages: "
                                        (lambda (input pred action)
                                          (if (eq action 'metadata)
                                              '(metadata (display-sort-function . identity))
                                            (complete-with-action action pages input pred)))
                                        nil t nil 'im:pages (car curr)))
               (pos (cdr (assoc choice pages))))
          (when pos
            (goto-char pos)
            (recenter-top-bottom 2)))
      (user-error "No pages found"))))
