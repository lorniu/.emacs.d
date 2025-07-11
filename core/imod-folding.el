;;; -*- lexical-binding: t -*-

;; - C-x $, indent based.
;; - hideshow, syntax based.
;; - outline-minor-mode, headline based.
;; - fold-this, region/token based.
;; - narrow/imenu

;; - Folding, ;; {{{ this way ;; }}}
;; - vimish-fold, based on `fold-this', like folding
;; - Origami, try and removed, I don't like it. 20180111
;; - hide-comnt.el, make comments invisible, I prefer `im/hs-hide-all-comments'

;;; Code:

(defface im/hs-ellipse-face '((t :inherit warning))
  "Hideshow ellipse."
  :group 'imfine)

(defface im/outline-ellipse-face '((t :weight thin))
  "Outline ellipse."
  :group 'imfine)

(xzz outline
  :diminish outline-minor-mode
  :hook ((prog-mode text-mode conf-mode) . outline-minor-mode)
  :config
  (defvar-local outline-fold-all-flag nil)
  (defvar-local outline-narrow-flag nil)
  (defun:hook outline-minor-mode-hook/add-face-to-invisible-ellipse ()
    (let ((display-table (or buffer-display-table (make-display-table))))
      (unless buffer-display-table
        (setq buffer-display-table display-table))
      (set-display-table-slot display-table 4
                              (vconcat (mapcar (lambda (c)
                                                 (make-glyph-code c 'im/outline-ellipse-face))
                                               "..."))))))

(xzz hideshow/e
  :diminish hs-minor-mode
  :hook ((prog-mode nxml-mode sgml-mode web-mode mhtml-mode) . hs-minor-mode)
  :bind ( :map imfine-mode-map
          ("C-c f"   . im/smart-folding)
          ("C-c F"   . im/smart-folding-all)
          ("C-c M-f" . (lambda () (interactive) (im/smart-folding-all 1)))
          ("C-c C-f" . im/transient-fold)
          :map hs-minor-mode-map
          ([(M-down-mouse-1)] . nil)
          ([(M-mouse-1)] . nil))
  :config
  (setopt hs-allow-nesting t
          hs-set-up-overlay
          (lambda (ov)
            (let ((m (make-sparse-keymap)))
              (define-key m [return] 'hs-show-block)
              (define-key m [mouse-1] 'hs-show-block)
              (when (eq 'code (overlay-get ov 'hs))
                (let ((s (overlay-start ov)) pre)
                  (cond ((derived-mode-p 'lisp-data-mode)
                         (setq pre " "))
                        ((string-match-p "^[ \t]*{" (buffer-substring (save-excursion (goto-char s) (line-beginning-position)) s))
                         (setq pre (concat " " (buffer-substring s (- s 1))))
                         (setf (overlay-start ov) (save-excursion (goto-char s) (skip-chars-backward "\t\n {") (point)))))
                  (overlay-put ov 'display
                               (concat pre
                                       (propertize
                                        (format "...%d..." (count-lines (overlay-start ov) (overlay-end ov)))
                                        'face 'im/hs-ellipse-face 'cursor t 'pointer 'hand)))
                  (overlay-put ov 'keymap m)))
              (when (eq 'comment (overlay-get ov 'hs))
                (overlay-put ov 'display "...")
                (overlay-put ov 'keymap m))))))

(xzz fold-this/e
  :ref "magnars/fold-this.el")

(xzz hide-lines
  :ref "vapniks/hide-lines")

(xzz edit-indirect
  :ref "Fanael/edit-indirect")

(xzz narrow-indirect
  "Utils of indirect buffer (C-x 4 c)"
  :commands (ni-narrow-to-defun-indirect-other-window
             ni-narrow-to-region-indirect-other-window
             ni-narrow-to-page-indirect-other-window)
  :ref "https://www.emacswiki.org/emacs/NarrowIndirect")

(advice-add 'set-selective-display
            :filter-args (lambda (args)
                           (if (or (car args) selective-display)
                               args
                             (list (1+ (current-column)))))
            '((name . set-selective-display-from-cursor-column)))

(defvar-local outline-prefer-p nil)

(defun:around forward-comment//avoid-across-page-break (fn count)
  "When forward comment, don't across page-break!"
  (let ((o (point)))
    (funcall fn count)
    (let* ((p (point))
           (q (save-excursion
                (goto-char o)
                (when (funcall (if (> count 0) #'re-search-forward #'re-search-backward) "^\f$" nil t)
                  (if (> count 0)
                      (progn
                        (beginning-of-line)
                        (skip-chars-backward " \t\n")
                        (if (> p (point)) (point)))
                    (end-of-line)
                    (skip-chars-forward " \t\n")
                    (if (> (point) p) (point)))))))
      (prog1 (not (= o (point)))
        (if (and q (> q o)) (goto-char q))))))



(defun im/smart-folding ()
  (interactive)
  (cond
   ;; if fold found, unfold
   ((%fold-this-some (point)) (fold-this-unfold (point)))
   ((ignore-errors (hs-overlay-at (line-end-position))) (call-interactively 'hs-show-block))
   ((and (eq (char-after) 10) (%fold-this-some (- (point) 1))) (fold-this-unfold (- (point) 1)))
   ;; if region found, fold-this
   ((use-region-p)
    (call-interactively 'fold-this))
   ;; if outline and folded, show it
   ((and outline-minor-mode
         (not (derived-mode-p 'markdown-mode))
         (not (derived-mode-p 'org-mode))
         (not (derived-mode-p 'python-mode))
         (equal (ignore-errors (outline--cycle-state)) 'hide-all))
    (call-interactively 'outline-cycle))
   ;; if {{{...}}} in comments, fold-this
   ((and (im:in-comment-p)
         (string-match "\\({{{\\|}}}\\)" (buffer-substring (line-beginning-position) (line-end-position))))
    (let ((tag (match-string 1 (buffer-substring (line-beginning-position) (line-end-position)))))
      (if (string-equal tag "{{{")
          (when-let* ((end (save-mark-and-excursion
                             (catch 'here
                               (while (search-forward "}}}" nil t)
                                 (when (im:in-comment-p)
                                   (throw 'here (line-end-position))))))))
            (fold-this (line-beginning-position) end))
        (when-let* ((beg (save-mark-and-excursion
                           (catch 'here
                             (while (search-backward "{{{" nil t)
                               (when (im:in-comment-p)
                                 (throw 'here (line-beginning-position))))))))
          (fold-this beg (line-end-position))))))
   ;; if on the line-break char, toggle fold-page
   ((or (and (eq (char-before) 10) (eq (char-after) 12))
        (and (eq (char-before) 10) (eq (char-after) 10) (eq (char-after (+ (point) 1)) 12))
        (and (eq (char-before) 10) (eq (char-after) 10) (eq (char-before (- (point) 1)) 12)))
    (save-mark-and-excursion
      (mark-page)
      (call-interactively 'fold-this)
      (backward-char)))
   ;; if gnus-summary-mode
   ((derived-mode-p 'gnus-summary-mode)
    (call-interactively
     (if (cl-find-if (lambda (ov) (eq (overlay-get ov 'invisible) 'gnus-sum)) (overlays-at (line-end-position)))
         'gnus-summary-show-thread
       'gnus-summary-hide-thread)))
   ;; if org-mode
   ((and (derived-mode-p 'org-mode) (org-at-heading-p))
    (call-interactively 'org-cycle))
   ;; if markdown-mode
   ((and (eq major-mode 'markdown-mode) (markdown-heading-at-point))
    (call-interactively 'markdown-cycle))
   ;; if outline-mode
   ((and (derived-mode-p 'outline-mode) (outline-on-heading-p))
    (call-interactively 'outline-cycle))
   ;; if eshell-mode
   ((derived-mode-p 'eshell-mode)
    (outline-toggle-children))
   ;; if toggle-hs-hiding/outline-minor-mode
   ((or hs-minor-mode outline-minor-mode)
    (cond ((and outline-minor-mode outline-prefer-p)
           (call-interactively 'outline-cycle))
          (hs-minor-mode
           (save-mark-and-excursion
             (call-interactively 'hs-toggle-hiding)))
          ((and outline-minor-mode (outline-on-heading-p))
           (call-interactively 'outline-cycle))))
   ;; otherwise
   (t (message "Nothing to fold/unfold."))))

(defun im/smart-folding-all (&optional arg)
  (interactive "P")
  (cond
   ;; if fold-this overlay found, unfold all
   ((or (%fold-this-some (point)) (and (eq (char-after) 10) (%fold-this-some (- (point) 1))))
    (call-interactively 'fold-this-unfold-all))
   ;; if {{{...}}} in comments, fold-this all
   ((and (im:in-comment-p)
         (string-match-p "{{{\\|}}}" (buffer-substring (line-beginning-position) (line-end-position))))
    (save-mark-and-excursion
      (let (beg end ps)
        (goto-char (point-min))
        (while (search-forward "{{{" nil t)
          (when (im:in-comment-p)
            (setq beg (point))
            (setq end (catch 'out
                        (while (search-forward "}}}" nil t)
                          (when (im:in-comment-p)
                            (throw 'out (point))))))
            (push (cons beg end) ps)))
        (cl-loop for (b . e) in ps do (fold-this b e)))))
   ;; if gnus-summary-mode
   ((derived-mode-p 'gnus-summary-mode)
    (call-interactively
     (if (cl-find-if (lambda (ov) (eq (overlay-get ov 'invisible) 'gnus-sum)) (car (overlay-lists)))
         'gnus-summary-show-all-threads
       'gnus-summary-hide-all-threads)))
   ;; if org-mode
   ((derived-mode-p 'org-mode)
    (call-interactively 'org-global-cycle))
   ;; mode as outline-mode
   ((derived-mode-p '(markdown-mode outline-mode emacs-news-mode eshell-mode))
    (call-interactively 'im/outline-toggle-all))
   ((or hs-minor-mode outline-minor-mode)
    (cond ((and outline-minor-mode outline-prefer-p)
           (call-interactively 'im/outline-toggle-all))
          (hs-minor-mode
           (let ((hs-hide-comments-when-hiding-all (not arg))) ; C-u then not hide comments
             (hs-minor-mode
              (save-mark-and-excursion
                (if (seq-find
                     (lambda (ov) (and (overlayp ov) (overlay-get ov 'hs)))
                     (overlays-in (point-min) (point-max)))
                    (hs-show-all)
                  (hs-hide-all))
                (recenter)))))
          ((and outline-minor-mode (outline-on-heading-p))
           (call-interactively 'im/outline-toggle-all))))))

(defun im/hs-hide-all-comments ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-normalize-vars)
    (while (re-search-forward comment-start-skip nil 'noerror)
      (pcase-let ((`(,beg ,end) (hs-inside-comment-p)))
        (when (and beg
                   (save-excursion
                     (goto-char beg)
                     (string-match-p "^[ \n\t]*$" (buffer-substring (line-beginning-position) (point)))))
          (hs-hide-comment-region beg end))
        (goto-char end))))
  (beginning-of-line)
  (run-hooks 'hs-hide-hook))

(defun im/hs-hide-level-and-comments ()
  (interactive)
  (let ((hs-allow-nesting t))
    (call-interactively 'im/hs-hide-all-comments)
    (call-interactively 'hs-hide-level)))

(defun im/outline-toggle-all ()
  (interactive)
  (if outline-fold-all-flag
      (progn (outline-show-all) (setq outline-fold-all-flag nil))
    (outline-show-all)
    (outline-hide-region-body (point-min) (point-max))
    (setq outline-fold-all-flag t)))

(defun im/outline-toggle-narrow ()
  (interactive)
  (if outline-narrow-flag
      (progn (outline-show-all)
             (setq outline-narrow-flag nil))
    (outline-hide-other)
    (setq outline-narrow-flag t)))

(defun im/outline-regexp-this ()
  (interactive)
  (outline-minor-mode 1)
  (let ((reg (read-string "Set outline-regexp to: " outline-regexp 'outline-regexp-his)))
    (when (> (length reg) 0)
      (setq-local outline-regexp reg))))

(defun im/outline-toggle-prefer ()
  (interactive)
  (setq-local outline-prefer-p (not outline-prefer-p))
  (message (concat (propertize "Priod: " 'face 'minibuffer-prompt)
                   (let ((s (if outline-prefer-p "Outline > Hideshow" "Hideshow > Outline")))
                     (put-text-property 8 10 'face 'warning s)
                     s))))

(defun %fold-this-some (point)
  (cl-find-if (lambda (ov) (eq (overlay-get ov 'type) 'fold-this)) (overlays-at point)))



(defmacro outline-set-rule (modes regexp &rest level-args)
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for m in (if (listp modes) modes (list modes))
            for sym = (intern (format "%s-hook/outline" (symbol-name m)))
            collect `(defun:hook ,sym ()
                       (outline-minor-mode 1)
                       (setq-local outline-regexp ,regexp)
                       ,(if level-args `(setq-local outline-level (lambda () ,@level-args)) t)))))

(outline-set-rule eshell-mode
  eshell-prompt-regexp 1)

(outline-set-rule Custom-mode
  "S" 1)

(outline-set-rule yaml-mode
  (concat "\\( *\\)\\(?:\\(?:--- \\)?\\|{\\|\\(?:[-,] +\\)+\\) *"
          "\\(?:" yaml-tag-re " +\\)?"
          "\\(" yaml-bare-scalar-re "\\) *:"
          "\\(?: +\\|$\\)")
  (- (match-end 1) (match-beginning 1)))



(defmacro hs-set-rule (modes &optional start end comment-start forward-sexp-func adjust-beg-func)
  (declare (indent 1))
  `(with-eval-after-load 'hideshow
     ,@(cl-loop for m in (if (listp modes) modes (list modes))
                collect `(setq hs-special-modes-alist (cl-remove ',m hs-special-modes-alist :key #'car))
                collect `(add-to-list 'hs-special-modes-alist ',(list m start end comment-start forward-sexp-func adjust-beg-func)))))

(hs-set-rule nxml-mode
  "<!--\\|<[^/>]*[^/]>"
  "-->\\|</[^/>]*[^/]>"
  "<!--"
  nxml-forward-element)

(hs-set-rule sgml-mode
  "<!--\\|<[^/>]*[^/]>"
  "-->\\|</[^/>]*[^/]>"
  "<!--"
  sgml-skip-tag-forward)

(hs-set-rule mhtml-mode
  "{\\|<[^/>]*?"
  "}\\|</[^/>]*[^/]>"
  "<!--"
  (lambda (_)
    (require 'sgml-mode)
    (pcase (get-text-property (point) `mhtml-submode)
      (`nil (mhtml-forward 1))
      (`submode (forward-sexp)))))

(hs-set-rule web-mode
  "<!--\\|<[^/>]*[^/]>\\|{"
  "-->\\|</[^/>]*[^/]>\\|}"
  "<!--\\|/[*/]")

(hs-set-rule ruby-mode
  "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
  "end\\|[]}]"
  "#\\|=begin"
  ruby-forward-sexp)

(hs-set-rule haskell-mode
  "^[a-zA-Z\f]+"
  ""
  "--\\|{-"
  (lambda (_)
    (let ((beg (point)))
      (if (and (equal (char-before) 10) (equal (char-after) 12))
          (forward-line)
        (forward-paragraph)
        (search-forward-regexp "^[-a-zA-Z\f]" nil t)
        (backward-char 2)
        (if (>= beg (point)) (goto-char (point-max)))
        (if (equal (char-before) 10) (backward-char))))))

(hs-set-rule latex-mode
  '("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
  "\\\\end{[a-zA-Z*]+}"
  "%"
  (lambda (_arg)
    ;; Don't fold whole document, that's useless
    (unless (save-excursion (search-backward "\\begin{document}" (line-beginning-position) t))
      (LaTeX-find-matching-end))))

(hs-set-rule matlab-mode
  "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
  "end"
  nil
  (lambda (_arg) (matlab-forward-sexp)))

(hs-set-rule makefile-gmake-mode
  "^[a-zA-Z$.].*:"
  ""
  "#"
  (lambda (_)
    (let* ((cur (point))
           (nx (save-excursion (makefile-next-dependency) (point)))
           (np (save-excursion (forward-paragraph) (point)))
           (nn (if (= cur nx) np (min nx np))))
      (goto-char nn)
      (forward-comment -100))))



(transient-define-prefix im/transient-fold () ; C-c C-f
  [:hide
   (lambda () t)
   ("s"     "" hs-show-block)
   ("h"     "" hs-hide-block)
   ("l"     "" hs-hide-level)
   ("L"     "" im/hs-hide-level-and-comments)
   ("c"     "" im/hs-hide-all-comments)

   ("F"     "" im/outline-toggle-all)
   ("N"     "" im/outline-toggle-narrow)

   ("C-s"   "" outline-show-subtree)
   ("C-e"   "" outline-show-entry)
   ("C-k"   "" outline-show-branches)
   ("C-TAB" "" outline-show-children)

   ("C-t"   "" outline-hide-body)
   ("C-o"   "" outline-hide-other)
   ("C-l"   "" outline-hide-leaves)
   ("C-d"   "" outline-hide-subtree)
   ("C-q"   "" outline-hide-sublevels)

   ("P"     "" im/outline-toggle-prefer)

   ("u"     "" outline-up-heading :transient t)
   ("n"     "" outline-next-visible-heading :transient t)
   ("p"     "" outline-previous-visible-heading :transient t)

   (","     "" fold-this-save-this)
   ]
  [[:description
    (lambda () (concat (unless outline-prefer-p "*") "Hideshow"))
    ("S" (lambda () (!tdesc "Ss" "   Show"))          hs-show-all           :format "%d")
    ("H" (lambda () (!tdesc "HhLlc" "Hide"))          hs-hide-all           :format "%d")
    ]
   [:description
    (lambda () (concat (if outline-prefer-p "*") "Outline (C-c @)"))
    ("A" (lambda () (!tdesc "A^sek " "  Show"))       outline-show-all      :format "%d")
    ("a" (lambda () (!tdesc "a^toldq" " Hide"))       outline-hide-entry    :format "%d")
    ]
   [""
    ("E" (lambda () (!tdesc "  E/P  " " Set-Outline")) im/outline-regexp-this :format "%d")
    ("f" (lambda () (!tdesc "fFN unp" " Toggle/Navi")) outline-cycle        :format "%d")
    ]
   ["Folding"
    ("#" "Fold-This" fold-this)
    ("." (lambda () (!tdesc ",." "Save/Load Fold"))   fold-this-restore     :format "%d")
    ]
   ["Misc"
    ("$" (lambda () (!tdesc "$" "Selective"))         set-selective-display :format "%d")
    ]
   ])

(transient-define-prefix im/transient-narrow () ; C-x n
  [:hide
   (lambda () t)
   ("D" "" ni-narrow-to-defun-indirect-other-window)
   ("N" "" ni-narrow-to-region-indirect-other-window)
   ("P" "" ni-narrow-to-page-indirect-other-window)
   ]
  [:if-mode
   'org-mode
   [("b" (lambda () (format "(%s) %s"
                            (propertize "org" 'face 'warning)
                            (!tdesc "b" "Narrow to Block")))
     org-narrow-to-block :format "%d")]
   [("e" "Narrow to Element" org-narrow-to-element)]
   [("s" "Narrow to Subtree" org-narrow-to-subtree)]
   [("B" "Indirect Narrow"   org-tree-to-indirect-buffer)]
   ]
  [:if-mode
   'restclient-mode
   [("c"
     (lambda () (format "(%s) %s" (propertize "restclient" 'face 'warning) (!tdesc "c" "Narrow to Current")))
     restclient-narrow-to-current :format "%d")]
   ]
  [[("d" (lambda () (!tdesc "d"     "Narrow to Defun"))   narrow-to-defun  :format "%d")]
   [("n" (lambda () (!tdesc "n"     "Narrow to Region"))  narrow-to-region :format "%d")]
   [("p" (lambda () (!tdesc "p"     "Narrow to Page"))    narrow-to-page   :format "%d")]
   [("O" (lambda () (!tdesc "D/N/P" "Indirect-Narrow"))   ignore           :format "%d")]
   [("w" "Widen" widen)]
   ]
  (interactive)
  (let ((transient-show-popup -0.3))
    (transient-setup 'im/transient-narrow)))
