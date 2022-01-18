;;; imod-folding.el --- Code Folding -*- lexical-binding: t -*-

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

(x hideshow/e
   "Use `hs-special-modes-alist' to set rules: (MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC)"
   :blackout hs-minor-mode
   :hook ((prog-mode-hook nxml-mode-hook sgml-mode-hook web-mode-hook mhtml-mode-hook) . hs-minor-mode)
   :bind ((im-keys-mode-map
           ("C-c f"   . im/smart-folding)
           ("C-c F"   . im/smart-folding-all)
           ("C-c M-f" . (lambda () (interactive) (im/smart-folding-all 1)))
           ("C-c C-f" . imtt/transient-fold))
          (hs-minor-mode-map
           ([(M-down-mouse-1)] . nil)
           ([(M-mouse-1)] . hs-mouse-toggle-hiding)))
   :init
   (setq hs-allow-nesting t)
   :defer-config
   (defvar %hs-display-line-overlay-map
     (let ((m (make-sparse-keymap)))
       (define-key m [return] 'hs-show-block)
       (define-key m [mouse-1] 'hs-show-block)
       m))
   (defun %hs-display-line-counts (ov)
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
                               'face font-lock-warning-face 'cursor t 'pointer 'hand)))
         (overlay-put ov 'keymap %hs-display-line-overlay-map))))
   (setq hs-set-up-overlay '%hs-display-line-counts))

(x outline
   "Use `outline-regexp' to set rule of buffer."
   :blackout outline-minor-mode
   :hook ((prog-mode-hook text-mode-hook conf-mode-hook) . outline-minor-mode)
   :defer-config
   (defvar-local outline-fold-all-flag nil)
   (defvar-local outline-narrow-flag nil))

(x fold-this/e
   :ref "magnars/fold-this.el")

(x hide-lines
   :ref "vapniks/hide-lines")

(x narrow-indirect
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
         (not (derived-mode-p 'org-mode))
         (equal (ignore-errors (outline--cycle-state)) 'hide-all))
    (call-interactively 'outline-cycle))
   ;; if {{{...}}} in comments, fold-this
   ((and (im-in-comment-p)
         (string-match "\\({{{\\|}}}\\)" (buffer-substring (line-beginning-position) (line-end-position))))
    (let ((tag (match-string 1 (buffer-substring (line-beginning-position) (line-end-position)))))
      (if (string-equal tag "{{{")
          (when-let ((end (save-mark-and-excursion
                            (catch 'here
                              (while (search-forward "}}}" nil t)
                                (when (im-in-comment-p)
                                  (throw 'here (line-end-position))))))))
            (fold-this (line-beginning-position) end))
        (when-let ((beg (save-mark-and-excursion
                          (catch 'here
                            (while (search-backward "{{{" nil t)
                              (when (im-in-comment-p)
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
   ;; if org-mode
   ((and (derived-mode-p 'org-mode) (org-at-heading-p))
    (call-interactively 'org-cycle))
   ;; if markdown-mode
   ((and (derived-mode-p 'markdown-mode) (markdown-heading-at-point))
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
   ((and (im-in-comment-p)
         (string-match-p "{{{\\|}}}" (buffer-substring (line-beginning-position) (line-end-position))))
    (save-mark-and-excursion
      (let (beg end ps)
        (goto-char (point-min))
        (while (search-forward "{{{" nil t)
          (when (im-in-comment-p)
            (setq beg (point))
            (setq end (catch 'out
                        (while (search-forward "}}}" nil t)
                          (when (im-in-comment-p)
                            (throw 'out (point))))))
            (push (cons beg end) ps)))
        (cl-loop for (b . e) in ps do (fold-this b e)))))
   ;; if org-mode
   ((derived-mode-p 'org-mode)
    (call-interactively 'org-global-cycle))
   ;; if markdown-mode
   ((derived-mode-p 'markdown-mode)
    (call-interactively 'im/outline-toggle-all))
   ;; if outline-mode
   ((derived-mode-p 'outline-mode)
    (call-interactively 'im/outline-toggle-all))
   ;; if eshell-mode
   ((derived-mode-p 'eshell-mode)
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
      (progn (outline-show-all) (setq outline-narrow-flag nil))
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
                     (put-text-property 8 10 'face 'font-lock-warning-face s)
                     s))))

(defun %fold-this-some (point)
  (cl-find-if (lambda (ov) (eq (overlay-get ov 'type) 'fold-this)) (overlays-at point)))



(defmacro hs-set-rule (modes &optional start end comment-start forward-sexp-func adjust-beg-func)
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for m in (if (listp modes) modes (list modes))
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
  "def\\|do\\|{"
  "end\\|}"
  "#"
  (lambda (_) (ruby-end-of-block)))

(hs-set-rule haskell-mode
  "^[a-zA-Z]+"
  ""
  "--\\|{-"
  (lambda (_)
    (let ((beg (point)))
      (if (and (equal (char-before) 10) (equal (char-after) 12))
          (forward-line)
        (forward-paragraph)
        (search-forward-regexp "^[-a-zA-Z]" nil t)
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



(defmacro outline-set-rule (modes regexp &rest level-args)
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for m in (if (listp modes) modes (list modes))
            for sym = (intern (format "%s-hook/outline" (symbol-name m)))
            collect
            `(defun:hook ,sym ()
               (outline-minor-mode 1)
               (setq-local outline-regexp ,regexp)
               ,(if level-args `(setq-local outline-level (lambda () ,@level-args)) t)))))

(outline-set-rule eshell-mode
  eshell-prompt-regexp 1)

(outline-set-rule Custom-mode
  "S" 1)

(outline-set-rule python-mode
  "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)"
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

(outline-set-rule yaml-mode
  (concat "\\( *\\)\\(?:\\(?:--- \\)?\\|{\\|\\(?:[-,] +\\)+\\) *"
          "\\(?:" yaml-tag-re " +\\)?"
          "\\(" yaml-bare-scalar-re "\\) *:"
          "\\(?: +\\|$\\)")
  (- (match-end 1) (match-beginning 1)))



(transient-define-prefix imtt/transient-fold () ; C-c C-f
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

(transient-define-prefix imtt/transient-narrow () ; C-x n
  [:hide
   (lambda () t)
   ("D" "" ni-narrow-to-defun-indirect-other-window)
   ("N" "" ni-narrow-to-region-indirect-other-window)
   ("P" "" ni-narrow-to-page-indirect-other-window)
   ]
  [:if-mode
   'org-mode
   [("b" (lambda () (format "(%s) %s"
                            (propertize "org" 'face 'font-lock-warning-face)
                            (!tdesc "b" "Narrow to Block")))
     org-narrow-to-block :format "%d")]
   [("e" "Narrow to Element" org-narrow-to-element)]
   [("s" "Narrow to Subtree" org-narrow-to-subtree)]
   [("B" "Indirect Narrow"   org-tree-to-indirect-buffer)]
   ]
  [:if-mode
   'restclient-mode
   [("c"
     (lambda () (format "(%s) %s" (propertize "restclient" 'face 'font-lock-warning-face) (!tdesc "c" "Narrow to Current")))
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
    (transient-setup 'imtt/transient-narrow)))

(provide 'imod-folding)

;;; imod-folding.el ends here
