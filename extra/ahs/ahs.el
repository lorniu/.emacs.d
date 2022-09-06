;;; ahs.el --- auto-highlight-symbol, Automatic highlighting current symbol minor mode

;; Copyright (C) 2009 2010 Mitsuo Saito
;; Created date 2009-03-03 21:44 +0900

;; Author: Mitsuo Saito <arch320@NOSPAM.gmail.com>
;; Adapted-By: Gennadiy Zlobin <gennad.zlobin@NOSPAM.gmail.com>
;; Version: 1.55
;; Package-Version: 20130313.243
;; Keywords: highlight face match convenience
;; URL: http://github.com/gennad/auto-highlight-symbol/raw/master/auto-highlight-symbol.el
;; Compatibility: GNU Emacs 22.3 23.x 24.x later

;;; Code:

(require 'color)

(eval-when-compile
  (require 'cl)
  (defvar dropdown-list-overlays nil))

(defgroup ahs nil
  "Automatic highlighting current symbol minor mode"
  :group 'convenience)

;;
;; (@* "Custom variable" )
;;
(defcustom ahs-extra-modes
  '( nxml-mode
     outline-mode
     web-mode
     html-mode
     sgml-mode
     xml-mode )
  "Major modes `ahs-mode' can run on."
  :group 'ahs
  :type '(repeat symbol))

(defcustom ahs-suppress-log nil
  "*Non-nil means suppress log message."
  :group 'ahs
  :type 'boolean)

(defcustom ahs-log-echo-area-only t
  "*Non-nil means log doesn't display the `*Messages*' buffer."
  :group 'ahs
  :type 'boolean)

(defcustom ahs-decorate-log t
  "*Non-nil means decorate logs."
  :group 'ahs
  :type 'boolean)

(defcustom ahs-edit-mode-lighter-pair '( "[" . "]" )
  "Decorate mode line lighter in edit mode."
  :group 'ahs)

(defcustom ahs-select-invisible 'immediate
  "Behavior when selected symbol in hidden text.

When the value is
  `immediate' Open hidden text. When leaving opened text, close it immediately.
  `temporary' Open hidden text. When unhighlight or change range, close the opened texts except selected.
  `open'      Open hidden text permanently.
  `skip'      Select next visible symbol.

Affects only overlay(hidden text) has a property `isearch-open-invisible'."
  :group 'ahs)

(defcustom ahs-edit-mode-on-hook nil
  "Normal hook for run when entering edit mode."
  :group 'ahs
  :type 'hook)

(defcustom ahs-edit-mode-off-hook nil
  "Normal hook for run when go out edit mode."
  :group 'ahs
  :type 'hook)

(defvar ahs-idle-timer nil)

(defcustom ahs-idle-interval 30.0
  "Number of seconds to wait before highlighting symbol."
  :group 'ahs
  :type 'float
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (timerp ahs-idle-timer)
           (cancel-timer ahs-idle-timer)
           (setq ahs-idle-timer nil)
           (ahs-start-timer))))

(defcustom ahs-default-ranges '(ahs-range-defun ahs-range-buffer)
  "Default Range."
  :group 'ahs)

;;
;; (@* "Rules" )
;;
(defcustom ahs-case-fold-search t
  "*Non-nil means symbol search ignores case."
  :group 'ahs
  :type 'boolean)

(defconst ahs-default-symbol-regexp "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-]\\{2,\\}$"
  "Default symbol regular expression.")

(defcustom ahs-include ahs-default-symbol-regexp
  "Variable for start matching.

This variable can be set in three different types.

  1. `REGEXP' Regular expression.
    If symbol matches regular expression `REGEXP' then start highlighting.

  2. `my-include-function' Function predicate.
    If return value is Non-nil then start highlighting.
    Function is called with one argument, the symbol.

  3. `alist'
      '(
        ( emacs-lisp-mode . \"REGEXP\")          ;; Regular expression in emacs-lisp-mode
        ( php-mode        . my-include-function) ;; Function predicate in php-mode
        )
    If major mode not in list `ahs-default-symbol-regexp' will be used instead."

  :group 'ahs)

(defcustom ahs-exclude nil
  "Variable for inhibit highlighting.

This variable can be set in three different types.

  1. `REGEXP' Regular expression.
    If symbol matches regular expression `REGEXP' then inhibit highlighting.

  2. `my-exclude-function' Function predicate.
    If return value is Non-nil then inhibit highlighting.
    Function is called with one argument, the symbol.

  3. `alist'
      '(
        ( ruby-mode . \"\\_<\\(end\\|def\\|class\\)\\_>\") ;; Regular expression in ruby-mode
        ( dos-mode  . i-hate-wxxxxxs)                      ;; Function predicate in dos-mode
        )
      If major mode not in list all symbols can be highlighted."

  :group 'ahs)

(defcustom ahs-face-check-include-overlay nil
  "*Non-nil means face checks include overlay face."
  :group 'ahs
  :type 'boolean)

(defcustom ahs-inhibit-face-list
  '( font-lock-comment-delimiter-face
     font-lock-comment-face
     font-lock-doc-face
     font-lock-doc-string-face
     font-lock-string-face )
  "Face list for inhibit highlighting."
  :group 'ahs
  :type '(repeat symbol))

(defcustom ahs-definition-face-list
  '( font-lock-keyword-face
     font-lock-function-name-face
     font-lock-variable-name-face )
  "Face list for highlight definition."
  :group 'ahs
  :type  '(repeat symbol))

(defvar ahs-markup-modes '(web-mode sgml-mode mhtml-mode html-mode xml-mode))

;;
;; (@* "Face" )
;;
(defface ahs-face
  `((t (:background
        ,(condition-case nil (color-darken-name (face-attribute 'default :background) 10) (error 'unspecified))
        :underline nil :slant normal)))
  "Highlight the symbol using this face."
  :group 'ahs)

(defface ahs-range-buffer-face
  `((((background dark)) (:background unspecified :underline "orange"))
    (t (:background unspecified :underline "orangered")))
  "Face used in `whole buffer' range."
  :group 'ahs)

(defface ahs-range-defun-face
  `((((background dark)) (:background unspecified :underline "limegreen"))
    (t (:background unspecified :underline "darkgreen")))
  "Face used in `beginning of defun' range."
  :group 'ahs)

(defface ahs-range-page-face
  `((((background dark)) (:background unspecified :underline "gray"))
    (t (:background unspecified :underline "dimgray")))
  "Face used in `display' range."
  :group 'ahs)

(defface ahs-edit-mode-face
  '((t (:foreground "White" :background "Coral3")))
  "Face used in edit mode."
  :group 'ahs)

;;
;; (@* "Internal variable" )
;;
(defvar ahs-range-list nil)
(defvar ahs-search-work  nil)
(defvar ahs-need-fontify nil)

(defvar ahs-inhibit-modification-commands
  '( undo redo ))

(defvar ahs-unhighlight-allowed-commands
  '( universal-argument
     universal-argument-other-key
     ahs-back-to-start
     ahs-backward
     ahs-backward-definition
     ahs-display-stat
     ahs-edit-mode
     ahs-forward
     ahs-forward-definition ))

;; Buffer local variable

(defvar ahs-current-overlay      nil)
(defvar ahs-current-range        nil)
(defvar ahs-highlighted          nil)
(defvar ahs-inhibit-modification nil)
(defvar ahs-opened-overlay-list  nil)
(defvar ahs-overlay-list         nil)
(defvar ahs-start-modification   nil)
(defvar ahs-start-point          nil)

(make-variable-buffer-local 'ahs-current-overlay      )
(make-variable-buffer-local 'ahs-current-range        )
(make-variable-buffer-local 'ahs-highlighted          )
(make-variable-buffer-local 'ahs-inhibit-modification )
(make-variable-buffer-local 'ahs-opened-overlay-list  )
(make-variable-buffer-local 'ahs-overlay-list         )
(make-variable-buffer-local 'ahs-start-modification   )
(make-variable-buffer-local 'ahs-start-point          )

;;
;; (@* "Logging" )
;;
(defconst ahs-log-data
  '(;; range
    ( range-badcondition . "Range `%s' incorrect major-mode or condition property is `nil'.")
    ( range-changed      . "Current range has been changed to `%s'.")
    ( range-notfound     . "Range `%s' doesn't exist.")
    ( range-notrange     . "Range `%s' wrong type range.")

    ( range-error-log1   . "---- ahs-mode range error log ----")
    ( range-error-log2   . "%s in `%s' range `%s' property")
    ( range-error-log3   . "---- end")
    ( range-error-log4   . "Range error occurred. see *Messages*. Current range has been changed to `%s'.")

    ;; error
    ( error-ahs-disable  . "`ahs-mode' is not working at current buffer.")
    ( error-read-only    . "Buffer is read-only: `%s'")
    ( error-scan-sexp    . "%s: \"%s\" %s %s")

    ;; edit-mode
    ( turn-on-edit-mode  . "Entering edit mode. %s")
    ( turn-off-edit-mode . "Exited edit mode.")

    ;; misc
    ( no-symbol-at-point . "No symbol to highlight at point.")
    ( exist-elsewhere    . "%s symbols exist elsewhere.")
    ( stat               . "Current range `%s' matched %s  displayed %s  hidden %s  before %s  after %s.")
    ( self               . "%s")
    )
  "Log data")

(defmacro ahs-decorate-if (body face)
  `(if ahs-decorate-log
       (propertize ,body 'face ,face)
     ,body))

(defmacro ahs-log-format (key)
  `(cdr (assoc ,key ahs-log-data)))

(defun ahs-log (key &rest args)
  "Display log."
  (unless ahs-suppress-log
    (let* ((data (ahs-log-format key))
           (msg (apply 'format data args))
           (message-log-max
            (not ahs-log-echo-area-only)))
      (message "%s" msg))) nil)


;;; What is under point ? Word, Symbol or Region ?

(defvar imhs-symbol-type nil)
(defvar imhs-region-limit 3)

(defun imhs-bounds-at-point ()
  (if (and (use-region-p) (> (- (region-end) (region-beginning)) imhs-region-limit))
      (progn
        (setq imhs-symbol-type 'region)
        (cons (region-beginning) (region-end)))
    (setq imhs-symbol-type nil)
    (bounds-of-thing-at-point 'symbol)))

(defun imhs-search-symbol-regexp (symbol)
  (let ((fmt (cond ((eq imhs-symbol-type 'region) "\\(%s\\)")
                   (t "\\_<\\(%s\\)\\_>"))))
    (format fmt (regexp-quote symbol))))


;;
;; (@* "Range" )
;;
(defmacro ahs-range-regist (range-name body &optional docstring)
  "Macro of regist new range.

\(fn RANGE-NAME BODY [DOCSTRING])"

  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "ahs-range-%s" range-name))
       nil ,docstring)
     (setq ,(intern (format "ahs-range-%s" range-name)) ,body)
     (add-to-list 'ahs-range-list ',(intern (format "ahs-range-%s" range-name)) t)
     (defun ,(intern (format "ahs-chrange-%s" range-name)) ()
       (interactive)
       (ahs-change-range ',(intern (format "ahs-range-%s" range-name)))
       (when (called-interactively-p 'interactive)
         (ahs-idle-function)))))

(defun ahs-decorated-current-range-name ()
  "Return decorated current range's name."
  (let ((name (ahs-current-range-prop 'name)))
    (if ahs-decorate-log
        (propertize name 'face `(:foreground ,(face-attribute `,(ahs-current-range-prop 'face) :underline)))
      name)))

(defun ahs-range-error-message (err prop range)
  "Display range error message."
  (let ((ahs-suppress-log)
        (ahs-log-echo-area-only))
    (ahs-log 'range-error-log1)
    (ahs-log 'range-error-log2
             err (ahs-get-range-prop 'name range) prop) ;; infinite loop? if 'name is badly function
    (ahs-log 'range-error-log3)

    (ahs-change-range-internal (ahs-most-available-range))
    (ahs-log 'range-error-log4
             (ahs-decorated-current-range-name))))

(defun ahs-get-range-prop (prop range &optional arg)
  "Return value of the `PROP' property of the `RANGE' range."
  (let ((value (cdr (assoc prop (symbol-value range)))))
    (cond
     ((equal value 'abort) 'abort)          ;; abort
     ((equal prop 'face)                    ;; face
      (if (facep value)
          value
        'ahs-range-page-face))

     ((and (functionp value)
           (equal prop 'major-mode)) value) ;; major-mode
     ((functionp value)                     ;; function
      (condition-case err
          (if arg
              (funcall value arg)
            (funcall value))
        (error err
               (ahs-range-error-message err prop range)
               'abort)))

     ((null value) 'none)                   ;; property not found

     ((symbolp value)                       ;; symbol
      (ignore-errors
        (symbol-value value)))
     (t value))))                           ;; others

(defun ahs-current-range-prop (prop &optional arg)
  "Return value of the `PROP' property of the current range."
  (ahs-get-range-prop prop 'ahs-current-range arg))

(defun ahs-valid-range-p (range &optional range-name)
  "Return Non-nil if `RANGE' range can run."
  (setq range-name
        (or range-name
            (let ((name (format "%s" range)))
              (if (string-match "ahs-range-" name)
                  (substring name (match-end 0) (length name))
                name))))
  (cond
   ((not (boundp range))
    (ahs-log 'range-notfound range-name))
   ((not (memq range ahs-range-list))
    (ahs-log 'range-notrange range-name))
   ((not (memq range (ahs-runnable-ranges)))
    (ahs-log 'range-badcondition
             (ahs-get-range-prop 'name range)))
   (t t)))

(defun ahs-most-available-range ()
  (cl-find-if (lambda (range)
                (memq range (ahs-runnable-ranges)))
              ahs-default-ranges))

(defun ahs-runnable-ranges (&optional getnext)
  "Return list of runnable ranges."
  (cl-loop with current   = nil
           with available = nil

           for range in ahs-range-list
           for r = (symbol-value range)
           for mode = (ahs-get-range-prop 'major-mode range)

           when (equal r ahs-current-range) do (setq current range)

           when (or (equal 'none mode)
                    (and (listp mode)
                         (apply 'derived-mode-p mode))
                    (eq major-mode mode))
           when (ahs-get-range-prop 'condition range)
           collect range into available

           finally
           return (if getnext
                      (or (cadr (memq current available))
                          (car available))
                    available)))

(defun ahs-change-range-internal (range)
  "Current range change to `RANGE' range."
  (setq ahs-current-range (symbol-value range))
  (ahs-current-range-prop 'init))

;;
;; (@* "Built-in range" )
;;
(defvar ahs-range-defun-start nil)
(defvar ahs-range-defun-end   nil)

(defvar ahs-range-page-start nil)
(defvar ahs-range-page-end   nil)

(defcustom ahs-range-defun-modes
  '( prog-mode )
  "Major modes `beginning of defun' range can run on."
  :group 'ahs
  :type '(repeat symbol))

(defcustom ahs-range-defun-function 'ahs-range-defun
  "Function used in `beginning of defun' range."
  :group 'ahs)

(defmacro ahs-range-defun-error (err)
  `(if (= 4 (length ,err))
       (apply 'ahs-log 'error-scan-sexp ,err)
     (ahs-log 'self ,err)))

(defun ahs-range-orignal-n2d ()
  "Original narrow-to-defun."
  (save-restriction
    (condition-case err
        (progn
          (narrow-to-defun)
          (cons (point-min) (point-max)))
      (error err (ahs-range-defun-error err)))))

(defun ahs-range-defun ()
  "Another narrow-to-defun."
  (condition-case err
      (let ((opoint (point))
            beg end)
        ;; Point in function
        (beginning-of-defun)
        (setq beg (point))
        (end-of-defun)
        (setq end (point))
        (cond
         ;; Between point-min and function
         ((equal beg (point-min))
          (goto-char opoint)
          (beginning-of-defun -1)
          (if (and (>= opoint beg)
                   (<  opoint end))
              ;; From point-min to first function
              (when (> end (point))
                (setq end (point)))
            ;; Outside function
            (setq beg end
                  end (point))))
         ;; Between function and function
         ((>= opoint end)
          (setq beg end)
          (beginning-of-defun -1)
          (setq end (point))))
        (cons beg end))
    (error err nil)))

(ahs-range-regist
    defun
  `((name          . "Function Scope")
    (lighter       . "ℱ")
    (face          . ahs-range-defun-face)
    (major-mode    . ahs-range-defun-modes)
    (before-search . (lambda (symbol)
                       (save-excursion
                         (let ((pos (funcall ahs-range-defun-function)))
                           (if (not (consp pos))
                               'abort
                             (setq ahs-range-defun-start (car pos))
                             (setq ahs-range-defun-end   (cdr pos)))))))
    (start         . ahs-range-defun-start)
    (end           . ahs-range-defun-end))
  "Function Scope")

'(ahs-range-regist
     page
   '((name    . "Page Scope")
     (lighter . "ℛ")
     (face    . ahs-range-page-face)
     (before-search . (lambda (symbol)
                        (save-excursion
                          (save-restriction
                            (let ((pos
                                   (progn
                                     (narrow-to-page)
                                     (cons (point-min) (point-max)))))
                              (if (not (consp pos))
                                  'abort
                                (setq ahs-range-page-start (car pos))
                                (setq ahs-range-page-end   (cdr pos))))))))
     (start   . ahs-range-page-start)
     (end     . ahs-range-page-end))
   "Page Scope")

(ahs-range-regist
    buffer
  '((name    . "Whole Buffer")
    (lighter . "ℬ")
    (face    . ahs-range-buffer-face)
    (start   . point-min)
    (end     . point-max))
  "Whole buffer")

;;
;; (@* "Timer" )
;;
(defun ahs-start-timer ()
  "Start idle timer."
  (unless ahs-idle-timer
    (setq ahs-idle-timer
          (run-with-idle-timer ahs-idle-interval t 'ahs-idle-function))))

(defun ahs-restart-timer ()
  "Restart idle timer."
  (when (timerp ahs-idle-timer)
    (cancel-timer ahs-idle-timer)
    (setq ahs-idle-timer nil)
    (ahs-start-timer)))

(defun ahs-idle-function ()
  "Idle function. Called by `ahs-idle-timer'."
  (when (and ahs-mode
             (not ahs-highlighted))
    (let ((hl (ahs-highlight-p)))
      (when hl
        (ahs-highlight (nth 0 hl)
                       (nth 1 hl)
                       (nth 2 hl))))))

(defmacro ahs-add-overlay-face (pos face)
  `(if ahs-face-check-include-overlay
       (append (ahs-get-overlay-face ,pos)
               (if (listp ,face)
                   ,face
                 (list ,face))) ,face))

(defun ahs-highlight-p ()
  "Ruturn Non-nil if symbols can be highlighted."
  (let* ((bounds (imhs-bounds-at-point))
         (beg (car bounds))
         (end (cdr bounds))
         (face (when bounds
                 (get-text-property beg 'face)))
         (symbol (when bounds
                   (buffer-substring beg end))))
    (when (and symbol
               (not (ahs-dropdown-list-p))
               (not (ahs-face-p (ahs-add-overlay-face beg face) 'ahs-inhibit-face-list))
               (not (ahs-symbol-p ahs-exclude symbol t))
               (ahs-symbol-p ahs-include symbol))
      (list symbol beg end))))

(defun ahs-symbol-p (pred symbol &optional nodefs)
  "Return Non-nil if `SYMBOL' matches `PRED'."

  (cond
   ;; HACK for Markup Languages, eg html
   ((and (not nodefs) (member major-mode ahs-markup-modes))
    (and symbol (looking-back "</?[a-zA-Z0-9.-]*")))

   ;; Default include/no exclude
   ((null pred)
    (unless nodefs
      (let ((case-fold-search ahs-case-fold-search))
        (string-match ahs-default-symbol-regexp symbol))))

   ;; REGEXP
   ((stringp pred)
    (let ((case-fold-search ahs-case-fold-search))
      (string-match pred symbol)))

   ;; Major mode
   ((listp pred)
    (let ((new-pred (cdr (assoc major-mode pred))))
      (ahs-symbol-p new-pred symbol nodefs)))

   ;; Function predicate
   ((functionp pred)
    (funcall pred symbol))))

(defun ahs-dropdown-list-p ()
  "Return Non-nil if dropdown-list is expanded."
  (and (featurep 'dropdown-list)
       dropdown-list-overlays))

(defun ahs-face-p (face faces)
  "Return Non-nil if `FACE' in `FACES'."
  (let ((facelist (symbol-value faces)))
    (if (listp face)
        (cl-loop for x in face
                 when (memq x facelist)
                 return x)
      (memq face facelist))))

(defun ahs-get-overlay-face (pos)
  "Return list of all overlays face at `POS'."
  (cl-loop for overlay in (overlays-at pos)
           for face = (overlay-get overlay 'face)
           when face
           when (symbolp face)
           collect face))

;;
;; (@* "Highlight" )
;;
(defun ahs-prepare-highlight (symbol)
  "Prepare for highlight."
  (let ((before (ahs-current-range-prop 'before-search symbol))
        (beg (ahs-current-range-prop 'start))
        (end (ahs-current-range-prop 'end)))
    (cond ((equal before 'abort) nil)
          ((not (numberp beg)) nil)
          ((not (numberp end)) nil)
          ((> beg end) nil)
          (t (cons beg end)))))

(defun ahs-search-symbol (symbol search-range)
  "Search `SYMBOL' in `SEARCH-RANGE'."
  (save-excursion
    ;; HACK for Markup Modes, eg html
    (if (member major-mode ahs-markup-modes)
        (let* ((obounds (bounds-of-thing-at-point 'symbol))
               (nbounds (ignore-errors
                          (require 'sgml-mode)
                          (goto-char (car obounds))
                          (if (looking-back "/")
                              (progn
                                (goto-char (1+ (cdr obounds)))
                                (sgml-skip-tag-backward 1)
                                (forward-char 1))
                            (backward-char 1)
                            (sgml-skip-tag-forward 1)
                            (backward-char 1))
                          (bounds-of-thing-at-point 'symbol)))
               (tprop (text-properties-at (car obounds)))
               (face (cadr (memq 'face tprop)))
               (fontified (cadr (memq 'fontified tprop))))
          (unless (or face fontified)
            (setq ahs-need-fontify t))
          (push (list (car obounds) (cdr obounds) face fontified) ahs-search-work)
          (when nbounds
            (push (list (car nbounds) (cdr nbounds) face fontified) ahs-search-work)))
      ;; Normal search
      (let ((case-fold-search ahs-case-fold-search)
            (regexp (imhs-search-symbol-regexp symbol))
            (beg (car search-range))
            (end (cdr search-range)))
        (goto-char end)
        (while (re-search-backward regexp beg t)
          (let* ((symbol-beg (match-beginning 1))
                 (symbol-end (match-end 1))
                 (tprop (text-properties-at symbol-beg))
                 (face (cadr (memq 'face tprop)))
                 (fontified (cadr (memq 'fontified tprop))))
            (unless (or face fontified)
              (setq ahs-need-fontify t))
            (push (list symbol-beg
                        symbol-end
                        face fontified)
                  ahs-search-work)))))))

(defun ahs-fontify ()
  "Fontify symbols for strict check."
  ;;;;
  ;;
  ;; (@* "Note" )
  ;;
  ;;  If symbol has no text properties, will be called `jit-lock-fontify-now'
  ;; to strict check.
  ;;
  ;; Some old PCs performance may be degraded when:
  ;;  * Editing large file.
  ;;  * So many matched symbols exists outside the display area.
  ;;
  ;; Tested on my old pentium4 pc (bought in 2002 xD)
  ;;  So dirty `font-lock-keywords' and use `whole buffer' range.
  ;; Result:
  ;;  +---------------+-----------+----------------+----------+
  ;;  | filename      | filesize  | matched symbol | result   |
  ;;  +---------------+-----------+----------------+----------+
  ;;  | `loaddefs.el' | 1,207,715 | `autoload'     | so slow  |
  ;;  | `org.el'      |   753,991 | `if'           | slow     |
  ;;  +---------------+-----------+----------------+----------+
  ;;
  ;; If you feel slow, please use `display area' range instead of `whole buffer' range.
  ;; And use `ahs-onekey-edit' to use `whole buffer' range.
  ;;
  (cl-loop with beg = nil
           with end = nil

           for symbol in ahs-search-work
           for fontified = (or (nth 2 symbol)
                               (nth 3 symbol))

           unless (or beg fontified) do (setq beg (nth 0 symbol))
           unless fontified          do (setq end (nth 1 symbol))

           when (and beg end fontified)
           do (progn
                (ignore-errors (jit-lock-fontify-now beg end))
                (setq beg nil
                      end nil))

           finally
           do (when (and beg end)
                (ignore-errors (jit-lock-fontify-now beg end)))))

(defun ahs-light-up ()
  "Light up symbols."
  (cl-loop for symbol in ahs-search-work

           for beg  = (nth 0 symbol)
           for end  = (nth 1 symbol)
           for face = (or (nth 2 symbol)
                          (get-text-property beg 'face))
           for face = (ahs-add-overlay-face beg face)

           unless (ahs-face-p face 'ahs-inhibit-face-list)
           do (let ((overlay (make-overlay beg end nil nil t)))
                (overlay-put overlay 'ahs-symbol t)
                (overlay-put overlay 'face `,(ahs-current-range-prop 'face))
                (push overlay ahs-overlay-list))))

(defun ahs-highlight (symbol beg end)
  "Highlight"
  (setq ahs-search-work  nil
        ahs-need-fontify nil)
  (let ((search-range (ahs-prepare-highlight symbol)))
    (when (consp search-range)
      (ahs-search-symbol symbol search-range)
      (when ahs-need-fontify (ahs-fontify))
      (ahs-light-up)
      (when ahs-overlay-list
        (ahs-highlight-current-symbol beg end)
        (setq ahs-highlighted  t
              ahs-start-point  beg
              ahs-search-work  nil
              ahs-need-fontify nil)
        (add-hook 'pre-command-hook 'ahs-unhighlight nil t) t))))

(defun ahs-unhighlight (&optional force)
  "Unhighlight"
  (when (or force
            (not (memq this-command
                       ahs-unhighlight-allowed-commands)))
    (ahs-remove-all-overlay)
    (remove-hook 'pre-command-hook 'ahs-unhighlight t)))

(defun ahs-highlight-current-symbol (beg end)
  "Highlight current symbol."
  (let* ((overlay  (make-overlay beg end nil nil t)))

    (overlay-put overlay 'ahs-symbol 'current)
    (overlay-put overlay 'priority 1000)
    (unless (use-region-p)
      (overlay-put overlay 'face 'ahs-face))
    (overlay-put overlay 'help-echo '(ahs-stat-string))

    (overlay-put overlay 'modification-hooks    '(ahs-modification-hook))
    (overlay-put overlay 'insert-in-front-hooks '(ahs-modification-hook))
    (overlay-put overlay 'insert-behind-hooks   '(ahs-modification-hook))

    (setq ahs-current-overlay overlay)))

(defun ahs-remove-all-overlay ()
  "Remove all overlays."
  (delete-overlay ahs-current-overlay)
  (mapc 'delete-overlay ahs-overlay-list)
  (mapc 'ahs-open-necessary-overlay ahs-opened-overlay-list)
  (setq ahs-current-overlay     nil
        ahs-highlighted         nil
        ahs-opened-overlay-list nil
        ahs-overlay-list        nil
        ahs-start-point         nil))

;;
;; (@* "Edit mode" )
;;
(defun ahs-modification-hook (overlay after debut fin &optional length)
  "Overlay's `modification-hook' used in edit mode."
  (when ahs-edit-mode
    (if (not after)
        (setq ahs-inhibit-modification
              (memq this-command
                    ahs-inhibit-modification-commands))
      (setq ahs-start-modification t))))

(defun ahs-edit-post-command-hook-function ()
  "`post-command-hook' used in edit mode."
  (cond
   ;; Exit edit mode
   ((not (ahs-inside-overlay-p ahs-current-overlay))
    (ahs-edit-mode -1))

   ;; Modify!!
   ((and ahs-start-modification
         (not ahs-inhibit-modification))
    (ahs-symbol-modification)))

  (setq ahs-start-modification   nil
        ahs-inhibit-modification nil))

(defun ahs-symbol-modification ()
  "Modify all highlighted symbols."
  (let ((source (buffer-substring-no-properties
                 (overlay-start ahs-current-overlay)
                 (overlay-end ahs-current-overlay))))
    (dolist (change ahs-overlay-list)
      (when (overlayp change)
        (let* ((beg (overlay-start change))
               (end (overlay-end change))
               (len (- end beg))
               (target (buffer-substring-no-properties beg end)))
          (unless (string= source target)
            (save-excursion
              (goto-char beg)
              (insert source)
              (delete-region (point) (+ len (point))))))))))

(defun ahs-edit-mode-on ()
  "Turn `ON' edit mode."
  (setq ahs-start-modification   nil
        ahs-inhibit-modification nil)
  (overlay-put ahs-current-overlay 'face 'ahs-edit-mode-face)
  (remove-hook 'pre-command-hook 'ahs-unhighlight t)
  (add-hook 'post-command-hook 'ahs-edit-post-command-hook-function nil t)
  (run-hooks 'ahs-edit-mode-on-hook)

  ;; Exit edit mode when undo over edit mode.
  (push '(apply ahs-clear t) buffer-undo-list)

  ;; Display log
  (unless ahs-suppress-log
    (let* ((st (ahs-stat))
           (alert
            (if (ahs-stat-alert-p st)
                (format (ahs-log-format 'exist-elsewhere)
                        (ahs-decorate-if
                         (number-to-string
                          (+ (nth 2 st)
                             (nth 3 st)))
                         font-lock-warning-face))
              "")))
      (ahs-log 'turn-on-edit-mode alert))))

(defun ahs-edit-mode-off (nomsg interactive)
  "Turn `OFF' edit mode."
  (if (and interactive
           (ahs-inside-overlay-p ahs-current-overlay))
      (progn
        (overlay-put ahs-current-overlay 'face `,(ahs-current-range-prop 'face))
        (add-hook 'pre-command-hook 'ahs-unhighlight nil t))
    (ahs-remove-all-overlay))
  (remove-hook 'post-command-hook 'ahs-edit-post-command-hook-function t)
  (run-hooks 'ahs-edit-mode-off-hook)

  ;; Display log
  (let ((ahs-suppress-log
         (or nomsg ahs-suppress-log)))
    (ahs-log 'turn-off-edit-mode))

  (force-mode-line-update))

(defun ahs-edit-mode-condition-p ()
  "Return Non-nil if edit mode can turn on."
  (cond
   ((not ahs-mode)
    (ahs-log 'error-ahs-disable))
   (buffer-read-only
    (ahs-log 'error-read-only (buffer-name)))
   (t t)))

(defun ahs-edit-cancel-current ()
  (interactive)
  (when ahs-edit-mode
    (if (< (length ahs-overlay-list) 2)
        (ahs-edit-mode -1)
      (let ((op (point)))
        (condition-case nil
            (ahs-forward)
          (error
           (ignore-errors (ahs-backward))))
        (mapc 'delete-overlay (overlays-at op))
        (setq ahs-overlay-list
              (seq-remove (lambda (ov)
                            (message "[[%s]]" (overlay-buffer ov))
                            (null (overlay-buffer ov)))
                          ahs-overlay-list))
        (if (< (length ahs-overlay-list) 2)
            (ahs-edit-mode -1)
          (force-mode-line-update))))))

;;
;; (@* "Select" )
;;
(defun ahs-select (pred &optional reverse onlydef)
  "Select highlighted symbol."
  (when ahs-highlighted
    (let* ((next (cl-loop with start = nil
                          for overlay in (if reverse
                                             (reverse ahs-overlay-list)
                                           ahs-overlay-list)

                          for skip = (cl-loop for hidden in (overlays-at (overlay-start overlay))
                                              when (overlay-get hidden 'invisible)
                                              when (or (equal ahs-select-invisible 'skip)
                                                       (not (overlay-get hidden 'isearch-open-invisible)))
                                              return hidden)

                          for selectable = (and (not skip)
                                                (or (not onlydef)
                                                    (ahs-definition-p overlay)))

                          when selectable
                          unless start do (setq start overlay)

                          when selectable
                          when (funcall pred overlay) return overlay

                          finally
                          return (or start
                                     ahs-current-overlay)))

           (beg (overlay-start next))
           (end (overlay-end next)))

      (dolist (overlay
               (unless (equal ahs-select-invisible 'skip)
                 (ahs-get-openable-overlays next)))
        (ahs-open-invisible-overlay-temporary overlay))

      (goto-char (+ beg (- (point) (overlay-start ahs-current-overlay))))
      (move-overlay ahs-current-overlay beg end))

    (when (equal ahs-select-invisible 'immediate)
      (ahs-close-unnecessary-overlays))))

(defun ahs-get-openable-overlays (overlay)
  "Return list of openable overlays."
  (cl-loop for openable in (overlays-at (overlay-start overlay))
           when (overlay-get openable 'invisible)
           when (overlay-get openable 'isearch-open-invisible)
           collect openable))

;; Modified from isearch.el
(defun ahs-close-unnecessary-overlays ()
  "Close unnecessary overlays."
  (let ((overlays ahs-opened-overlay-list)
        (newlist))
    (setq ahs-opened-overlay-list nil)
    (dolist (overlay overlays)
      (if (ahs-inside-overlay-p overlay)
          (push overlay newlist)
        (let ((func-temp (overlay-get overlay 'isearch-open-invisible-temporary)))
          (if func-temp
              (funcall func-temp overlay t)
            (ahs-store-property  overlay 'isearch-invisible  'invisible)
            (ahs-store-property  overlay 'isearch-intangible 'intangible)))))
    (setq ahs-opened-overlay-list newlist)))

(defun ahs-open-necessary-overlay (overlay)
  "Open the `OVERLAY' if it is necessary. Otherwise close."
  (when (overlayp overlay)
    (let ((inside-overlay (ahs-inside-overlay-p overlay))
          (func-temp (overlay-get overlay 'isearch-open-invisible-temporary))
          (func      (overlay-get overlay 'isearch-open-invisible)))
      (when (or inside-overlay (not func-temp))
        (ahs-store-property overlay 'isearch-invisible  'invisible)
        (ahs-store-property overlay 'isearch-intangible 'intangible))
      (if (or inside-overlay
              (equal ahs-select-invisible 'open))
          (when func (funcall func overlay))
        (when func-temp (funcall func-temp overlay t))))))

(defun ahs-open-invisible-overlay-temporary (overlay)
  "Open the `OVERLAY' temporary."
  (let ((func (overlay-get overlay 'isearch-open-invisible-temporary)))
    (if func
        (funcall func overlay nil)
      (ahs-store-property overlay 'invisible  'isearch-invisible)
      (ahs-store-property overlay 'intangible 'isearch-intangible)) ;; intangible need?
    (push overlay ahs-opened-overlay-list)))

(defun ahs-store-property (overlay from to)
  "Store `OVERLAY' property."
  (overlay-put overlay to (overlay-get overlay from))
  (overlay-put overlay from nil))

;; No doc xD
(defun ahs-forward-p        (x) (< (overlay-start ahs-current-overlay) (overlay-start x)))
(defun ahs-backward-p       (x) (> (overlay-start ahs-current-overlay) (overlay-start x)))
(defun ahs-definition-p     (x) (seq-contains ahs-definition-face-list (get-char-property (overlay-start x) 'face)))
(defun ahs-start-point-p    (x) (equal (overlay-start x) ahs-start-point))
(defun ahs-inside-overlay-p (x) (and (>= (point) (overlay-start x)) (<= (point) (overlay-end x))))
(defun ahs-inside-display-p (x) (and (>= (window-end) (overlay-start x)) (<= (window-start) (overlay-start x))))
(defun ahs-hidden-p         (x) (cl-loop for overlay in (overlays-at (overlay-start x))
                                         when (overlay-get overlay 'invisible)
                                         return t))

;;
;; (@* "Misc" )
;;
(defun ahs-stat ()
  "Return list of the current status."
  (append (list (ahs-decorated-current-range-name)
                (length ahs-overlay-list))

          (cl-loop with hidden?   = 0
                   with before    = 0
                   with after     = 0
                   with displayed = 0

                   for x in ahs-overlay-list

                   count (funcall 'ahs-backward-p x) into before
                   count (funcall 'ahs-forward-p x)  into after

                   count (and (funcall 'ahs-inside-display-p x)
                              (cl-incf hidden?)
                              (not (funcall 'ahs-hidden-p x)))
                   into displayed

                   finally
                   return (list before after displayed (- hidden? displayed)))))

(defun ahs-stat-alert-p (status)
  "Return Non-nil if many symbols are highlighted but displayed one or zero."
  (and (< (nth 4 status) 2)
       (or (> (nth 2 status) 0)
           (> (nth 3 status) 0)
           (> (nth 5 status) 0))))

(defmacro ahs-decorate-number (number)
  `(unless (equal ,number 0)
     (setq ,number
           (ahs-decorate-if
            (number-to-string ,number) font-lock-warning-face))))

(defun ahs-stat-string (&optional status)
  "Return the formatted `STATUS'. `STATUS' defaults to the current status."
  (let* ((st (or status
                 (ahs-stat)))
         (before (nth 2 st))
         (after  (nth 3 st))
         (hidden (nth 5 st)))

    (when (ahs-stat-alert-p st)
      (ahs-decorate-number before)
      (ahs-decorate-number after)
      (ahs-decorate-number hidden))

    (format (ahs-log-format 'stat)
            (nth 0 st)
            (nth 1 st)
            (nth 4 st)
            hidden before after)))

(defun ahs-lighter ()
  "Set mode line lighter."
  (if ahs-mode
      (propertize
       (concat ""
               (when ahs-edit-mode (car ahs-edit-mode-lighter-pair))
               (if ahs-edit-mode
                   (format "%d" (length ahs-overlay-list))
                 (ahs-current-range-prop 'lighter))
               (when ahs-edit-mode (cdr ahs-edit-mode-lighter-pair))
               "  ")
       'face
       `(:foreground
         ,(face-attribute `,(ahs-current-range-prop 'face) :underline)))
    ""))

(defun ahs-init ()
  "Initialize"
  (unless ahs-current-range
    (ahs-change-range (ahs-most-available-range) t))
  (force-mode-line-update)
  (ahs-start-timer))

(defun ahs-clear (&optional verbose)
  "Remove all overlays and exit edit mode."
  (if ahs-edit-mode
      (ahs-edit-mode -1)
    (when ahs-highlighted
      (ahs-unhighlight t))))

(defun ahs-mode-maybe ()
  "Fire up `ahs-mode' if major-mode in ahs-modes."
  (if (and (not (minibufferp (current-buffer)))
           (or (derived-mode-p 'prog-mode)
               (member major-mode ahs-extra-modes)))
      (ahs-mode t)))

(defun ahs-init-lighter ()
  (let ((lighter '(:eval (ahs-lighter))))
    (unless (cl-find lighter mode-line-format :test 'equal)
      (push lighter
            (nthcdr
             (cl-position 'mode-line-misc-info mode-line-format)
             mode-line-format)))))

;;
;; (@* "Interactive" )
;;
(defun ahs-forward ()
  "Select highlighted symbols forwardly."
  (interactive)
  (deactivate-mark)
  (ahs-select 'ahs-forward-p t))

(defun ahs-backward ()
  "Select highlighted symbols backwardly."
  (interactive)
  (deactivate-mark)
  (ahs-select 'ahs-backward-p))

(defun ahs-forward-definition ()
  "Select highlighted symbols forwardly. only symbol definition."
  (interactive)
  (ahs-select 'ahs-forward-p t t))

(defun ahs-backward-definition ()
  "Select highlighted symbols backwardly. only symbol definition."
  (interactive)
  (ahs-select 'ahs-backward-p nil t))

(defun ahs-back-to-start ()
  "Go back to the starting point.

Limitation:
  If you change range during highlights, starting point will be reset."
  (interactive)
  (ahs-select 'ahs-start-point-p))

(defun ahs-change-range (&optional range nomsg)
  "Current range change to `RANGE' range. `RANGE' defaults to next runnable range."
  (interactive)
  (ahs-clear (not nomsg))

  (unless (and range (ahs-valid-range-p range))
    (setq range (ahs-runnable-ranges t)))
  (ahs-change-range-internal range)
  (let ((ahs-suppress-log nomsg))
    (ahs-log 'range-changed (ahs-decorated-current-range-name)))

  (when (called-interactively-p 'interactive)
    (ahs-idle-function))
  (force-mode-line-update))

(defun ahs-set-idle-interval (secs)
  "Set wait until highlighting symbol when emacs is idle."
  (interactive "nSeconds to idle, before highlighting symbol: ")
  (when (and (numberp secs)
             (not (equal secs 0)))
    (setq ahs-idle-interval secs)
    (ahs-restart-timer)))

(defun ahs-display-stat ()
  "Display current status.

Display current range name, number of matched symbols and the details.

The details are as follows:
  1. Displayed symbols
  2. Hidden symbols inside the display area
  3. Symbols before the cursor
  4. Symbols after the cursor

That's all."

  (interactive)
  (let ((ahs-suppress-log
         (and (not (called-interactively-p 'interactive))
              ahs-suppress-log)))
    (ahs-log 'self (ahs-stat-string))))

;;
;; (@* "Define mode" )
;;
(defvar ahs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<left>"    ) 'ahs-backward            )
    (define-key map (kbd "M-<right>"   ) 'ahs-forward             )
    (define-key map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
    (define-key map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
    (define-key map (kbd "M--"         ) 'ahs-back-to-start       )
    (define-key map (kbd "C-x M-r"     ) 'ahs-change-range        )
    (define-key map (kbd "C-x C-a"     ) 'ahs-edit-mode           )
    map))

(defvar ahs-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-u") 'ahs-edit-cancel-current)
    map))

;;;###autoload
(define-globalized-minor-mode global-ahs-mode
  ahs-mode ahs-mode-maybe
  :group 'ahs)

;;;###autoload
(define-minor-mode ahs-mode
  "Toggle Auto Highlight Symbol Mode"
  :group 'ahs
  (if ahs-mode
      (ahs-init))
  (ahs-clear))

;;;###autoload
(define-minor-mode ahs-edit-mode
  "Go to edit mode."
  :keymap ahs-edit-mode-map
  :group 'ahs-mode
  (if ahs-edit-mode (ahs-idle-function))
  (cond ((not (ahs-edit-mode-condition-p)) nil)
        ((not ahs-highlighted) (ahs-log 'no-symbol-at-point))
        ((not ahs-edit-mode)
         (ahs-edit-mode-off nil (called-interactively-p 'interactive)))
        (ahs-edit-mode (ahs-edit-mode-on))))

;;; lighter
(ahs-init-lighter)

;;
;; (@* "Revert" )
;;
;; Remove all overlays and exit edit mode before revert-buffer
(add-hook 'before-revert-hook 'ahs-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ahs)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ahs.el ends here
