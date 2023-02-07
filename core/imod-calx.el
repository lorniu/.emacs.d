;;; -*- lexical-binding: t -*-

;;; Code:

(xzz calendar
  :config
  (require 'cal-china-x nil t)
  (setq calendar-week-start-day 1
        mark-holidays-in-calendar t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays)))

(defun im:cal-chinese-to-absolute (y m d)
  "将农历转换为绝对日期."
  (require 'cal-china)
  (let* ((m (if (floatp m) (+ 0.5 (truncate m)) m))
         (d1 (ignore-errors (+ (cadr (assoc m (calendar-chinese-year y))) d -1)))
         (d2 (ignore-errors (+ (cadr (assoc m (calendar-chinese-year (1+ y)))) d -1)))
         (dv (calendar-absolute-from-gregorian (list (truncate m) d y))))
    (or (cl-find-if (lambda (d) (and d (< dv d))) (list d1 d2))
        (user-error "Maybe the date you provide is invalid"))))



(xzz calc
  :init (defvar calc-date-format-history)
  :config
  (setopt calc-multiplication-has-precedence nil ; division and multiplication have same precedence
          calc-date-format '(YYYY "-" MM "-" DD " " Www (" " hh ":" mm ":" ss) CN)
          calc-date-format-history `(,calc-date-format
                                     (YYYY "-" MM "-" DD " " Www (" " hh ":" mm ":" ss))
                                     ((H ":" mm C SS pp " ") Www " " Mmm " " D ", " YYYY)))
  (with-eval-after-load 'calc-units
    (setopt math-additional-units
            '((b   nil        "bit")
              (B   "8 * b"    "byte")
              (KiB "2^10 * B" "Kibibyte")
              (MiB "2^20 * B" "Mebibyte")
              (GiB "2^30 * B" "Gibibyte")
              (TiB "2^40 * B" "Tebibyte")
              (PiB "2^50 * B" "Pebibyte")
              (EiB "2^60 * B" "Exbibyte"))
            math-units-table nil))
  (with-eval-after-load 'calc-ext
    (define-key calc-mode-map "i" #'calc-inverse) ; Inverse instead of Info
    (define-key calc-mode-map "h" #'calc-hyperbolic) ; Hyperbolic instead of help
    (define-key calc-mode-map "\M-w" #'calc-copy-region)))

(defun:override calc-date-notation ()
  "Change display format for <date> via `completing-read'."
  (interactive)
  (calc-wrapper
   (let* ((df (completing-read
               "Date format: "
               (mapcar #'prin1-to-string (cons calc-date-format calc-date-format-history))
               nil nil nil t (prin1-to-string calc-date-format)))
          (dl (car (read-from-string df))))
     (add-to-history 'calc-date-format-history dl)
     (calc-change-mode 'calc-date-format dl t))))

(defun calc-calendar ()
  "Open Calendar for current date in stack top."
  (interactive)
  (let ((date (calc-top)))
    (unless (eq (car-safe date) 'date)
      (user-error "Not a date, can not open calendar"))
    (cl-letf (((symbol-function 'calendar-current-date)
               (lambda ()
                 (let ((dt (math-date-to-dt date)))
                   (list (cadr dt) (caddr dt) (car dt))))))
      (calendar))))

(defun:around math-format-date-part//add-CN-part (fn x)
  "为 <date> 增加农历显示."
  (require 'cal-china-x)
  (if (and math-fd-date (eq x 'CN))
      (let* ((dt (math-date-to-dt math-fd-date))
             (ds (list (nth 1 dt) (nth 2 dt) (car dt)))
             (dc (calendar-chinese-from-absolute (calendar-absolute-from-gregorian ds))))
        (propertize
         (format " %s·%s%s%s·%s"
                 (calendar-chinese-sexagesimal-name (cadr dc))
                 (if (integerp (cl-caddr dc)) "" "闰")
                 (aref cal-china-x-month-name (1-  (floor (cl-caddr dc))))
                 (aref cal-china-x-day-name (1- (cl-cadddr dc)))
                 (cal-china-x-get-horoscope (car ds) (cadr ds)))
         'font-lock-face 'font-lock-comment-face))
    (funcall fn x)))

(defun calc-chinese-date ()
  "输入农历日期."
  (interactive)
  (require 'calc-forms)
  (require 'cal-china-x)
  (calc-wrapper
   (cl-letf (((symbol-function #'math-parse-date-validate)
              (lambda (y big m d &rest _r)
                (and (not big) (natnump y) (< y 100) (setq y (+ y (if (< y 40) 2000 1900))))
                (let* ((ab (im:cal-chinese-to-absolute y (if big m (+ 0.0 m)) d))
                       (gr (calendar-gregorian-from-absolute ab)))
                  (list 'date (math-dt-to-date (list (caddr gr) (car gr) (cadr gr))))))))
     (calc-push (math-parse-date (read-string "农历日期 (YYYY/MM/DD, YY for leap month): " nil 'calc-chinese-date-history))))))

(defun calc-copy-region ()
  "Do not copy the line number when region actived."
  (interactive nil calc-mode)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (call-interactively #'calc-copy-region-as-kill)))

;; Take region content as initial input.

(defun:around calc//with-grab-region (f &rest args)
  (interactive "P\ni\np")
  (let (init)
    (when (use-region-p)
      (let ((text (im:thing-at-region-or-point)))
        (unless (string-blank-p text) (setq init text))))
    (apply f (car args) (cadr args))
    (when (and init (derived-mode-p 'calc-mode))
      (calc-wrapper
       (let ((math-expr-opers (math-expr-ops)))
         (calc-alg-entry init))))))

(defvar quick-calc-init-string nil)

(defun:around calc-do-quick-calc//quick-calc-with-region (f &rest args)
  (let ((quick-calc-init-string (im:thing-at-region-or-point "")))
    (apply f args)))

(defun:around calc-do-alg-entry//quick-calc-with-region (f &rest args)
  (when (and (string= (car args) "")
             (string= (cadr args) "Quick calc: "))
    (setf (car args) quick-calc-init-string))
  (apply f args))


;;; Transient UI for Calc

(defun im:get-calc-keymap-bindings (prefix &optional filter)
  "Return commands binds begin with PREFIX in `calc-mode-map'."
  (let ((keys (if prefix (keymap-lookup calc-mode-map prefix) calc-mode-map)) cmds)
    (map-keymap (lambda (key def)
                  (setq key (key-description (vconcat prefix (list key))))
                  (setq def (if (symbolp def) def))
                  (when (and filter (not (funcall filter key (symbol-name def))))
                    (setq def nil))
                  (if def (push def cmds)))
                keys)
    cmds))

(transient-define-prefix im/assist-calc-mode ()
  :transient-non-suffix 'transient--do-exit
  [[("SPC" "Top.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Top: "
         `( "calc.*-quick"
            "calc.*-truncate-"
            calc-insert-register calc-copy-to-register
            calc-clean-num calc-select-part
            ,@(im:get-calc-keymap-bindings nil)))))
    (" h " "Help.." calc-full-help)
    (" m " "Modes.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Modes: "
         (im:get-calc-keymap-bindings "m") "depth$")))
    (" T " "Trail.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Trail: " "^calc-trail-" "mode$")))]
   [("d r" "Disp Radix.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Radix: " "^calc.*-radix$")))
    ("d n" "Disp Notation.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Notation: " "^calc.*-notation$")))
    ("d l" "Disp Language.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Language: " "^calc.*-language$")))
    ("d d" "Disp Other.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Display: "
         (im:get-calc-keymap-bindings "d" (lambda (_ d) (not (string-match-p "-radix$\\|-notation$\\|-language$\\|-redo$" d)))))))]
   [("f" "Functions.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Functions: "
         `( calc-abs calc-idiv calc-floor calc-trunc calc-ceiling
            calc-sqrt calc-exp calc-ln calc-ln calc-log10 calc-log
            ,@(im:get-calc-keymap-bindings "f")))))
    ("r" "Trigonometric.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Trigonometric: "
         '(calc-pi
           calc-sin calc-arcsin calc-sinh calc-arcsinh calc-sec calc-sech
           calc-cos calc-arccos calc-cosh calc-arccosh calc-csc calc-csch
           calc-tan calc-arctan calc-tanh calc-arctanh calc-cot calc-coth
           calc-conj calc-imaginary calc-arctan2 calc-sincos
           calc-to-degrees calc-to-radians calc-degrees-mode calc-radians-mode))))
    ("p" "Programmability.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Programmability: "
         '( calc-equal-to calc-remove-equal calc-not-equal-to
            calc-less-than calc-greater-than calc-less-equal calc-greater-equal
            calc-in-set "^calc-logical-"))))
    ("k" "Combinatorics/Statistics.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Combinatorics/Statistics: "
         `(calc-factorial ,@(im:get-calc-keymap-bindings "k"))
         "keep-args$")))]
   [("b" "2#101/business.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "binary/business: "
         `(calc-percent calc-convert-percent ,@(im:get-calc-keymap-bindings "b")))))
    ("t" "<1970/1/1>.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Time: "
         `("hms" calc-time calc-calendar calc-chinese-date calc-date-notation ,@(im:get-calc-keymap-bindings "t"))
         "-trail\\|-tan\\|-quick")))
    ("v" "[vector/matrix].."
     (lambda ()
       (interactive)
       (im:execute-command-matching "vector/matrix: "
         `(calc-conj calc-concat ,@(im:get-calc-keymap-bindings "v")))))
    ("u" "[Statistics].."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Statistics: "
         (im:get-calc-keymap-bindings "u" (lambda (_ y) (string-match-p "vector" (format "%s" y)))))))]
   [("a" "Algebra.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Algebra: "
         (im:get-calc-keymap-bindings "a")
         '("-abs$"
           calc-equal-to calc-remove-equal calc-not-equal-to
           calc-less-than calc-greater-than calc-less-equal calc-greater-equal
           calc-in-set))))
    ("c" "Convert.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Convert: "
         `(calc-floor calc-round ,@(im:get-calc-keymap-bindings "c")) "-cos$")))
    ("U" "Units.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Units: "
         (im:get-calc-keymap-bindings "u") "-vector\\|-undo")))
    ("l" "Log Units.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Log Units: "
         (im:get-calc-keymap-bindings "l"))))]
   [("s" "Store/Recall.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Store/Recall: "
         (im:get-calc-keymap-bindings "s"))))
    ("j" "Select.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Select: "
         (im:get-calc-keymap-bindings "j") "conj$")))
    ("g" "Graphics.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Graphics: "
         (im:get-calc-keymap-bindings "g") "argument$")))
    ("z" "Define/User.."
     (lambda ()
       (interactive)
       (im:execute-command-matching "Define/User: " (im:get-calc-keymap-bindings "Z"))))]]
  (interactive)
  (if (eq major-mode 'calc-mode)
      (transient-setup 'im/assist-calc-mode)
    (user-error "You should invoke this in calc-mode")))
