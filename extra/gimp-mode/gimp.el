;;; gimp.el --- For writing GIMP Fu-Script. -*- lexical-binding: t -*-


(require 'company)
(require 'eldoc)
(require 'cmuscheme)


;;; variables

(defvar gimp-prompt "GIMP> ")
(defvar gimp-buffer "*gimp*")
(defvar gimp-temp-buffer " *gimp-temp*")
(defvar gimp-process nil)


;;; input normalize

(defun gimp-normalize-input-string (string)
  (let* ((len (length (encode-coding-string string 'utf-8))))
    (if (> len (* 256 256)) (error "[Send-String] too long: %d" len)
      (let ((high (/ len 256)) (low (mod len 256)))
        (concat (format "G%c%c" high low)
                (encode-coding-string string 'utf-8))))))

(defun gimp-input-sender (proc string)
  (comint-send-string proc (gimp-normalize-input-string string)))


;;; output filters

(defun gimp-normalize-output-header (head-string)
  (if (and (char-equal (aref head-string 0) ?G)
           (>= (length head-string) 4)
           (not (string-prefix-p gimp-prompt head-string)))
      (let* ((ns (encode-coding-string head-string 'utf-8))
             (err? (not (eq 0 (aref ns 1))))
             (high (aref ns 2)) (low (aref ns 3))
             (total (+ low (* 256 high)))
             (remain (subseq ns 4))
             (remain-len (length remain))
             (remain-mb (decode-coding-string remain 'utf-8)))
        (list total err? remain-mb remain-len (= remain-len total)))
    (error "Perhaps wrong format HEAD.")))

(defun gimp-filter-to-comint (string)
  (with-current-buffer (get-buffer-create gimp-temp-buffer)
    (goto-char (point-max))
    (insert string)
    (if (> (point-max) 4)
        (condition-case err
            (let ((nl (gimp-normalize-output-header (buffer-string))))
              (if (nth 4 nl)
                  (prog1
                      (format "%s\n%s" (nth 2 nl) gimp-prompt)
                    (erase-buffer))
                ""))
          (error (prog1
                     (format "%s\n%s" (buffer-string) gimp-prompt)
                   (erase-buffer))))
      (if (char-equal (char-after 1) ?G)
          ""
        (erase-buffer)
        string))))

(defun gimp-filter-to-string (proc string)
  (with-current-buffer (get-buffer-create gimp-temp-buffer)
    (goto-char (point-max))
    (insert string)
    (if (> (point-max) 4)
        (let* ((nl (gimp-normalize-output-header (buffer-string))))
          (when (nth 4 nl) ; finish
            (funcall (process-get proc 'callback) (nth 2 nl))
            (erase-buffer))))))


;;; helps

(defun gimp-proc ()
  "Return current gimp process."
  (if (and gimp-process (process-live-p gimp-process))
      gimp-process
    (gimp-connect)))

(defun gimp-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))

(defun clean-temp-buffer ()
  (ignore-errors (kill-buffer gimp-temp-buffer)))


;;; commands

(defun gimp-send-input ()
  (interactive)
  (clean-temp-buffer)
  (set-process-filter (gimp-proc) 'comint-output-filter)
  (let ((ori-point (point))
        (pmark (process-mark (gimp-proc))))
    (if (gimp-input-complete-p pmark (point-max))
        (progn
          (goto-char (point-max))
          (comint-send-input))
      (newline-and-indent))))

(defun gimp-send-to-string (string callback)
  (interactive)
  (clean-temp-buffer)
  (process-put (gimp-proc) 'callback callback)
  (set-process-filter (gimp-proc) 'gimp-filter-to-string)
  (send-string (gimp-proc) (gimp-normalize-input-string string)))

(defun gimp-send-region (start end)
  "Send the current region to the Gimp process."
  (interactive "r")
  (clean-temp-buffer)
  (set-process-filter (gimp-proc) 'comint-output-filter)
  (with-current-buffer gimp-buffer (comint-snapshot-last-prompt))
  (comint-output-filter (gimp-proc) "\n")
  (gimp-input-sender (gimp-proc) (buffer-substring start end)))

(defun gimp-send-definition ()
  "Send the current definition to the Gimp process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (gimp-send-region (point) end))))

(defun gimp-send-last-sexp ()
  "Send the previous sexp to the Gimp process."
  (interactive)
  (gimp-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun gimp-load-file (file-name)
  "Load a Scheme file FILE-NAME into the Gimp process."
  (interactive (comint-get-source "Load Script-Fu file: " scheme-prev-l/c-dir/file scheme-source-modes t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (setq gimp-newline-p t)
  (comint-send-string (gimp-proc)
                      (gimp-normalize-input-string
                       (concat "(load \"" file-name "\")"))))


;;; mode and map

(defvar gimp-mode-map
  (let ((m (copy-keymap scheme-mode-map)))
    (define-key m "\C-x\C-e" 'gimp-send-last-sexp)
    (define-key m "\C-c\C-r" 'gimp-send-region)
    (define-key m "\C-c\C-c" 'gimp-send-definition)
    (define-key m "\C-c\C-l" 'gimp-load-file)
    (define-key m "\C-ci" 'connect-gimp) m))

(defvar inferior-gimp-mode-map
  (let ((m (copy-keymap inferior-scheme-mode-map)))
    (define-key m "\C-m" 'gimp-send-input)
    (define-key m "\C-a" 'comint-bol-or-process-mark)
    (define-key m "\C-c\C-c" 'comint-send-input)
    m))

;;;###autoload
(define-derived-mode gimp-mode scheme-mode "GIMP MODE"
  "Mode for editing script-fu of GIMP."
  (use-local-map gimp-mode-map)
  (company-mode-on)
  (setq-local eldoc-documentation-function 'gimp-eldoc-documentation-function))

(define-derived-mode inferior-gimp-mode inferior-scheme-mode
  "Inferior GIMP"
  "Mode for interaction with inferior gimp process."
  (use-local-map inferior-gimp-mode-map)
  (setq comint-prompt-read-only t)
  (setq comint-prompt-regexp (concat "^" gimp-prompt))
  (setq comint-input-sender 'gimp-input-sender)
  (setq comint-preoutput-filter-functions '(gimp-filter-to-comint))
  (setq-local eldoc-documentation-function 'gimp-eldoc-documentation-function))

(defun gimp-initial-script ()
  (let ((eldoc-define
         "(define (im-gimp-eldoc-info name)
(let* ((sl (gimp-procedural-db-proc-info name)) (desc (nth 0 sl)) (arg-num (nth 6 sl)) (args '()))
  (let loop ((n (- arg-num 1)))
    (if (>= n 0) (begin (set! args (cons (cadr (gimp-procedural-db-proc-arg name n)) args)) (loop (- n 1)))))
  (set! args (cons name args))
  (list desc args)))"))
    (gimp-send-to-string eldoc-define (lambda (x) x))))

;;;###autoload
(defun connect-gimp (host port)
  (interactive (if current-prefix-arg
                   (list (read-from-minibuffer "Host: " "127.0.0.1")
                         (read-number "Port: " 10008))
                 (list "127.0.0.1" 10008)))
  (clean-temp-buffer)
  (when (not (comint-check-proc gimp-buffer))
    (set-buffer (apply 'make-comint "gimp" '("127.0.0.1" . 10008) nil nil)))

  (setq gimp-process (get-process "gimp"))
  (set-process-coding-system gimp-process nil 'iso-latin-1)
  (set-process-query-on-exit-flag gimp-process nil)

  (pop-to-buffer gimp-buffer '((display-buffer-reuse-window
                                display-buffer-at-bottom)
                               (window-height . 0.3)))
  (inferior-gimp-mode)
  (company-mode-on)

  (gimp-complete-candidates)
  (comint-output-filter gimp-process "Welcome to GImp-Fu Script World! (TinyScheme)")
  (sit-for 0.1)
  (gimp-initial-script))


;;; company support

(defvar gimp-complete-candidates nil)

(defun gimp-complete-candidates ()
  (gimp-send-to-string "(gimp-procedural-db-query \"\" \"\" \"\" \"\" \"\" \"\" \"\")"
                       (lambda (x)
                         (setq gimp-complete-candidates (cadar (read-from-string x))))))

(defun company-gimp-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-gimp-backend))
    (prefix (and (find major-mode '(gimp-mode inferior-gimp-mode))
                 (company-grab-symbol)))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c)) gimp-complete-candidates))))

(add-to-list 'company-backends 'company-gimp-backend)


;;; eldoc support

(defvar gimp-eldoc-caches nil)

(defun gimp-eldoc-get (name)
  (when (and name (> (length name) 8))
    (let* ((ret (assoc name gimp-eldoc-caches)))
      (if ret
          (nth 1 ret)
        (gimp-send-to-string
         (format "(im-gimp-eldoc-info \"%s\")" name)
         (lambda (c)
           (let ((ret (read-from-string c)))
             (when (and (car ret) (listp (car ret)))
               (push (list name (format "(%s)" (string-join (cadar ret) " ")))
                     gimp-eldoc-caches)))))))))

(defun gimp-eldoc-documentation-function ()
  (let* ((cand (elt company-candidates company-selection))
         (sym (cond (cand cand) ; company candidates
                    ((eql last-command-event 32) ; after SPC
                     (save-excursion (backward-char) (thing-at-point 'symbol)))
                    (t (thing-at-point 'symbol)))))
    (gimp-eldoc-get sym)))


(provide 'gimp)
