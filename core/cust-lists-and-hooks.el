;;; cust-lists-and-hooks.el --- Lists and Hooks -*- lexical-binding: t -*-

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
         (window-height . fit-window-to-buffer))

        ("." nil (reusable-frames . t))))

(setq auto-mode-alist
      (append '(("\\.class\\'"           . class-mode)
                ("\\.scm\\'"             . scheme-mode)
                ("\\.\\(ba\\)?sh\\'"     . sh-mode)
                ("\\.xaml\\'"            . nxml-mode)
                ("\\.\\(ini\\|inf\\)\\'" . conf-mode))
              auto-mode-alist))

(setq jka-compr-compression-info-list
      (prog1 ;; for .rar, you should install `unarchiver'
          (append `(["\\.plist$"
                     "converting text XML to binary plist" "plutil" ("-convert" "binary1" "-o" "-" "-")
                     "converting binary plist to text XML" "plutil" ("-convert" "xml1" "-o" "-" "-")
                     nil nil "bplist"])
                  jka-compr-compression-info-list)
        (jka-compr-update)))


;;; Display Functions

(defmacro im/make-fun--display-my-buffer-in-direction-or- (other)
  (let ((fname (intern (format "%%display-buffer-in-direction-or-%s" other)))
        (dname (intern (format "display-buffer-%s" other))))
    `(defun ,fname (buffer alist)
       (if (and (> (frame-width) 120)
                (null (window-in-direction 'right))
                (null (window-in-direction 'left)))
           (display-buffer-in-direction buffer alist)
         (,dname buffer alist)))))

(im/make-fun--display-my-buffer-in-direction-or- below-selected)
(im/make-fun--display-my-buffer-in-direction-or- at-bottom)

(defun %display-buffer-at-bottom-follows-with-quit (buffer alist)
  (display-buffer-at-bottom buffer alist)
  (select-window (get-buffer-window buffer))
  (with-current-buffer buffer (view-mode 1)))


;;; Global Hooks

(defun:hook find-file-hook/readonly ()
  "Files that should be readonly."
  (let* ((case-fold-search nil)
         (match? (lambda (&rest items) (string-match-p (apply #'join-as-regor-group items) buffer-file-name))))
    (when (and buffer-file-name
               (or (and ic/find-file-readonly-regexp
                        (string-match-p ic/find-file-readonly-regexp buffer-file-name))
                   (and (not
                         (funcall match? ; exclude
                                  "/usr/home"
                                  "\\.cache/" "tmp/" "vvv/" "notes"
                                  "autoloads.el$" "loaddefs.el$"))
                        (funcall match? ; include
                                 "^/usr/"
                                 ".emacs.d/packages"
                                 ".roswell/lisp/quicklisp"))))
      (view-mode 1)
      (make-local-variable 'view-mode-map)
      (define-key view-mode-map "q" 'View-kill-and-leave))))

(defun:hook before-save-hook/cleanup ()
  (unless (cl-find major-mode '(org-mode))
    (delete-trailing-whitespace)))

(defun:hook after-save-hook/bytecompile ()
  (when nil
    (save-window-excursion
      (when (and (eq major-mode 'emacs-lisp-mode)
                 (string-match-p "^[a-z1-9]" (buffer-name))
                 (string-match-p "\\/\\.emacs\\.d\\/\\(core\\|extra\\)\\/" (buffer-file-name)))
        (byte-compile-file (buffer-file-name))))))

(provide 'cust-lists-and-hooks)

;;; cust-lists-and-hooks.el ends here
