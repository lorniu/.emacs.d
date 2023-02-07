;;; -*- lexical-binding: t -*-

;; https://gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html

;;; Code:

(defvar ln:mode-line-prev '())
(defvar ln:mode-line-post '((ln-proxy (:eval ln:proxy-format))))

(put 'ln:mode-line-prev 'risky-local-variable t)
(put 'ln:mode-line-post 'risky-local-variable t)


;;; Show current project

(defvar ln:project-mode-line '(:eval (ln:project-mode-line)))

(put 'ln:project-mode-line 'risky-local-variable t)

(defvar-local ln:project-of-current-buffer nil)

(defun ln:project-mode-line ()
  (or ln:project-of-current-buffer
      (setq ln:project-of-current-buffer
            (if-let* ((pname (project-root (project-current))))
                (propertize (if-let* ((r (file-remote-p default-directory)))
                                (format "{%s}  " (directory-file-name r))
                              (format "<%s>  " (file-name-nondirectory (directory-file-name pname))))
                            'face (if (facep 'project-mode-line-face) 'project-mode-line-face 'font-lock-comment-face))
              " "))))


;;; Custom buffer-name on mode-line

(setq-default mode-line-buffer-identification
              (list
               (propertize "%12b"       ; buffer-name
                           'face 'mode-line-buffer-id
                           'mouse-face 'mode-line-highlight
                           'help-echo '(format "%s\nMouse-1 click to copy the path"
                                               (or (buffer-file-name) default-directory))
                           'local-map mode-line-buffer-identification-keymap)))

(define-key mode-line-buffer-identification-keymap [mode-line mouse-1] #'ln/yank-current-full-name)
(define-key mode-line-buffer-identification-keymap [mode-line mouse-3] #'ln/yank-current-dir-and-buffer-name)


;;; Show (point) on mode-line

(defvar ln:current-point-p nil)

(setq-default mode-line-position
              `((:propertize ("" mode-line-percent-position)
                             display (min-width (5.0))
                             mouse-face mode-line-highlight
                             help-echo "Window Scroll Percentage")
                (:propertize (:eval (concat " (%l,%c" (if ln:current-point-p (format " %s" (point))) ")"))
                             display (min-width (10.0))
                             mouse-face mode-line-highlight
                             help-echo "Row and column of current point\nMouse-1 to triggle show current point position"
                             local-map (keymap (mode-line keymap (mouse-1 . (lambda ()
                                                                              (interactive)
                                                                              (setq ln:current-point-p (not ln:current-point-p))
                                                                              (force-mode-line-update))))))
                (ln:current-point-p "  ")))


;; Mode-Line

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated) display (min-width (6.0)))
                ,(if IS-G 'mode-line-frame-identification "  ")

                ln:project-mode-line
                ln:mode-line-prev

                mode-line-buffer-identification ; name
                "   " mode-line-position        ; positions
                ,(if IS-G `(vc-mode vc-mode))   ; vcs/git
                "  " mode-line-modes " "        ; modes

                mode-line-misc-info
                ln:mode-line-post
                mode-line-end-spaces))
