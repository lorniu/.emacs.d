;;; -*- lexical-binding: t -*-

;; https://gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html

;;; Code:

(defvar im:mode-line-prev '())
(defvar im:mode-line-post '((im.proxy (:eval im:proxy-format))))

(put 'im:mode-line-prev 'risky-local-variable t)
(put 'im:mode-line-post 'risky-local-variable t)


;;; Show current project

(defvar im:project-mode-line '(:eval (im:project-mode-line)))

(put 'im:project-mode-line 'risky-local-variable t)

(defvar-local im:project-of-current-buffer nil)

(defun im:project-mode-line ()
  (or im:project-of-current-buffer
      (setq im:project-of-current-buffer
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

(define-key mode-line-buffer-identification-keymap [mode-line mouse-1] #'im/yank-current-full-name)
(define-key mode-line-buffer-identification-keymap [mode-line mouse-3] #'im/yank-current-dir-and-buffer-name)


;;; Show (point) on mode-line

(defvar im:current-point-p nil)

(setq-default mode-line-position
              `((:propertize ("" mode-line-percent-position)
                             display (min-width (5.0))
                             mouse-face mode-line-highlight
                             help-echo "Window Scroll Percentage")
                (:propertize (:eval (concat " (%l,%c" (if im:current-point-p (format " %s" (point))) ")"))
                             display (min-width (10.0))
                             mouse-face mode-line-highlight
                             help-echo "Row and column of current point\nMouse-1 to triggle show current point position"
                             local-map (keymap (mode-line keymap (mouse-1 . (lambda ()
                                                                              (interactive)
                                                                              (setq im:current-point-p (not im:current-point-p))
                                                                              (force-mode-line-update))))))
                (im:current-point-p "  ")))


;; Mode-Line

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated) display (min-width (6.0)))
                ,(if IS-G 'mode-line-frame-identification "  ")

                im:project-mode-line
                im:mode-line-prev

                mode-line-buffer-identification ; name
                "   " mode-line-position        ; positions
                ,(if IS-G `(vc-mode vc-mode))   ; vcs/git
                "  " mode-line-modes " "        ; modes

                mode-line-misc-info
                im:mode-line-post
                mode-line-end-spaces))
