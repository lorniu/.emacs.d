(deftheme origin "origin when ssh")


(custom-theme-set-faces
 'origin
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(custom-variable-tag ((t (:foreground "#0035ff" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 
 `(magit-diff-context-highlight ((t (:background "#111111" :foreground "#b2b2b2"))))
 '(magit-diff-added-highlight ((t (:background "#66aa66" :foreground "#002200"))))
 '(magit-section-highlight ((t (:background "#333333"))))

 `(ivy-current-match ((t (:background "#444155" :inherit bold))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit bold))))
 `(ivy-minibuffer-match-face-2 ((t (:foreground "#4f97d7" :underline t))))
 `(ivy-minibuffer-match-face-3 ((t (:foreground "#b1951d" :underline t))))
 `(ivy-minibuffer-match-face-4 ((t (:foreground "#67b11d" :underline t))))
 `(ivy-remote ((t (:foreground "#28def0"))))
 )


(provide-theme 'origin)
