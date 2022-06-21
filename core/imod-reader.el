;;; imod-reader.el --- Docs -*- lexical-binding: t -*-

;;; Code:

(x image-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))
   :defer-config
   (define-key image-mode-map "c" 'im/yank-current-buffer-name))



(x pdf-tools
   "A replacement of DocView for PDF files, it's created on-demand."
   :ref "vedang/pdf-tools"
   :init
   (defun:hook find-file-hook/pdf-tools ()
     (and buffer-file-name
          (string-equal (file-name-extension buffer-file-name) "pdf")
          (not (featurep 'pdf-tools))
          (require 'pdf-tools)
          (not (executable-find pdf-info-epdfinfo-program))
          (message "You should execute `M-x pdf-tools-install' to enable pdf-tools")))
   (defun:hook pdf-view-mode-hook ()
     ;; double-click to select current word
     (require 'pdf-sel)
     (pdf-sel-mode 1))
   :defer-config
   ;; fire when epdfinfo exists
   (when (executable-find pdf-info-epdfinfo-program)
     (add-to-list 'magic-mode-alist (cons "%PDF" 'pdf-view-mode))
     (pdf-tools-install :no-query))
   ;; variable
   (setq-default pdf-view-display-size 'fit-page)
   (setq pdf-annot-activate-created-annotations t)
   (setq pdf-view-resize-factor 1.1)
   ;; keymap
   (require 'pdf-history)
   (define-key pdf-history-minor-mode-map (kbd "r") nil)
   (define-key pdf-history-minor-mode-map (kbd "l") nil)
   (define-key pdf-view-mode-map (kbd "r") 'image-rotate)
   (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
   (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
   (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
   (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
   (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
   (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save))

(defun im/rotate-pdf-with-pdftk (&optional counterclockwise-p page)
  "Rotate PDF file with 'pdftk', install it first."
  (interactive (list current-prefix-arg nil))
  (unless (executable-find "pdftk")
    (user-error "Rotation requires pdftk"))
  (let* ((rotate (if counterclockwise-p "right" "left"))
         (file (let ((f (read-file-name "PDF to rotate: " nil nil t)))
                 (if (not (and (file-regular-p f) (string-equal (file-name-extension f) "pdf")))
                     (user-error "Please choose a pdf file")
                   (file-truename f))))
         (pages (cond ((not page) (format "1-end%s" rotate)) ; whole doc?
                      ((= page 1) (format "%d%s %d-end" page rotate (1+ page))) ; first page?
                      (t (format "1-%d %d%s %d-end" (1- page) page rotate (1+ page)))))
         (newfile (format "%s%s.pdf" (file-name-sans-extension file) (gensym '_00)))
         (cmd (format "pdftk '%s' cat %s output '%s'" file pages newfile))
         (ret (shell-command-to-string cmd)))
    (message "Rotate command: %s" cmd)
    (if (file-exists-p newfile)
        (message "Rotate success as '%s'." newfile)
      (user-error "%s" ret))))



(x nov
   "EPUB reader."
   :ref "https://depp.brause.cc/nov.el/"
   :init
   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'imod-reader)

;;; imod-reader.el ends here
