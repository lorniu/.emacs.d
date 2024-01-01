;;; -*- lexical-binding: t -*-

;;; Code:

(xzz nov
  "EPUB reader."
  :ref "https://depp.brause.cc/nov.el/"
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (when IS-WIN
    (setq nov-unzip-program (executable-find "bsdtar")
          nov-unzip-args '("-xC" directory "-f" filename))))

(xzz nov-xwidget
  :after nov
  :if (featurep 'xwidget)
  :init
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

(xzz pdf-tools
  "A replacement of DocView for PDF files, it's created on-demand."
  :ref "vedang/pdf-tools"
  :preface (defvar im.pdf-tools-p IS-G)
  :if im.pdf-tools-p
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook ((pdf-view-mode . (lambda () (require 'pdf-sel) (pdf-sel-mode 1))))
  :config
  (setopt pdf-view-display-size 'fit-page
          pdf-annot-activate-created-annotations t
          pdf-view-resize-factor 1.1)
  (if (not (executable-find pdf-info-epdfinfo-program))
      (message "You should execute `M-x pdf-tools-install' to enable pdf-tools")
    (require 'pdf-history)
    (define-key pdf-history-minor-mode-map (kbd "r") nil)
    (define-key pdf-history-minor-mode-map (kbd "l") nil)
    (define-key pdf-view-mode-map (kbd "r") 'image-rotate)
    (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
    (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
    (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
    (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)))

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

(defun:around abort-if-file-too-large//exclude-some-files (fn &rest args)
  (let ((type (file-name-extension (caddr args))))
    (unless (member type '("pdf" "epub"))
      (apply fn args))))

(cl-defmethod im:local-assist ((_ (eql 'nov-mode)))
  (im/describe-major-mode-keymap))

(cl-defmethod im:local-assist ((_ (eql 'pdf-view-mode)))
  (im/describe-major-mode-keymap))
