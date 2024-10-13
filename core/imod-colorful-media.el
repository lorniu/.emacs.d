;;; imod-colorful-media.el --- For Fun -*- lexical-binding: t -*-

;;; Code:

;;; Music

(x emms
   "Play music in emacs.
   "
   "Make sure some player in your PATH (brew install mpv/vls...)
   "
   "To edit tags, install mid3v2 first (pacman -S python-mutagen)
   "
   :commands emms
   :init
   (setq emms-playlist-buffer-name " *EMMS*")
   (setq emms-show-formatoo "Playing: %s")
   (setq emms-directory (locc "emms"))
   (make-directory emms-directory t)
   :config
   (require 'emms-setup)
   (emms-all)
   (emms-cache -1)
   (emms-default-players)
   ;; keys
   (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
   (define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
   (define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
   (define-key emms-playlist-mode-map (kbd "<right>") (lambdai (emms-seek +10)))
   (define-key emms-playlist-mode-map (kbd "<left>")  (lambdai (emms-seek -10))))


;;; Video

(x mpv
   :ref ("https://github.com/kljohann/mpv.el"
         "https://github.com/mpv-player/mpv"
         "https://github.com/yt-dlp/yt-dlp"
         "https://github.com/FFmpeg/FFmpeg"
         "Docs: https://mpv.io/manual/master/#properties")
   :init
   (defun im/mpv-extra-options ()
     (interactive)
     (require 'mpv)
     (let ((s (read-from-minibuffer "MPV Options: "
                                    (mapconcat #'identity mpv-default-options " "))))
       (setq mpv-default-options
             (if (string-equal "" s) nil
               (split-string s " ")))
       (message "%S" mpv-default-options))))

(x mpvi
   :init
   (setq mpvi-danmaku2ass (file-truename (loce "share/danmaku2ass.py")))
   :commands (mpvi-open mpvi-open-from-favors mpvi-seek mpvi-insert mpvi-clip mpvi-emms-add mpvi-org-link-init))


;;; Image

;; canvas-mode based on svg, like edraw:
;;   https://gitlab.com/atamariya/emacs/-/blob/dev/lisp/svg.el
;;   https://lifeofpenguin.blogspot.com/2021/08/scribble-notes-in-gnu-emacs.html

;;; Code:

(x image-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))
   :config
   (define-key image-mode-map "c" 'im/yank-current-buffer-name))

(x gimp
   :commands (connect-gimp gimp-mode))

(x edraw
   "Insert [[edraw:]] then type `C-x C-o'.

Not in any elpa now. See Issue #2 for more."
   :ref ("misohena/el-easydraw"
         "Blog: https://misohena.jp/blog/2021-09-21-emacs-easy-draw.html")
   :commands (color-picker-by-edraw
              edraw-color-picker-replace-or-insert-color-at-point)
   :init
   (with-eval-after-load 'org
     (require 'edraw-org)
     (edraw-org-setup-default))
   (defalias #'color-picker-by-edraw #'edraw-color-picker-replace-or-insert-color-at-point))


;;; Document/Book

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
   :config
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

(provide 'imod-colorful-media)

;;; imod-colorful-media.el ends here
