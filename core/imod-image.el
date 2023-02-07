;;; -*- lexical-binding: t -*-

;; canvas-mode based on svg, like edraw:
;;   https://gitlab.com/atamariya/emacs/-/blob/dev/lisp/svg.el
;;   https://lifeofpenguin.blogspot.com/2021/08/scribble-notes-in-gnu-emacs.html

;;; Code:

(x image-mode
   :mode "\\.otf\\'"
   :config
   (define-key image-mode-map "c" 'ln/yank-current-buffer-name))

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
   (defalias #'color-picker-by-edraw #'edraw-color-picker-replace-or-insert-color-at-point))

(x gnuplot
   ;; TODO: Make it work on Windows.
   :if (executable-find "gnuplot")
   :config (setq gnuplot-program "gnuplot"))

(x cowsay
   ;;  ________
   ;; < cowsay >
   ;;  --------
   ;;         \   ^__^
   ;;          \  (oo)\_______
   ;;             (__)\       )\/\
   ;;                 ||----w |
   ;;                 ||     ||
   :ref "lassik/emacs-cowsay")

(x figlet
   ;;   _
   ;; _|_o _ | __|_
   ;;  | |(_||(/_|_
   ;;      _|
   :ref "http://www.figlet.org/"
   :commands (figlet figlet-samples))

(x all-the-icons
   "M-x all-the-icons-install-fonts."
   :ref "domtronn/all-the-icons.el")
