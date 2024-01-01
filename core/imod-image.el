;;; -*- lexical-binding: t -*-

;; canvas-mode based on svg, like edraw:
;;   https://gitlab.com/atamariya/emacs/-/blob/dev/lisp/svg.el
;;   https://lifeofpenguin.blogspot.com/2021/08/scribble-notes-in-gnu-emacs.html

;;; Code:

(xzz image-mode
  :mode "\\.otf\\'"
  :config
  (define-key image-mode-map "c" 'im/yank-current-buffer-name))

(xzz gimp
  :commands (connect-gimp gimp-mode))

(xzz edraw
  "Insert [[edraw:]] then type `C-x C-o'.

Not in any elpa now. See Issue #2 for more."
  :ref ("misohena/el-easydraw"
        "Blog: https://misohena.jp/blog/2021-09-21-emacs-easy-draw.html")
  :commands (color-picker-by-edraw
             edraw-color-picker-replace-or-insert-color-at-point)
  :init
  (defalias #'color-picker-by-edraw #'edraw-color-picker-replace-or-insert-color-at-point))

(xzz gnuplot
  ;; TODO: Make it work on Windows.
  :if (executable-find "gnuplot")
  :config (setq gnuplot-program "gnuplot"))

(xzz cowsay
  ;;  ________
  ;; < cowsay >
  ;;  --------
  ;;         \   ^__^
  ;;          \  (oo)\_______
  ;;             (__)\       )\/\
  ;;                 ||----w |
  ;;                 ||     ||
  :ref "lassik/emacs-cowsay")

(xzz figlet
  ;;   _
  ;; _|_o _ | __|_
  ;;  | |(_||(/_|_
  ;;      _|
  :ref "http://www.figlet.org/"
  :commands (figlet figlet-samples))

(xzz all-the-icons
  "M-x all-the-icons-install-fonts."
  :ref "domtronn/all-the-icons.el")
