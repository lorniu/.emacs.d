;;; imod-graph.el --- imod-graph -*- lexical-binding: t -*-

;; canvas-mode based on svg, like edraw:
;;   https://gitlab.com/atamariya/emacs/-/blob/dev/lisp/svg.el
;;   https://lifeofpenguin.blogspot.com/2021/08/scribble-notes-in-gnu-emacs.html

;;; Code:

(x gimp
   :commands (connect-gimp gimp-mode))

(x edraw
   "Not in any elpa now. See Issue #2 for more."
   :ref ("misohena/el-easydraw"
         "Blog: https://misohena.jp/blog/2021-09-21-emacs-easy-draw.html")
   :commands (edraw-editor-create))

(provide 'imod-graph)

;;; imod-graph.el ends here
