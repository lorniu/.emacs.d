;;; -*- lexical-binding: t -*-

;;; Code:

(xzz markdown-mode
  :ref "jrblevin/markdown-mode"
  :bind (:map markdown-mode-map ("C-c l" . im/markdown-hide-levels))
  :config
  (setq markdown-hide-markup nil
        markdown-fontify-code-blocks-natively nil)
  (defun:hook markdown-mode-hook () (visual-line-mode 1))
  ;; fixup 'which-func-ff-hook error' (#578)
  (setq markdown-nested-imenu-heading-index nil))

(xzz markdown-toc
  "Generate TOC for markdown file"
  :ref "ardumont/markdown-toc")



(transient-define-prefix im/assist-markdown-mode ()
  [:hide
   (lambda () t)
   ("C" "" markdown-toggle-fontify-code-blocks-natively)
   ]
  [["Toggle"
    ("m"   "Markup"         markdown-toggle-markup-hiding)
    ("M"   "Maths"          markdown-toggle-math)
    ("U"   "Url"            markdown-toggle-url-hiding)
    ("I"   "Images"         markdown-toggle-inline-images)
    ]
   ["Action"
    ("o"  "Open"            markdown-open)
    ("e"  "Export"          markdown-export)
    ("P"  "Preview"         markdown-preview)
    ("p"  "Preview-Live"    markdown-live-preview-mode)
    ("r"  "Generate ToC"    markdown-toc-refresh-toc)
    ]
   ["Insert"
    ("i l" "Link"           markdown-insert-link)
    ("i i" "Image"          markdown-insert-image)
    ("i t" "Table"          markdown-insert-table)
    ("i n" "Footnote"       markdown-insert-footnote)
    ("i f" "Foldable Block" markdown-insert-foldable-block)
    ]
   ]
  (interactive)
  (if (memq major-mode '(markdown-mode gfm-mode))
      (transient-setup 'im/assist-markdown-mode)
    (user-error "You should invoke this in markdown-mode or gfm-mode")))

(defalias 'im/assist-gfm-mode 'im/assist-markdown-mode)



(defun im/markdown-hide-levels ()
  "Hide to Lv-2."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "##+ " nil t)
      (outline-hide-entry)
      (forward-line 1))))
