;;; imod-mmm.el --- Multiple Major Modes -*- lexical-binding: t -*-

;; web-mode/mmm-mode/polymode

;;; Code:

(x polymode
   :ref "polymode/polymode"
   :init
   (push '("\\.org$" . org-mode) auto-mode-alist)
   (push '("\\.md$" . markdown-mode) auto-mode-alist)
   (eval-after-load 'org '(require 'poly-org))
   (eval-after-load 'markdown-mode '(require 'poly-markdown))
   :commands (poly-html-mode poly-vue-mode))

(x edit-indirect
   :ref "Fanael/edit-indirect")


;;; Poly - Html

(with-eval-after-load 'polymode
  (define-hostmode poly-html-hostmode
      :mode 'mhtml-mode)
  (define-innermode poly-html-js-innermode
      :mode 'js-mode
      :head-matcher "<script.*>"
      :tail-matcher "</script>"
      :head-mode 'host
      :tail-mode 'host)
  (define-innermode poly-html-css-innermode
      :mode 'css-mode
      :head-matcher "<style.*>"
      :tail-matcher "</style>"
      :head-mode 'host
      :tail-mode 'host)
  (define-polymode poly-html-mode
      :hostmode 'poly-html-hostmode
      :innermodes '(poly-html-js-innermode poly-html-css-innermode)))


;;; Poly - Vue

(with-eval-after-load 'polymode
  (define-innermode poly-vue-template-innermode
      :mode 'html-mode
      :head-matcher "^<[[:space:]]*\\(?:template\\)[[:space:]]*>"
      :tail-matcher "^</[[:space:]]*\\(?:template\\)[[:space:]]*>"
      :head-mode 'host :tail-mode 'host)
  (define-innermode poly-vue-script-innermode
      :mode 'js-mode
      :head-matcher "<[[:space:]]*\\(?:script\\)[[:space:]]*>"
      :tail-matcher "</[[:space:]]*\\(?:script\\)[[:space:]]*>"
      :head-mode 'host :tail-mode 'host)
  (define-auto-innermode poly-vue-template-tag-lang-innermode
      :head-matcher "^<[[:space:]]*\\(?:template\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
      :tail-matcher "^</[[:space:]]*\\(?:template\\)[[:space:]]*>"
      :mode-matcher (cons  "^<[[:space:]]*\\(?:template\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
      :head-mode 'host :tail-mode 'host)
  (define-auto-innermode poly-vue-script-tag-lang-innermode
      :head-matcher "<[[:space:]]*\\(?:script\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
      :tail-matcher "</[[:space:]]*\\(?:script\\)[[:space:]]*>"
      :mode-matcher (cons  "<[[:space:]]*\\(?:script\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
      :head-mode 'host :tail-mode 'host)
  (define-auto-innermode poly-vue-style-tag-lang-innermode
      :head-matcher "<[[:space:]]*\\(?:style\\)\\(?:scoped\\|[[:space:]]\\)*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"']*\\(?:scoped\\|[[:space:]]\\)*>"
      :tail-matcher "</[[:space:]]*\\(?:style\\)[[:space:]]*>"
      :mode-matcher (cons  "<[[:space:]]*\\(?:style\\)\\(?:scoped\\|[[:space:]]\\)*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"']\\(?:scoped\\|[[:space:]]\\)*>" 1)
      :head-mode 'host :tail-mode 'host)
  (define-innermode poly-vue-style-innermode
      :mode 'css-mode
      :head-matcher "<[[:space:]]*\\(?:style\\)[[:space:]]*\\(?:scoped\\|[[:space:]]\\)*>"
      :tail-matcher "</[[:space:]]*\\(?:style\\)[[:space:]]*>"
      :head-mode 'host :tail-mode 'host)
  (define-polymode poly-vue-mode
      :hostmode 'poly-sgml-hostmode
      :innermodes '(poly-vue-template-tag-lang-innermode
                    poly-vue-script-tag-lang-innermode
                    poly-vue-style-tag-lang-innermode
                    poly-vue-template-innermode
                    poly-vue-script-innermode
                    poly-vue-style-innermode)))

(provide 'imod-mmm)

;; Local Variables:
;; lisp-indent-function: common-lisp-indent-function
;; End:

;;; imod-mmm.el ends here
