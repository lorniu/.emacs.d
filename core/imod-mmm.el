;;; -*- lexical-binding: t -*-

;;  1. web-mode
;;  2. mmm-mode
;;  3. polymode

;;; Code:

(xzz polymode
  :ref "polymode/polymode"
  :commands (poly-html-mode poly-vue-mode))


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


;;; Poly - Vue.js

(with-eval-after-load 'polymode
  (define-hostmode poly-vue-hostmode
      :mode 'html-mode)
  (define-innermode poly-vue-script-innermode
      :mode 'js-mode
      :head-matcher "<[[:space:]]*\\(?:script\\)[[:space:]]*>"
      :tail-matcher "</[[:space:]]*\\(?:script\\)[[:space:]]*>"
      :head-mode 'host :tail-mode 'host)
  (define-innermode poly-vue-style-innermode
      :mode 'css-mode
      :head-matcher "<[[:space:]]*\\(?:style\\)[[:space:]]*\\(?:scoped\\|[[:space:]]\\)*>"
      :tail-matcher "</[[:space:]]*\\(?:style\\)[[:space:]]*>"
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
  (define-polymode poly-vue-mode
      :hostmode 'poly-vue-hostmode
      :innermodes '(poly-vue-script-tag-lang-innermode
                    poly-vue-style-tag-lang-innermode
                    poly-vue-script-innermode
                    poly-vue-style-innermode)))

;; Local Variables:
;; lisp-indent-function: common-lisp-indent-function
;; End:
