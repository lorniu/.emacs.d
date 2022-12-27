;;; iorg+TeX.el --- LaTeX -*- lexical-binding: t -*-

;;; Code:

(x latex
   :if (executable-find "latex")
   :init
   (setq-default TeX-engine 'xetex)
   :config
   (defun:hook LaTeX-mode-hook/my-latex ()
     (setq TeX-command-default "LaTeX")

     (setq TeX-master t)
     (setq TeX-auto-save t)
     (setq TeX-parse-self t)

     (setq TeX-source-correlate-mode t)
     (setq TeX-source-correlate-method 'synctex)
     (setq reftex-plug-into-AUCTeX t)

     (reftex-mode t)))

(x ox-latex
   :init
   (setq org-latex-listings t

         org-latex-pdf-process
         '("xelatex -interaction nonstopmode %f"
           "xelatex -interaction nonstopmode %f")

         org-latex-default-packages-alist
         '(("UTF8" "ctex"      t)
           (""     "graphicx"  t)
           (""     "longtable" nil)
           (""     "wrapfig"   nil)
           (""     "rotating"  nil)
           ("normalem" "ulem"  t)
           (""     "amsmath"   t)
           (""     "textcomp"  t)
           (""     "amssymb"   t)
           (""     "capt-of"   nil)
           (""     "hyperref"  nil))

         org-babel-latex-htlatex "make4ht -u -x"))


;; tikz + org

(defvar org-tikz-default-convert-alias
  '(("imagemagick"  "convert %s.pdf %s.%e")
    ("pdftocairo"   "pdftocairo -svg %s.pdf %s.%e")
    ("pdf2svg"      "pdf2svg %s.pdf %s.%e")
    ("inkscape"     "inkscape -l %s.%e %s.pdf")))

(defvar org-babel-default-header-args:tikz '((:exports . "results")))

(defun org-babel-execute:tikz (body params)
  "Options:

   - :file
   - :documentclass
   - :header
   - :before
   - :convert
   - :plain

   Support format: svg/pdf, and any convert command output.
"
  (let* ((out-file (cdr (assq :file params)))
         (class (cdr (assq :documentclass params)))
         (extension (file-name-extension (or out-file "")))
         (convert (cdr (assq :convert params))))
    (unless out-file
      (error "Please provide :file option"))
    (unless (or convert (member extension '("pdf" "svg")))
      (error "Should be pdf/svg format or provide :convert command."))
    (if (and convert
             (not (string-match-p " " convert))
             (not (assoc convert org-tikz-default-convert-alias)))
        (error (format "Convert Alias should in: %s" (mapconcat (lambda (x) (car x)) org-tikz-default-convert-alias "/"))))
    (when class
      (if (not (stringp class))
          (error "Please quote documentclass as a String"))
      (and (string= extension "svg") (not (string-match-p "dvisvgm" class))
           (error "'dvisvgm' option should in :documentclass")))

    (let* ((tex-file (org-babel-temp-file "latex-" ".tex"))
           (plain-input-p (cdr (assq :plain params)))
           (header (cdr (assq :header params)))
           (before (cdr (assq :before params)))
           ;; [tikz,dvisvgm]{standalone}
           (template "%%%%
\\documentclass%s
\\usepackage[UTF8]{ctex}
\\usepackage{graphicx,ulem,tikz}
%s\n
\\begin{document}
%s
%s
\\end{document}\n")
           (log-buf-name "*Ob-Tikz Output*")
           (log-buf (get-buffer-create log-buf-name)))
      (with-temp-file tex-file
        (cond ((or (string= extension "pdf") convert)
               (insert
                (if plain-input-p
                    body
                  (format template
                          (or class "[tikz,border=5pt]{standalone}")
                          (or header "")
                          (or before "")
                          body))))
              ((string= extension "svg")
               (insert (if plain-input-p
                           body
                         (format template
                                 (or class "[tikz,border=5pt,dvisvgm]{standalone}")
                                 (or header "")
                                 (or before "")
                                 body))))))
      (when (file-exists-p out-file) (delete-file out-file))
      (save-window-excursion
        (let* ((transient-file
                (let ((default-directory (file-name-directory tex-file))
                      (file-no-ext (file-name-sans-extension tex-file)))
                  (cond ((string= extension "pdf")
                         (shell-command (format "xelatex -interaction nonstopmode %s" tex-file) log-buf))
                        (convert
                         (let ((alias (cadr (assoc convert org-tikz-default-convert-alias))))
                           (if alias (setq convert alias)))
                         (setq convert (replace-regexp-in-string "%e" extension convert))
                         (setq convert (replace-regexp-in-string "%s" file-no-ext convert))
                         (shell-command (format "xelatex -interaction nonstopmode %s && %s" tex-file convert) log-buf))
                        ((string= extension "svg")
                         (shell-command (format "xelatex -no-pdf %s && dvisvgm %s.xdv" tex-file file-no-ext) log-buf)))
                  (format "%s.%s" file-no-ext extension)))
               (warnings
                (progn
                  (require 'ox-latex)
                  (with-current-buffer log-buf (compilation-mode))
                  (org-latex--collect-warnings log-buf)))
               (errorp nil))
          (if (not (file-exists-p transient-file))
              (setq errorp t)
            (ignore-errors (make-directory (file-name-directory out-file)))
            (rename-file transient-file out-file))
          (mapc #'delete-file (directory-files (file-name-directory tex-file)
                                               t
                                               (concat
                                                (regexp-quote (file-name-base tex-file))
                                                "\\(?:\\.[0-9]+\\)?\\."
                                                (regexp-opt org-latex-logfiles-extensions))
                                               t))
          (if errorp (error "File produced with errors: %s, detail from %s." warnings log-buf))))
      nil)))

(provide 'iorg+TeX)

;;; iorg+TeX.el ends here
