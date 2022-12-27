;;; icod-lisp.el --- lisp family -*- lexical-binding: t -*-

;; [Common Lisp]

;; Choose one plan before you begin:
;; 1. Install ros, then use 'ros install xxx' to manage your env.
;; 2. Install sbcl/ccl manually, then config slime/sly as you wish.
;;
;; Introduction:
;; - SLIME (The Superior Lisp Interaction Mode for Emacs) maybe the best IDE for lisp.
;; - Sly is a fork and improved version of SLIME
;;   + Flex-style [Completion] out-of-box, better than slime-company.
;;   + Fancy [Tracer], C-c C-t/T
;;   + Wanderful [Sticker], Non-intrusive logger, C-c C-s C-s/r
;;   + [Presentations] object with backreferences
;; - Roswell is tool like 'pip/gem', it can help to maintain lisp env and convenience to exec lisp script.

;;; Code:

(require 'lisp-mode)


;;; Common Lisp

(defvar ic/lisp-dev 'sly)
(defvar ic/lisp-init-file (loce "share/lisp/init.lisp"))

(prog1 'assert-lisp-env
  (defun %lisp/devenv-nofound ()
    (if ic/lisp-dev (message "`ic/lisp-dev' is `%s', but not found in `load-path'." ic/lisp-dev)
      (message "Please setup `ic/lisp-dev' correctlly.")))
  (when ic/lisp-dev
    (condition-case _err
        (load (format "%s-autoloads" ic/lisp-dev) nil t)
      (error (%lisp/devenv-nofound)))))

(x lisp-mode
   :ref ("SLY: joaotavora/sly"
         "IRC-Log: https://irclog.tymoon.eu/libera/%23commonlisp")
   :init
   (pcase ic/lisp-dev
     ('slime
      (setq slime-net-coding-system                      'utf-8-unix
            slime-inhibit-pipelining                     t
            slime-repl-banner-function                   (lambda () nil)
            slime-enable-evaluate-in-emacs               t)
      (with-eval-after-load 'slime
        (slime-setup '(slime-fancy slime-asdf slime-hyperdoc))
        (defun:hook slime-connected-hook () (%lisp/load-init-file))))

     ('sly
      (setq org-babel-lisp-eval-fn 'sly-eval)
      (setq sly-enable-evaluate-in-emacs t)
      (setq sly-description-autofocus t)
      (defun:hook sly-mode-hook ()
        (define-key sly-editing-mode-map (kbd "M-n") nil)
        (define-key sly-editing-mode-map (kbd "M-p") nil))
      (defun:hook sly-mrepl-mode-hook ()
        (local-set-key (kbd "C-r") 'isearch-backward)
        (local-set-key (kbd "C-c M-o") 'sly-mrepl-clear-repl))
      (defun:hook sly-connected-hook () (%lisp/load-init-file))
      (defun:override sly-push-definition-stack$ () (xref-push-marker-stack)))

     (_ (message "Please install and add slime/sly to `load-path'!"))))

(cl-macrolet ((define-lisp-implementations (&rest decl)
                (macroexp-progn
                 (cl-loop
                  with var = (intern (format "%s-lisp-implementations" ic/lisp-dev))
                  for (symbol . args) in (cl-remove-if (lambda (x) (not (executable-find (caadr x)))) decl)
                  for funame = (intern (format "%s/%s" ic/lisp-dev symbol))
                  collect `(defun ,funame () (interactive) (,ic/lisp-dev ',symbol)) into fs
                  collect `(,symbol ,@args) into hs
                  finally return (append fs (list `(with-eval-after-load ic/lisp-dev (setq ,var (append ',hs ,var)))))))))
  (define-lisp-implementations
   (ros  ("ros" "run"))
   (ccl  ("ccl"))
   (sbcl ("sbcl"))))

(defun %lisp/load-init-file ()
  (when (file-exists-p ic/lisp-init-file)
    (funcall
     (pcase ic/lisp-dev
       ('slime 'slime-eval-async)
       ('sly 'sly-eval-async))
     `(cl:unless
       (cl:member :imloaded cl:*features*)
       (cl:load (cl:merge-pathnames ,ic/lisp-init-file))))))

(defun im/ensure-quicklisp ()
  (interactive)
  (let ((url "http://beta.quicklisp.org/quicklisp.lisp")
        (file (expand-file-name "~/.quicklisp/quicklisp.lisp")))
    (unless (file-exists-p file)
      (files--ensure-directory (file-name-directory file))
      (url-copy-file url file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (insert (format "\n(quicklisp-quickstart:install :path \"%s\")" (file-name-directory file)))
        (insert "(let ((*do-not-prompt* t)) (ql:add-to-init-file))")
        (pcase ic/lisp-dev ('sly (sly-eval-buffer)) ('slime (slime-eval-buffer)))))))


;;; Scheme

;; Geiser is the slime of Scheme, but too heavy.
;; So use `run-scheme' function instead, enough for me.

(x cmuscheme
   "Run scheme with `run-scheme' command directly."
   :commands run-scheme)


;;; Emacs Lisp

(x macrostep
   :ref "joddie/macrostep"
   :config
   (setq macrostep-expand-in-separate-buffer nil))

(defun im/dispatch-macroexpand (&optional arg)
  "Expand macro for emacs lisp."
  (interactive "P")
  (if (and (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))
      (save-excursion
        (when (and (= ?\n (char-after))
                   (= (point) (cdr (bounds-of-thing-at-point 'sexp))))
          (backward-char))
        (require 'macrostep)
        (let ((macrostep-expand-in-separate-buffer arg))
          (macrostep-expand)))
    (call-interactively #'pp-macroexpand-expression)
    (switch-to-buffer-other-window "*Pp Macroexpand Output*")
    (local-set-key "q" #'delete-window)))

(transient-define-prefix imtt/transient-debugger-mode ()
  [[ ("d" "debugger-step-through" debugger-step-through)
     ("c" "debugger-continue" debugger-continue)
     ("j" "debugger-jump" debugger-jump) ]
   [ ("e" "debugger-eval-expression" debugger-eval-expression)
     ("r" "debugger-return-value" debugger-return-value)
     ("R" "debugger-record-expression" debugger-record-expression) ]
   [ ("b" "debugger-frame" debugger-frame)
     ("u" "debugger-frame-clear" debugger-frame-clear)
     ("l" "debugger-list-functions" debugger-list-functions) ]]
  (interactive)
  (if (eq major-mode 'debugger-mode)
      (transient-setup 'imtt/transient-debugger-mode)
    (user-error "Sorry, but this is not debugger-mode")))

(provide 'icod-lisp)

;;; icod-lisp.el ends here
