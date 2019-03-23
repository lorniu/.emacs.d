;;; patches.el --- Patches, Overrides -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst PATCHES-VERSION 0.1)



;;; Haskell Mode

(defun patch/haskell ()
  (defun haskell-process-type ()
    (let ((cabal-sandbox (locate-dominating-file default-directory "cabal.sandbox.config"))
          (stack         (locate-dominating-file default-directory "stack.yaml"))
          (cabal-new     (locate-dominating-file default-directory "cabal.project"))
          (cabal         (locate-dominating-file default-directory (lambda (d) (cl-find-if (lambda (f) (string-match-p ".\\.cabal\\'" f)) (directory-files d))))))
      (if (eq 'auto haskell-process-type)
          (cond
           ((and cabal-sandbox (executable-find "cabal")) (setq inferior-haskell-root-dir cabal-sandbox) 'cabal-repl)
           ((and stack (executable-find "stack")) (setq inferior-haskell-root-dir stack) 'stack-ghci)
           ((and cabal-new (executable-find "cabal")) (setq inferior-haskell-root-dir cabal-new) 'cabal-new-repl)
           ((and cabal (executable-find "cabal")) (setq inferior-haskell-root-dir cabal) 'cabal-repl)
           ((executable-find "ghc") (setq inferior-haskell-root-dir default-directory) 'ghci)
           (t (error "Could not find any installation of GHC.")))
        haskell-process-type)))

  (defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
    (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
      (with-current-buffer (haskell-interactive-mode-splices-buffer session)
        (erase-buffer)))
    (let* ((ok (cond
                ((haskell-process-consume process "Ok,\\(?:.+\\) modules? loaded\\.$") t)
                ((haskell-process-consume process "Failed,\\(?:.+\\) modules? loaded\\.$") nil)
                ((haskell-process-consume process "Ok, modules loaded: \\(.+\\)\\.$") t)
                ((haskell-process-consume process "Failed, modules loaded: \\(.+\\)\\.$") nil)
                (t (error (message "Unexpected response from haskell process.")))))
           (modules (haskell-process-extract-modules buffer))
           (cursor (haskell-process-response-cursor process))
           (warning-count 0))
      (haskell-process-set-response-cursor process 0)
      (haskell-check-remove-overlays module-buffer)
      (while
          (haskell-process-errors-warnings module-buffer session process buffer)
        (setq warning-count (1+ warning-count)))
      (haskell-process-set-response-cursor process cursor)
      (if (and (not reload) haskell-process-reload-with-fbytecode)
          (haskell-process-reload-with-fbytecode process module-buffer)
        (haskell-process-import-modules process (car modules)))
      (if ok (haskell-mode-message-line (if reload "Reloaded OK." "OK."))
        (haskell-interactive-mode-compile-error session "Compilation Failed."))
      (when cont
        (condition-case-unless-debug e
            (funcall cont ok)
          (error (message "%S" e))
          (quit nil)))))

  (defun haskell-mode-find-def (ident)
    (when (stringp ident)
      (let ((reply (haskell-process-queue-sync-request
                    (haskell-interactive-process)
                    (format (if (string-match "^[a-zA-Z_]" ident) ":info %s" ":info (%s)") ident))))
        (let ((match (string-match "-- Defined \\(at\\|in\\) \\(.+\\)$" reply)))
          (when match
            (let ((defined (match-string 2 reply)))
              (let ((match (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)$" defined)))
                (if match (list 'file
                                (expand-file-name (match-string 1 defined) (haskell-session-current-dir (haskell-interactive-session)))
                                (string-to-number (match-string 2 defined))
                                (string-to-number (match-string 3 defined)))
                  (let ((match (string-match "‘\\(.+?\\):\\(.+?\\)’$" defined)))
                    (if match (list 'library
                                    (match-string 1 defined)
                                    (match-string 2 defined))
                      (let ((match (string-match "‘\\(.+?\\)’$" defined)))
                        (if match (list 'module (match-string 1 defined)))))))))))))))



;;; Eshell-Visual

(defun patch/eshell ()
  (defun eshell-exec-visual (&rest args)
    (let* (eshell-interpreter-alist
	       (interp (eshell-find-interpreter (car args) (cdr args)))
	       (program (car interp))
	       (args (eshell-flatten-list (eshell-stringify-list (append (cdr interp) (cdr args)))))
	       (term-buf (generate-new-buffer (concat "*" (file-name-nondirectory program) "*")))
	       (eshell-buf (current-buffer))
           (win-git-p (and (eq system-type 'windows-nt) (string-match-p "git" program))))
      (save-current-buffer
        (switch-to-buffer term-buf)
        (term-mode)
        (set (make-local-variable 'term-term-name) eshell-term-name)
        (make-local-variable 'eshell-parent-buffer)
        (setq eshell-parent-buffer eshell-buf)
        ;; force less with page break
        (when win-git-p
          (setq args (append (list "-c" "core.pager=less -+F") args)))
        (term-exec term-buf program program nil args)
        (let ((proc (get-buffer-process term-buf)))
	      (if (and proc (eq 'run (process-status proc)))
              (progn
                (if win-git-p (set-process-coding-system proc 'utf-8-unix 'utf-8-unix))
	            (set-process-sentinel proc 'eshell-term-sentinel))
	        (error "Failed to invoke visual command")))
        (term-char-mode)
        (if eshell-escape-control-x
	        (term-set-escape-char ?\C-x))))
    nil))


(provide 'patches)

;;; patches.el ends here
