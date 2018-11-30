;;; patches.el --- Patches, Override

;;; Code:

(defconst patches-loaded t)


;;; Haskell

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
      (haskell-interactive-mode-compile-error session "Compilation failed."))
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
                      (if match (list 'module (match-string 1 defined))))))))))))))



;;; tide

(defun tide-project-root ()
  "Project root folder determined based on the presence of tsconfig.json."
  (or
   tide-project-root
   (let ((root (or (locate-dominating-file default-directory "tsconfig.json")
                   (locate-dominating-file default-directory "jsconfig.json"))))
     (unless root
       (message "Using current %s as project root." (propertize default-directory 'face '(:foreground "ForestGreen")))
       (setq root default-directory))
     (let ((full-path (expand-file-name root)))
       (setq tide-project-root full-path)
       full-path))))



;;; Simple-Httpd

(defvar httpd-maybe-cache t)
(defvar httpd-body-docorator nil)

(cl-defun httpd-start
    (&key (root "~/vvv") (host "0.0.0.0") (port 5555)
          (cache t) (body-decorator (lambda (mime body) body)))
  (interactive)
  (httpd-stop)
  (httpd-log `(start ,(current-time-string)))
  (if root (setq httpd-root root))
  (setq httpd-maybe-cache cache)
  (setq httpd-body-docorator body-decorator)
  (make-network-process :name     "httpd"
                        :server   t
                        :family   'ipv4
                        :host     host
                        :service  port
                        :coding   'binary
                        :filter-multibyte nil
                        :filter   'httpd--filter
                        :log      'httpd--log)
  (run-hooks 'httpd-start-hook))

(defun httpd-send-file (proc path &optional req)
  (httpd-discard-buffer)
  (let ((etag (httpd-etag path)) s
        (req-etag (cadr (assoc "If-None-Match" req)))
        (mtime (httpd-date-string (nth 4 (file-attributes path))))
        (mime (httpd-get-mime (file-name-extension path))))
    (if (and httpd-maybe-cache (equal req-etag etag))
        (with-temp-buffer
          (httpd-log `(file ,path not-modified))
          (httpd-send-header proc "text/plain" 304))
      (httpd-log `(file ,path))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents path)
        (setq s (funcall httpd-body-docorator mime (buffer-string)))
        (erase-buffer)
        (insert s)
        (httpd-send-header proc mime 200
                           :Cache-Control "no-chache,no-store"
                           :Last-Modified mtime :ETag etag)))))
