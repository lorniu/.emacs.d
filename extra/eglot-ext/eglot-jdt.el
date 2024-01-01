;;; eglot-jdt.el --- Java's Eclipse jdt language server -*- lexical-binding: t -*-

;;; Code:

(require 's)
(require 'eglot)

(defvar eglot-jdt-lombok-path "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")

(defvar eglot-jdt-cache-dir (locate-user-emacs-file "eglot-cache/jdt-ls"))


;;; Contract

(defclass eglot-jdt-server (eglot-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(cl-defmethod eglot-workspace-folders ((server eglot-jdt-server))
  (let ((project (eglot--project server)))
    (vconcat
     (mapcar (lambda (dir)
               (list :uri (eglot--path-to-uri dir)
                     :name (abbreviate-file-name dir)))
             `(,(project-root project)
               ,(expand-file-name "jdt.ls-java-project" (eglot-jdt-workspace-dir project))
               ,@(project-external-roots project))))))

(cl-defmethod eglot-initialization-options ((server eglot-jdt-server))
  "Passes through required JDT initialization options."
  `(:settings (:java
               (:import (:maven (:enabled t))
                :configuration (:updateBuildConfiguration "automatic" :checkProjectSettingsExclusions t)
                :project (:referencedLibraries ["lib/**/*.jar"])
                :server (:launchMode "Hybrid")
                :completion (:importOrder ["java" "javax" "com" "org"])))
    :extendedClientCapabilities (:classFileContentsSupport t)))

(defun eglot-jdt-workspace-dir (&optional project)
  (unless project (setq project (eglot--current-project)))
  (let* ((root (project-root project))
         (name (concat (md5 (expand-file-name (project-root project))) "-"
                       (file-name-nondirectory (directory-file-name root))))
         (workspace (expand-file-name name eglot-jdt-cache-dir)))
    (unless (file-directory-p workspace)
      (make-directory workspace t))
    workspace))

(defun eglot-jdt-contract (_interactive)
  "Return cons (CLASS . ARGS) for connecting to Eclipse JDT.
If INTERACTIVE, prompt user for details."
  (unless (executable-find "jdtls")
    (user-error "You should install 'jdtls' first"))
  (unless (file-exists-p eglot-jdt-lombok-path)
    (user-error "You should install 'lombok' to .m2 dir"))
  (cons 'eglot-jdt-server
        (list
         "jdtls"
         (concat "--jvm-arg=-javaagent:" (expand-file-name eglot-jdt-lombok-path))
         "-configuration" (expand-file-name "config" eglot-jdt-cache-dir)
         "-data" (eglot-jdt-workspace-dir))))

(add-to-list 'eglot-server-programs '(java-mode . eglot-jdt-contract))

(defun eglot-jdt-project-try-eglot (dir)
  "Xref outside project directory."
  (when (and eglot--servers-by-project
             (string-match
              (concat
               (file-name-nondirectory (directory-file-name eglot-jdt-cache-dir))
               "/\\([^-]+\\)[^/]+/jdt.ls-java-project/src")
              dir))
    (cl-find-if (lambda (p)
                  (string=
                   (match-string 1 dir)
                   (md5 (expand-file-name (project-root p)))))
                (hash-table-keys eglot--servers-by-project))))

(add-to-list 'project-find-functions #'eglot-jdt-project-try-eglot)


;;; URI handler

(defun eglot-jdt--get-filename (url)
  "Get the name of the buffer calculating it based on URL."
  (or (save-match-data
        (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url)
          (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 url) t t))))
      (pcase-let ((`(_ ,file-name _ ,jar)
                   (s-match "jdt://.*?/\\(.*?\\)\\?=\\(.*?\\)/.*/\\(.*\\)" (url-unhex-string url))))
        (when (and file-name jar)
          (format "%s(%s)" file-name (replace-regexp-in-string "\\\\\\|/" "" jar))))
      (save-match-data
        (when (string-match "chelib://\\(.*\\)" url)
          (let ((matched (match-string 1 url)))
            (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
      (error "Unable to match %s" url)))

(defun eglot-jdt--resolve-uri (uri)
  "Load a file corresponding to URI executing request to the jdt server."
  (let* ((buffer-name (eglot-jdt--get-filename uri))
         (file-location (expand-file-name buffer-name (expand-file-name "jdt.ls-java-project/src" (eglot-jdt-workspace-dir)))))
    (unless (file-readable-p file-location)
      (let ((path (file-name-directory file-location)))
        (unless (file-directory-p path)
          (make-directory path t)))
      (let ((content (jsonrpc-request (eglot--current-server-or-lose) :java/classFileContents (list :uri uri)))
            (meta-location (concat file-location ".metadata-uri")))
        (with-temp-file file-location
          (insert content))
        (with-temp-file meta-location
          (insert uri))))
    file-location))

(defun eglot-jdt-handle-uri (fn url)
  (if (and (stringp url)
           (string-match-p "^\\(jdt\\|chelib\\)://" url))
      (eglot-jdt--resolve-uri url)
    (funcall fn url)))

(advice-add 'eglot--uri-to-path :around #'eglot-jdt-handle-uri)

(provide 'eglot-jdt)

;;; eglot-jdt.el ends here
