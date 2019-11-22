;;; imooy.el --- Drawing, downloading and so on. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x ob-dot
   :if (executable-find "dot") ;; choco install graphviz
   :config (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot))

(x ob-ditaa
   :if (executable-find "java")
   :config (setq org-ditaa-jar-path (locate-user-emacs-file "share/ditaa.jar")))

(x ob-plantuml
   :if (and (executable-find "java") (executable-find "dot"))
   :init
   (setq plantuml-jar-path (locate-user-emacs-file "plantuml.jar")
         plantuml-default-exec-mode 'jar)
   (setq org-plantuml-jar-path plantuml-jar-path)
   :config
   (when (and (null (file-exists-p org-plantuml-jar-path))
              (yes-or-no-p "Download plantuml.jar Now?"))
     ;; IF NOT WORK, RUN THIS IN SHELL:
     ;; wget https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar -O ~/.emacs.d/plantuml.jar
     (url-copy-file "https://newcontinuum.dl.sourceforge.net/project/plantuml/plantuml.jar" org-plantuml-jar-path)))

(x gnuplot
   ;; TODO: Make it work on Windows.
   :if (executable-find "gnuplot")
   :init (setq gnuplot-program "gnuplot"))

(x org-download
   :init
   (setq org-download-backend (if (executable-find "wget") "wget \"%s\" -O \"%s\"" t))
   (setq org-download-screenshot-file (concat temporary-file-directory "scrot.png"))

   :config
   (defvar org-download-last-save-dir nil)

   (defun org-download-clipboard-method ()
     (cond
      ((executable-find "xclip") "xclip -selection clipboard -t image/png -o > %s")
      ((env-windows) "powershell -Command (Get-Clipboard -Format Image).save('%s')")
      (t (error "no proper tool, please install xclip or powershell"))))

   (defun org-download-clipboard ()
     (interactive)
     (let ((default-directory "~"))
       (shell-command (format (org-download-clipboard-method) org-download-screenshot-file)))
     (org-download-image org-download-screenshot-file))

   (defun org-download--fullname (link &optional ext)
     (let* ((filename (file-name-nondirectory
                       (car (url-path-and-query
                             (url-generic-parse-url link)))))
            (base (if (and org-download-last-save-dir
                           (search (expand-file-name default-directory) org-download-last-save-dir))
                      org-download-last-save-dir
                    default-directory))
            (path (read-file-name "File save as: " base base))
            (dir (file-name-directory path))
            (name (file-name-nondirectory path)))
       (if (not (file-exists-p dir)) (make-directory dir t))
       (setq org-download-last-save-dir (expand-file-name dir))
       (if (not (string-blank-p name))
           (format "%s.%s" path (or ext (file-name-extension filename)))
         (when (string-match ".*?\\.\\(?:png\\|jpg\\)\\(.*\\)$" filename)
           (setq filename (replace-match "" nil nil filename 1)))
         (abbreviate-file-name
          (expand-file-name
           (org-download--fullname-format filename ext)
           dir)))))

   (defun org-download-rename-at-point ()
     (interactive)
     (let* ((link-name (org-element-property :path (org-element-context)))
            (rela-path (file-name-directory link-name))
            (file-name (file-name-nondirectory link-name))
            (dir-path (concat default-directory rela-path))
            (current-path (concat dir-path "/" file-name))
            (ext (file-name-extension file-name))
            (new-name (read-string "Rename file at point to: " (file-name-sans-extension file-name)))
            (new-path (concat dir-path "/" new-name "." ext)))
       (rename-file current-path new-path)
       (message "File successfully renamed to '%s'." new-name)
       (org-download-replace-all link-name (concat rela-path new-name "." ext))
       (org-display-inline-images)))

   (defun org-download-rename-last-file ()
     (interactive)
     (let* ((dir-path (file-name-directory org-download-path-last-file))
            (newname (read-string "Rename last file to: " (file-name-base org-download-path-last-file)))
            (ext (file-name-extension org-download-path-last-file))
            (newpath (concat dir-path newname "." ext)))
       (when org-download-path-last-file
         (rename-file org-download-path-last-file newpath 1)
         (message "Last file renamed to '%s'." newname)
         (org-download-replace-all (file-name-nondirectory org-download-path-last-file) (concat newname "." ext))
         (setq org-download-path-last-file newpath)
         (org-display-inline-images)))))


(provide 'imooy)

;;; imooy.el ends here
