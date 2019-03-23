;;; imfavor.el --- Favorite things -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ic/ssh-hosts
  `((,(format "vip@%s" *vps*) . ,(format "/ssh:vip@%s:~/" *vps*)))
  "Frequently visited hosts with tramp. Used by `ssh-edit'."
  :type '(repeat (cons (choice :tag "Name" string) (choice :tag "Host" string)))
  :group 'imfine)

(defcustom ic/favorite-hosts '("https://github.com")
  "Favorite hosts, used by `im/host-viewer'."
  :type '(repeat string)
  :group 'imfine)

(defcustom ic/favorite-files
  '(("immor.el" "~/.emacs.d/core/immor.el" :readonly)
    ("vvv/" "~/vvv/" :interactive)
    ("notes/" note-directory :project)
    ("emacs.cache/" _CACHE_ :interactive)
    ("emacs.script/" "~/.emacs.d/scripts/")
    ("emacs.init.el" custom-file)
    ("xmonad.hs" "~/.notes/x.share/xmonad/xmonad.hs" (env-linux))
    ("/etc/systemd/" "/sudo::/etc/systemd/system/multi-user.target.wants" (env-linux))
    ("Windows Homepath" (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH")) (env-windows)))
  "Favorite files, used by `im/file-viewer'."
  :type '(alist :key-type string)
  :group 'imfine)



(defun ssh-edit ()
  "Shortcut for remote site described in `ic/ssh-hosts'."
  (interactive)
  (let* ((item (completing-read "Connect to: " (mapcar 'car ic/ssh-hosts)))
         (host (or (cdr (assoc-string item ic/ssh-hosts)) item)))
    (find-file host)))

(defun im/host-viewer ()
  "Browser url, candidates with `ic/favorite-hosts'. TODO: add org-file support."
  (interactive)
  (ivy-read "Host: "
            ic/favorite-hosts
            :action (lambda (url)
                      (let ((url (if (string-match-p "^http\\|^ftp" url) url
                                   (concat "http://" url))))
                        (condition-case _
                            (browse-url url)
                          (error (browse-web url)))))))

(defun im/file-viewer ()
  "View my favorite files, via `ic/favorite-files'. 3rd args can be :dir/:readonly/(env-linux)."
  (interactive)
  (ivy-read "File: "
            ic/favorite-files
            :require-match t
            :predicate
            (lambda (lst)
              (let ((path (cadr lst)) (modes (cddr lst)) sexp)
                (if (not (stringp path)) (setq path (eval path)))
                (setf (cadr lst) path)
                (setq sexp (cl-find-if 'listp modes))
                (and (or (file-remote-p path) (file-exists-p path))
                     (or (null sexp) (eval sexp)))))
            :action
            (lambda (lst)
              (let ((path (cadr lst)) (modes (cddr lst)))
                (cond ((and (file-directory-p path) (memq :interactive modes))
                       (let ((default-directory path)) (call-interactively 'dired)))
                      ((and (file-directory-p path) (memq :project modes))
                       (let ((default-directory path)) (call-interactively 'counsel-projectile-find-file)))
                      ((and (file-directory-p path) (memq :project-dir modes))
                       (let ((default-directory path)) (call-interactively 'counsel-projectile-find-dir)))
                      ((memq :readonly modes) (im/open-file-view path))
                      (t (find-file path)))))))

(defun im/add-favor-file (path &optional label first-p mode)
  "Add new file to `ic/favorite-files'."
  (interactive (list (read-file-name "Choose File: " nil nil t)))
  (if (not (file-exists-p path))
      (error "Failed, file '%s' Not Found." path)
    (add-to-list 'ic/favorite-files `(,(or label (file-name-nondirectory path)) ,path ,mode) (not first-p))
    (message "Favor file added successfully.")))


(provide 'imfavor)

;;; imfavor.el ends here
