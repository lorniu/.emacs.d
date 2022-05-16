;;; favors.el --- Favors -*- lexical-binding: t -*-

;;; Code:

(defcustom ic/favorites nil
  "Favorite files, used by `im/view-favorites'."
  :type '(alist :key-type string))

(defcustom ic/favorite-filter nil
  "Extra filter. A function to filter current ITEM."
  :type 'string)


;;; C-x C-r

(defvar favorites-default
  `(("notes/"                   org-directory  :project)
    ("emacs/"                   (loce "core/") :project)
    ("emacs.cache/"             (locc))
    ("emacs.share/"             (loco "share/emacs/"))

    ("h.source"                 "~/source/")
    ("n.webapp"                 (loco "webapp/"))
    ("n.samples"                (loco "samples/"))
    ("n.draft"                  (loco "000/draft.org"))
    ("n.favors"                 (loco "000/favors.org"))
    ("n.scripts"                (loco "000/scripts.org"))

    ("conf: private-init.el"    custom-file)
    ("conf: xmonad.hs"          (let ((local (loco (format "share/xmonad/xmonad-%s.hs" system-name)))
                                      (common (loce "share/xmonad/xmonad.hs")))
                                  (if (file-exists-p local) local common)))
    ("conf: pacman.conf"        "/sudo::/etc/pacman.conf" (file-exists-p "/etc/pacman.conf"))

    ("lisp: emacs/init.lisp"    (loce "share/lisp/init.lisp"))
    ("lisp: private-init.lisp"  (loco (format "share/lisp/init-%s.lisp" system-name)))
    ("lisp: quicklisp-local"    "~/.roswell/lisp/quicklisp/local-projects/")
    ("lisp: quicklisp-software" "~/.roswell/lisp/quicklisp/dists/quicklisp/software/")

    ("host: Dropbox"            "https://dropbox.com")
    ("host: MvnRepository"      "https://mvnrepository.com")

    ((format "ssh: %s" (imup))  (format "/ssh:%s:~/" (imup))                         (imup))

    ("sys: /usr/local/"         "/sudo::/usr/local/"                                 IS-BSD)
    ("sys: /etc/systemd/"       "/sudo::/etc/systemd/system/multi-user.target.wants" (file-directory-p "/etc/systemd"))
    ("sys: Windows Homepath"    (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))    IS-WIN))
  "((label path modes)+)")

(defun im/view-favorites ()
  "View favorites. 3rd args can be :interactive/:readonly/IS-LINUX..."
  (interactive)
  (cl-labels ((label-tag (label) ; (host/x xxx host)
                (if (string-match-p ": " label)
                    (let ((ret (split-string label ": ")))
                      (append ret (list (if (string-match-p "/" (car ret))
                                            (car (split-string label "/"))
                                          (car ret)))))))
              (db-p (label)
                (string-match-p "^db:" label))
              (host-p (label)
                (aif (label-tag label) (string-match-p "^host" (car it)))))
    (let* ((vertico-sort-function)
           (candidates (mapcar (lambda (item)
                                 (let ((label (car item)) (path (cadr item)))
                                   (when (not (stringp label))
                                     (setq label (eval label)))
                                   (when (and (not (db-p label)) (not (stringp path)))
                                     (setq path (eval path)))
                                   ;; face
                                   (cond ((listp path))
                                         ((file-remote-p path)
                                          (setq label (propertize label 'face '(:weight bold))))
                                         ((file-directory-p path)
                                          (setq label (propertize label 'face '(:underline t)))))
                                   ;; tag/format
                                   (aif (label-tag label)
                                     (setq label
                                           (cond ((db-p label)
                                                  (format "%-6s %s"
                                                          (propertize (concat (car it) ":") 'face 'font-lock-keyword-face)
                                                          (cadr it)))
                                                 ((host-p label)
                                                  (format "%-22s %s"
                                                          (format "%-6s %s"
                                                                  (propertize (concat (car it) ":") 'face dired-header-face)
                                                                  (cadr it))
                                                          (propertize (concat "(" path ")") 'face dired-ignored-face)))
                                                 (t
                                                  (format "%-6s %s"
                                                          (propertize (concat (car it) ":") 'face 'font-lock-string-face)
                                                          (cadr it))))))
                                   (append (list label path) (cddr item))))
                               (append ic/favorites favorites-default)))
           (candidates-refind (cl-remove-if-not
                               (lambda (item)
                                 (let ((label (car item)) (path (cadr item))
                                       (sex-p (cl-find-if 'listp (cddr item))))
                                   (and path
                                        (not (cl-find :disabled (cddr item)))
                                        (or (null sex-p)
                                            (eval sex-p))
                                        (or (null ic/favorite-filter)
                                            (not (funcall ic/favorite-filter item)))
                                        (or (db-p label)
                                            (host-p label)
                                            (file-remote-p path)
                                            (file-exists-p path)))))
                               (append
                                (cl-remove-if #'label-tag candidates :key 'car)
                                (cl-sort (cl-remove-if-not #'label-tag candidates :key 'car)
                                         (lambda (x y)
                                           (let ((tag-x (label-tag x))
                                                 (tag-y (label-tag y))
                                                 (host-x-p (host-p x))
                                                 (host-y-p (host-p y)))
                                             (cond ((and host-x-p (not host-y-p)) nil)
                                                   ((and host-y-p (not host-x-p)) t)
                                                   ((and tag-x tag-y
                                                         (string-equal (caddr tag-x) (caddr tag-y))
                                                         (not (string-equal (car tag-x) (car tag-y))))
                                                    (< (length (car tag-x)) (length (car tag-y))))
                                                   (t (string-lessp x y)))))
                                         :key #'car))))
           (candidate (completing-read "Favorites: " candidates-refind nil t nil nil (caar candidates-refind)))
           (item (or (cl-find-if (lambda (c) (string= (car c) candidate)) candidates-refind) (user-error "Nothing to do.")))
           (label (car item))
           (path (cadr item))
           (modes (cddr item)))
      (cond ((db-p label) ; DB link
             (sql-connect (car path)))

            ((host-p label) ; HOST link
             (condition-case _ (browse-url path)
               (error (browse-web path))))

            ;; directory
            ((and (file-directory-p path) (memq :project modes))
             (let ((default-directory path))
               (call-interactively 'project-find-file)))
            ((and (file-directory-p path) (memq :project-dir modes))
             (let ((default-directory path))
               (call-interactively 'im/project-find-directory)))
            ((file-directory-p path)
             (let ((default-directory path))
               (call-interactively 'dired)))

            ((memq :readonly modes) (im-open-file-view path))
            (t (find-file path))))))

(defun im/add-favorite (label path &optional first-p mode)
  "Add new file to `ic/favorites', MODE can be :interactive/:project/:project-dir/:readonly or (show-condition)."
  (interactive (let ((f (read-file-name "Choose File: " nil nil t)))
                 (list (file-name-nondirectory f) f)))
  (if (and (not (string-prefix-p "db" label))
           (not (string-prefix-p "host" label))
           (not (file-remote-p path))
           (not (file-exists-p path)))
      (user-error "Failed, file '%s' Not Found." path)
    (let ((favor (list label path)))
      (if mode (setq favor (append favor (list mode))))
      (add-to-list 'ic/favorites favor (not first-p)))
    (if (called-interactively-p 'any) (message "Favor file '%s' added successfully." label) label)))


;;; r/xxx

(defmacro defreference (name &rest refs)
  (declare (indent 1))
  (let* ((flatten (cl-loop for item in refs
                           if (stringp item) collect item
                           if (listp item) append
                           (if (stringp (car item))
                               (cl-loop for r in item collect (eval r))
                             (list (eval item)))))
         (consed (if (null flatten) (user-error "No suitable reference.")
                   (cl-loop for item in flatten
                            if (string-match "^\\(.*\\): \\(.*\\)$" item) collect
                            (cons (match-string 2 item) (match-string 1 item))
                            else collect (cons item nil))))
         (formatted (cl-loop for item in consed
                             for ref = (car item)
                             if (not (string-prefix-p "http" ref)) do
                             (setq ref (format "https://github.com/%s" ref))
                             collect (cons ref (cdr item))))
         (propertized (cl-loop with face = 'font-lock-doc-face
                               for item in formatted
                               for label = (cdr item)
                               if label do
                               (let ((len (cl-loop for i in formatted if (cdr i) maximize (1+ (length (car i))))))
                                 (setq label (format (format "%%-%ds (%%s)" len) (car item) label))
                                 (add-face-text-property (length (car item)) (length label) face nil label))
                               else do
                               (setq label (car item))
                               collect label)))
    (setq name (symbol-name name))
    (let ((fun (intern
                (if (string-match-p "^|.*|$" name)
                    (substring name 1 -1)
                  (format (concat (unless (string-match-p "/" name) "r/") (if (cdr flatten) "%s*" "%s")) name)))))
      `(defun ,fun ()
         ,(format "%s\n\nBrowser/Yank it." propertized)
         (interactive)
         (let* ((refs ',propertized)
                (ref (car (split-string
                           (minibuffer-with-setup-hook
                               (lambda ()
                                 (let ((map (make-composed-keymap nil (current-local-map))))
                                   (define-key map (kbd "M-w")
                                               (myc-make-action (ref)
                                                 (setq ref (car (split-string ref " ")))
                                                 (kill-new ref)
                                                 (message "Copy REF: %s" ref)))
                                   (use-local-map map)))
                             (let ((completion-ignore-case t))
                               (completing-read "REF: " refs nil t)))
                           " "))))
           (when (string-match "\\[\\(.+\\)\\]" ref)
             (setq ref
                   (string-replace
                    (match-string 0 ref)
                    (read-string (format "%s: " (match-string 1 ref)) nil nil (match-string 1 ref))
                    ref)))
           (browse-url ref))))))

(defreference emacs
  "Mail List: https://lists.gnu.org/archive/html/emacs-devel/"
  "Forum: https://www.reddit.com/r/emacs/"
  "Forum: https://emacs-china.org/"
  "Download: https://gnu.org/software/emacs/download.html"
  "Windows Alpha: https://alpha.gnu.org/gnu/emacs/pretest/windows/"
  "Windows Mirror: http://mirrors.ustc.edu.cn/gnu/emacs/windows/"
  "elc -> eln: http://akrl.sdf.org/gccemacs.html"
  "Config: hlissner/doom-emacs"
  "Source Repo: https://git.savannah.gnu.org/cgit/emacs.git/"
  "Melpa: https://melpa.org/#/"
  "Melpa Github: melpa/melpa")

(defreference |rrr|
  "Wallpaper: https://wallhaven.cc"
  "Wallpaper: https://pixabay.com"
  "MP3 download: https://www.mp3juices.cc/"
  "MP3 download: https://www.jbsou.cn/"
  "Game Audio: https://opengameart.org"
  "Music: music.163.com")

(defreference downloads
  "SQLite3: https://sqlite.org/download.html"
  "Graphviz: http://graphviz.org/download/")

(defreference utils
  "Net Speed: https://www.speedtest.cn"
  "Ip Checker: https://www.vps234.com/ipchecker/")

(provide 'favors)

;;; favors.el ends here
