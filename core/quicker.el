;;; -*- lexical-binding: t -*-

;; 'quicker' used to quickly open dir, file and other resources.
;;
;;    - manager resources via 'im.quicker-list', 'im/add-to-quicker'
;;    - look for 'im.quicker-list-default' as the example of syntax
;;    - view them via 'r/quicker'
;;
;; 'defreference' and 'r/...' used to open links.
;;
;;    - define references directly with 'defreference' macro
;;    - define references for package with ':ref' keyword in x macro
;;    - common ones: 'r/emacs', 'r/links.ee'
;;
;; 'r/links.org' used to view links in links.org:
;;
;;    - two modes 1) open in org-buffer 2) completing-read links
;;    - edit direcly or use `org-capture' to manage the links in org
;;
;; 'r/links.system' used to view the bookmarks of system browser.
;;
;;    - two modes 1) open in org-buffer 2) completing-read links
;;    - support edge and chrome
;;
;; All in all, a top interface `im/transient-quicker' is provided, run it with 'C-x C-r'.

;;; Code:

(defcustom im.quicker-list nil
  "Resource files, used by `r/quicker'."
  :type '(alist :key-type string)
  :group 'imfine)

(defcustom im.org-wip (loco "WIP/")
  "Directory of WIP in notes repository."
  :type 'string
  :group 'imfine)

(defcustom im.system-links.bookmark
  (cl-find-if #'file-exists-p
              (list "~/.config/microsoft-edge/Default/Bookmarks"
                    "~/.config/microsoft-edge-beta/Default/Bookmarks"
                    "~/Library/Application Support/Microsoft Edge/Default/Bookmarks"
                    (substitute-in-file-name "$LOCALAPPDATA/Microsoft/Edge/User Data/Default/Bookmarks")))
  "Location of system browser's bookmark file."
  :type 'file
  :group 'imfine)

(defcustom im.search-templates
  '((google         "https://google.com/search?q=%s")
    (google-cn      "https://google.com/search?q=%s&lr=lang_zh-CN")
    (dict-iciba     "https://www.iciba.com/%s")
    (github         "https://github.com/search?ref=simplesearch&q=%s")
    (stackoverflow  "https://stackoverflow.com/search?q=%s")
    (wikipedia      "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
    (arch-wiki      "https://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go" :browser 'eww-browse-url)
    (iconify        "https://iconify.design/icon-sets/?query=%s")
    (wolfram-alpha  "https://www.wolframalpha.com/input/?i=%s")
    (youtube        "https://www.youtube.com/results?aq=f&oq=&search_query=%s"))
  "Url templates used to search from web."
  :type '(alist :key-type symbol)
  :group 'imfine)


;;; r/quicker (files, commands and links)

(defvar im.quicker-list-default
  `(("notes/emacs"              (loco "share/emacs/"))
    ("notes/repos"              (loco "repos/"))
    ("notes/webapp"             (loco "webapp/"))

    ("emacs.cache.dir"          (locc))

    ("conf: hosts"              #'ims/edit-hosts-file)
    ("conf: private-init.el"    custom-file)
    ("conf: notes/vv"           (loco "bin/vv"))
    ("conf: xmonad.hs"          (let ((local (loco (format "share/xmonad/xmonad-%s.hs" system-name)))
                                      (common (loce "share/xmonad/xmonad.hs")))
                                  (if (file-exists-p local) local common)))
    ("conf: pacman.conf"        "/sudo::/etc/pacman.conf"                            (file-exists-p "/etc/pacman.conf"))
    ("conf: ime/fcitx"          ime:fcitx-user-data-dir)
    ("conf: ime/rime"           ime:rime-user-data-dir)

    ("lisp: emacs/init.lisp"    (loce "share/lisp/init.lisp"))
    ("lisp: private-init.lisp"  (loco (format "share/lisp/init-%s.lisp" system-name)))
    ("lisp: quicklisp-local"    "~/.roswell/lisp/quicklisp/local-projects/")
    ("lisp: quicklisp-software" "~/.roswell/lisp/quicklisp/dists/quicklisp/software/")

    ((format "remote/ssh: %s" im.host)
     (function (lambda ()
                 (interactive)
                 (let* ((host (format "/ssh:%s" (im:host)))
                        (conn (completing-read "Tramp to: "
                                               (im:completion-table
                                                (list (concat host ":~/")
                                                      (concat host "|sudo::/")))
                                               nil nil host)))
                   (find-file conn))))
     (and im.host (executable-find "ssh")))

    ("sys: /usr/local/"         "/sudo::/usr/local/"                                 (file-directory-p "/usr/local"))
    ("sys: /etc/systemd/"       "/sudo::/etc/systemd/system/multi-user.target.wants" (file-directory-p "/etc/systemd"))
    ("sys: Win Home"            (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))    IS-WIN)

    ("util: im/proxy"           #'im/proxy)
    ("util: ims/youtube-dl-url" #'ims/youtube-dl-url)
    ("util: im/screenshot-svg"  #'im/screenshot-svg)
    ("util: im/start-localhost" #'im/start-localhost)
    ("util: aria2"              #'aria2                                              (executable-find "aria2c"))
    ("util: im/wifi-nmcli"      #'im/wifi-nmcli                                      (executable-find "nmcli")))
  "((LABEL TARGET FILTER)+)")

(defun r/quicker ()
  "View resource files."
  (interactive)
  (let* ((parsed (mapcar
                  (lambda (item)
                    (pcase-let ((`(,label ,target ,filter) item) (type) (tag))
                      (setq label (eval label))
                      (setq tag (when (string-match-p ": " label)
                                  (car (split-string label ": "))))
                      (setq type (cond
                                  ((string= tag "db") :db)
                                  ((string= tag "host") :host)
                                  ((string-match-p "/\\.\\.$" label) :project)
                                  ((equal (car-safe target) 'function) :command)))
                      (unless (member type '(:db :command))
                        (setq target (eval target)))
                      (when (and (null type) (file-remote-p target))
                        (setq type :remote))
                      (list :label label :target target :filter filter :type type :tag tag)))
                  (append im.quicker-list im.quicker-list-default)))
         (filtered (cl-remove-if-not
                    (lambda (item)
                      (cl-destructuring-bind (&key filter target type &allow-other-keys) item
                        (if filter (dlet ((it target)) (eval filter)) (or type (file-exists-p target)))))
                    parsed))
         (sorted (append
                  (cl-remove-if
                   (lambda (item) (plist-get item :tag)) filtered)
                  (cl-sort (cl-remove-if-not
                            (lambda (item) (plist-get item :tag)) filtered)
                           (lambda (x y)
                             (let ((x-tag (plist-get x :tag)) (y-tag (plist-get y :tag)))
                               (cond ((and (equal x-tag "host") (not (equal y-tag "host"))) nil) ; host at last
                                     ((and (equal y-tag "host") (not (equal x-tag "host"))) t)
                                     (t (string-lessp (plist-get x :label) (plist-get y :label)))))))))
         (propertized (mapcar
                       (lambda (item)
                         (cl-destructuring-bind (&key label target type tag &allow-other-keys) item
                           (cond ((equal type :command)
                                  (setq label (propertize label 'face '(:inherit font-lock-variable-name-face))))
                                 ((or (listp target) (symbolp target)))
                                 ((file-remote-p target)
                                  (setq label (propertize label 'face '(:weight bold))))
                                 ((file-directory-p target)
                                  (setq label (propertize label 'face '(:inherit font-lock-function-name-face)))))
                           (when tag
                             (let ((tag (concat tag ":"))
                                   (desc (string-trim (cl-subseq label (+ (length tag) 1)))))
                               (setq label
                                     (cond ((equal type :database)
                                            (format "%-6s %s" (propertize tag 'face 'font-lock-keyword-face) desc))
                                           ((equal type :host)
                                            (format "%-22s %s"
                                                    (format "%-6s %s" (propertize tag 'face dired-header-face) desc)
                                                    (propertize (concat "(" target ")") 'face dired-ignored-face)))
                                           (t (format "%-6s %s" (propertize tag 'face 'font-lock-string-face) desc))))))
                           (plist-put item :label label)))
                       sorted))
         (labels (mapcar (lambda (item) (plist-get item :label)) propertized))
         (cand (completing-read "Resources: " (im:completion-table labels) nil t nil nil (car labels)))
         (item (cl-find-if (lambda (c) (string= (plist-get c :label) cand)) propertized)))
    (unless item (user-error "Nothing to do"))
    (cl-destructuring-bind (&key target type &allow-other-keys) item
      (cond
       ((equal type :db) (sql-connect (car target)))
       ((equal type :host) (condition-case _ (browse-url target) (error (browse-web target))))
       ((equal type :command) (call-interactively (cadr target)))
       ((file-directory-p target) (let ((default-directory (file-name-as-directory target)))
                                    (call-interactively (if (equal type :project) 'project-find-file 'find-file))))
       (t (find-file target))))))

(defun im/add-to-quicker (label path &optional first-p filter)
  "Add new item to `im.quicker-list'."
  (interactive (let ((f (read-file-name "Choose File: " nil nil t)))
                 (list (file-name-nondirectory f) f)))
  (if (and (not (functionp path))
           (not (string-prefix-p "db" label))
           (not (string-prefix-p "host" label))
           (not (file-remote-p path))
           (not (file-exists-p path)))
      (user-error "Failed, file '%s' Not Found" path)
    (let ((res (list label (if (functionp path) (list 'function path) path))))
      (if filter (setq res (append res (list filter))))
      (add-to-list 'im.quicker-list res (not first-p)))
    (if (called-interactively-p 'any) (message "Resource '%s' added successfully." label) label)))


;;; r/xxx (links)

;; (defreference xxx
;;   :force :trim
;;   "r1"
;;   "d: r2"
;;   "d: r3 x"
;;   cmd
;;   ("r3" "r4")
;;   (func))

(defmacro defreference (name &rest refs)
  (declare (indent 1))
  (let ((fun (intern (concat "r/" (symbol-name name)))) flags cx-co)
    (while (keywordp (car refs))
      (if (equal (car refs) :cx-co)
          (setq cx-co (progn (pop refs) (pop refs))) ; :cx-co to supply C-x C-o function
        (push (pop refs) flags))) ; :force for not append / :trim for short url display
    (let* ((flatten (cl-loop
                     for item in refs ; flatten items like: ("r3" "r4")
                     if (and (listp item) (not (symbolp (car item)))) append (cl-loop for r in item collect r)
                     else collect item))
           (merged (if-let* ((oldrs (and (not (member :force flags)) (ignore-errors (funcall fun t)))))
                       (append oldrs flatten) flatten))) ; multiple invoke, merge all items together (if no keyword :force)
      `(defun ,fun (&optional noninteractive)
         ,(if (and (stringp (car merged)) (not (cdr merged))) "" "*") ; document string
         (interactive)
         (let* ((refs ',merged))
           (if noninteractive refs
             (let* ((refs (delq-nil ; deal item that is a form: eval and append the results
                           (cl-loop for ref in refs
                                    if (consp ref) append (ensure-list (eval ref))
                                    else collect ref)))
                    (parsed (if (null refs)  ; extract label from item, complete the abbrev links: (link-display label link)
                                (user-error "No suitable reference")
                              (mapcar (lambda (item)
                                        (if (symbolp item) item ; command
                                          (let ((link item) label group slink)
                                            (when (string-match "^\\(.+\\): +\\([^ ]+\\)\\(?: +\\([[:alnum:]]\\)\\)?$" item) ; parse "label: link :group"
                                              (setq link (match-string 2 item) label (match-string 1 item) group (match-string 3 item)))
                                            (when (not (string-prefix-p "http" link)) ; complete for github
                                              (setq link (format "https://github.com/%s" link)))
                                            (setq slink (if (member :trim ',flags) (car (split-string (car (split-string link "?")) "#")) link))
                                            (list slink label link group))))
                                      refs)))
                    (maxlen (cl-loop for i in parsed ; max length url, used to format partial display string
                                     if (car-safe (cdr-safe i))
                                     maximize (1+ (length (car i)))))
                    (groups (cl-loop for r in parsed ; groups info fetch from all items
                                     if (cadddr r) collect (cadddr r) into gs
                                     finally (return (if gs (concat "[" (string-join (delete-dups gs)) "]")))))
                    (propertized (mapcar (lambda (item) ; propertize item with face
                                           (if (symbolp item)
                                               (propertize (symbol-name item) 'face 'font-lock-function-name-face)
                                             (let ((label (cadr item)))
                                               (if (null label)
                                                   (setq label (car item))
                                                 (setq label (format (format "%%-%ds %%s" maxlen)
                                                                     (car item)
                                                                     (if (string-prefix-p "`" label) (cl-subseq label 1) (concat "(" label ")"))))
                                                 (add-face-text-property (length (car-safe item)) (length label) 'font-lock-doc-face nil label))
                                               (when-let* ((group (cadddr item)))
                                                 (setq label (propertize label 'group group)))
                                               (propertize label 'rlink (caddr item))))) ; pass real link with property, `minibuffer-allow-text-properties' should be set
                                         parsed))
                    (ref (minibuffer-with-setup-hook
                             (lambda ()
                               (let ((map (make-composed-keymap nil (current-local-map))))
                                 (define-key map (kbd "M-w")
                                             (lambda ()
                                               (interactive)
                                               (im:exit-minibuffer-with-message (ref (im:completion-compat :current))
                                                 (setq ref (car (split-string ref " ")))
                                                 (when (string-match-p "^http" ref)
                                                   (kill-new ref)
                                                   (message "Copy reference: %s" ref)))))
                                 (define-key map (kbd "C-c C-o")
                                             (lambda ()
                                               (interactive)
                                               ,(if cx-co
                                                    `(funcall ,cx-co (im:completion-compat :current))
                                                  `(message "no :open specified."))))
                                 (use-local-map map)))
                           (let ((completion-ignore-case t)
                                 (minibuffer-allow-text-properties t)
                                 (prompt (concat "reference of " ,(symbol-name name) (if groups (concat " " groups)) ": ")))
                             (completing-read prompt
                                              (lambda (input _pred action)
                                                (let (group (str input))
                                                  (when (string-match "^\\([[:alnum:]]\\) +\\(.*\\)$" input)
                                                    (let ((g (match-string 1 input)))
                                                      (when (string-match-p groups g)
                                                        (setq group g)
                                                        (setq str (match-string 2 input))
                                                        (add-face-text-property (minibuffer-prompt-end) (+ 1 (minibuffer-prompt-end)) 'warning))))
                                                  (pcase action
                                                    (`metadata `(metadata (category . quicker)
                                                                          (display-sort-function . ,#'identity)))
                                                    (`(boundaries . ,suffix)
                                                     `(boundaries . ,(cons (if group 2 0) (length suffix))))
                                                    (_ (let ((collection
                                                              (cl-loop for r in propertized
                                                                       if (or (null group) (string= group (get-text-property 1 'group r)))
                                                                       collect r)))
                                                         (complete-with-action action collection str nil))))))
                                              nil t)))))
               (if (string-match-p (concat "^\\(?:" groups " \\)?http") ref)
                   (let ((link (get-text-property 3 'rlink ref))) ; link
                     (message (concat "link " (propertize link 'face 'font-lock-doc-face) " opened in browser."))
                     (browse-url link))
                 (call-interactively (intern ref))))))))))

(defun r:group (char &rest items)
  (declare (indent 1))
  (cl-loop for i in items collect (format "%s %c" i char)))

(defreference emacs
  "Mail List:   https://lists.gnu.org/archive/html/"
  "Source:      https://git.savannah.gnu.org/cgit/emacs.git/"
  "Source:      https://github.com/emacs-mirror/emacs"
  "Forum:       https://www.reddit.com/r/emacs/"
  "Forum:       https://emacs-china.org/"
  "Download:    https://gnu.org/software/emacs/download.html"
  "Config:      https://github.com/doomemacs/doomemacs"
  "Melpa:       https://melpa.org/#/"
  "Repo/Melpa:  https://github.com/melpa/melpa"
  "Topic-Org-Link:   https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/"
  "Topic-FaceAttrs:  https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html"
  "Topic-Propertize: https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html"
  "Topic-Overlay:    https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html"
  "Topic-Customize:  https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Types.html"
  "Windows Alpha:    https://alpha.gnu.org/gnu/emacs/pretest/windows/"
  "Windows Mirror:   https://mirrors.ustc.edu.cn/gnu/emacs/windows/")


;;; r/links.ee (links)

(defreference links.ee
  "Arch: https://archive.archlinux.org/"
  (r:group ?s
    "Draw:            https://excalidraw.com"
    "Draw:            https://www.processon.com"
    "Draw:            https://draw.io"
    "Photo:           https://photopea.com"
    "Color:           https://palettemaker.com"
    "Color:           https://flatuicolors.com"
    "Color:           https://colorhunt.co/palettes/popular"
    "FileConvert:     https://convertio.co/zh/"
    "Util:            https://www.speedtest.cn"
    "Util:            https://www.vps234.com/ipchecker/"
    "Repo/DrawIO:     https://github.com/jgraph/drawio")
  (r:group ?r
    "Audio:           https://opengameart.org"
    "MP3:             https://www.mp3juices.cc"
    "MP3:             https://www.jbsou.cn"
    "Wallpaper:       https://wallhaven.cc"
    "Wallpaper:       https://pixabay.com"
    "Wallpaper:       https://wallpaperaccess.com"
    "UI/UX:           https://www.ls.graphics"
    "Icon:            https://icon-sets.iconify.design"
    "Icon:            https://fontawesome.com/icons?d=gallery"
    "Icon:            http://www.logobook.com")
  (r:group ?d
    "SQLite3:         https://sqlite.org/download.html"
    "Graphviz:        https://graphviz.org/download/"
    "DrawIO:          https://github.com/jgraph/drawio-desktop/releases"
    "BestTrace:       https://www.ipip.net/product/client.html"
    "Media/ffmpeg:    https://ffmpeg.org/download.html"
    "Media/MPV:       https://mpv.io/installation"
    "Media/yt-dlp:    https://github.com/yt-dlp/yt-dlp/wiki/Installation"
    "Media/BBDown:    https://github.com/nilaoda/BBDown")
  (when IS-WIN
    (r:group ?d
      "Aria2:                https://github.com/aria2/aria2/releases"
      "Snipaste:             https://www.snipaste.com/download.html"
      "ShareX:               https://getsharex.com/downloads"
      "ScreenToGif:          https://screentogif.com"
      "oCam:                 https://www.aichunjing.com/soft/2018-10-28/423.html"
      "Listary:              https://www.listary.com/download"
      "Everywhere:           https://www.voidtools.com/downloads"
      "RipGrep/rg:           https://github.com/BurntSushi/ripgrep/releases"
      "Balsamiq Mockups:     https://juejin.cn/post/6943480321979613198"
      "XMind:                https://xmind.cn/"
      "SumatraPDF:           https://www.sumatrapdfreader.org/download-free-pdf-viewer"
      "V2Ray:                https://github.com/v2fly/v2ray-core/releases"
      "PowerToys:            https://learn.microsoft.com/en-us/windows/powertoys/install"
      "GeekUninstaller:      https://geekuninstaller.com/download"
      "WizTree/DiskAnalyzer: https://diskanalyzer.com/download")))


;;; r/links.org (links)

(defreference links.org
  :force :trim
  :cx-co (lambda (item)
           (im:exit-minibuffer-with-message nil
             (setq item (car (split-string item)))
             (pop-to-buffer (find-file-noselect org-default-links-file))
             (goto-char (point-min))
             (search-forward item nil t)
             (org-show-entry)))
  (with-current-buffer (find-file-noselect org-default-links-file)
    (org-element-map (org-element-parse-buffer) 'link
                     (lambda (link)
                       (let (hls title (p (org-element-lineage link '(headline))))
                         (while p
                           (push p hls)
                           (setq p (org-element-lineage p '(headline))))
                         (setq title (mapconcat (lambda (h) (org-element-property :raw-value h)) hls " | "))
                         (when title
                           (format "`* %s: %s" title (org-element-property :raw-link link))))))))

(defun r/links.org-1 ()
  "Prompt and show the `org-default-links-file' buffer."
  (interactive)
  (unless (file-exists-p org-default-links-file)
    (user-error "org-default-links-file (%s) not exists" org-default-links-file))
  (require 'org)
  (let ((headline (im:org-completing-read-headline org-default-links-file)))
    (pop-to-buffer (find-file-noselect org-default-links-file))
    (when headline
      (goto-char (point-min))
      (re-search-forward (format org-complex-heading-regexp-format (regexp-quote headline)) nil t)
      (beginning-of-line 2)
      (im:org-show-plain-text-and-children-headlines))))


;;; r/links.system (links)

(defreference links.system
  :force :trim
  :cx-co (lambda (item)
           (setq item (car (split-string item)))
           (im:exit-minibuffer-with-message nil
             (with-current-buffer (find-file im.system-links.bookmark)
               (goto-char (point-min))
               (search-forward item nil t)
               (back-to-indentation))))
  (with-current-buffer (r/view-system-links-as-org)
    (org-element-map (org-element-parse-buffer) 'link
                     (lambda (link)
                       (let ((title (car (org-element-property :title (org-element-lineage link '(headline)))))
                             (path (org-element-property :raw-link link)))
                         (when (and (stringp title) (< (length path) 100))
                           (format "`(%s) %s: %s" title (caddr link) path)))))))

(defun r/links.system-1 ()
  "Prompt and show bookmarks of the Browser."
  (interactive)
  (with-current-buffer (r/view-system-links-as-org)
    (when-let* ((headline (im:org-completing-read-headline)))
      (goto-char (point-min))
      (re-search-forward (format org-complex-heading-regexp-format (regexp-quote headline)) nil t)
      (beginning-of-line 2)
      (outline-show-subtree)
      (pop-to-buffer (current-buffer)))))

(defun r/view-system-links-as-org ()
  "Sync bookmark of system browser to org buffer."
  (interactive)
  (unless (and im.system-links.bookmark (file-readable-p im.system-links.bookmark))
    (user-error "Bookmark file not found. Correct `im.system-links.bookmark' first"))
  (require 'org)
  (let ((data (let ((json-object-type 'alist)
                    (json-array-type  'list)
                    (json-key-type    'symbol)
                    (json-false       nil)
                    (json-null        nil))
                (json-read-file im.system-links.bookmark))))
    (with-current-buffer (get-buffer-create (format "*Bookmarks (%s)*" (if (string-match-p "edge" im.system-links.bookmark) "Edge" "Chrome")))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cl-labels ((fn (al level)
                      (pcase (alist-get 'type al)
                        ("folder"
                         (insert (format "%s %s\n" (make-string level ?*) (alist-get 'name al)))
                         (mapc (lambda (item) (fn item (+ level 1)))  (alist-get 'children al)))
                        ("url"
                         (insert (format "- %s\n" (org-make-link-string (alist-get 'url al) (alist-get 'name al))))))))
          (fn (alist-get 'bookmark_bar (alist-get 'roots data)) 1)
          (fn (alist-get 'other (alist-get 'roots data)) 1))
        (org-mode)
        (set-buffer-modified-p nil)
        (view-mode 1))
      (goto-char (point-min))
      (if (called-interactively-p 'any)
          (pop-to-buffer (current-buffer))
        (current-buffer)))))


;;; r/search-xxx (Search Web)

(defun r:define-search-function (tag url)
  (defalias (intern (format "r/search-%s" tag))
    `(lambda (term)
       ,(format "Search %s for selected text." (capitalize (symbol-name tag)))
       (interactive
        (list (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (let ((current (thing-at-point 'symbol 'no-properties)))
                  (read-string (concat (format "Search %s" (capitalize (symbol-name ',tag)))
                                       (if current (format " (%s)" current))
                                       ": ")
                               nil nil (or current ""))))))
       (browse-url (format ,url (url-hexify-string term))))))

(cl-loop for (tag url) in im.search-templates do (r:define-search-function tag url))


;;; Interface, C-x C-r

(require 'transient)

(defun r:find-file (file)
  (if (and file (file-exists-p file)) (find-file file)
    (user-error "File or Directory not found")))

(defun r:read-file (dir &optional cmd sort)
  (unless (and dir (file-exists-p dir))
    (user-error "Directory not found"))
  (let ((vertico-sort-function (if sort (if (equal sort t) #'vertico-sort-history-length-alpha sort)))
        (default-directory dir))
    (call-interactively (or cmd #'dired))))

(transient-define-prefix im/transient-quicker ()
  :transient-non-suffix 'transient--do-exit
  [:hide
   (lambda () t)
   ("R  "    "a"            r/quicker)
   ("C-r"    "b"            r/quicker)
   ("A  "    "h"            im/org-agenda-lines)
   ("i  "    "d"            (lambda () (interactive) (r:read-file im.org-wip)))
   ("C-a"    "i"            (lambda () (interactive) (r:find-file agenda-directory)))
   ("C-n"    "j"            (lambda () (interactive) (r:find-file (loco ""))))
   ("C-e"    "k"            (lambda () (interactive) (r:find-file (loce ""))))
   ("C-s"    "l"            (lambda () (interactive) (r:find-file im.srcdir)))
   ("C-w"    "n"            (lambda () (interactive) (r:find-file im.workdir)))
   ("C-d"    "p"            (lambda () (interactive) (r:find-file im.downdir)))
   ("O  "    "r"            im/open-directory-externally)
   ("C-o"    "s"            im/open-directory-externally)
   ("l C-l"  "u"            r/links.org-1)
   ("l C-s"  "v"            r/links.system-1)
   ("N"      "y"            (lambda () (interactive) (r:find-file "/db:/Notes/")))]
  [[("r"   "Quicker.."      r/quicker)
    ("p"   "Project.."      project-switch-project)
    ("/"   "Search Web.."   im/transient-retrieve)]
   [("l e" "links.ee"       r/links.ee)
    ("l l" "links.org"      r/links.org)
    ("l s" "links.system"   r/links.system)]
   [("e"   "Emacs.."        (lambda () (interactive) (r:read-file (loce "") #'project-find-file)))
    ("n"   "Notes.."        (lambda () (interactive) (r:read-file (loco "") #'project-find-file)))
    ("a"   "Agenda.."       im/org-agenda-files)]
   [("w"   "Workdir.."      (lambda () (interactive) (r:read-file im.workdir)))
    ("s"   "Source.."       (lambda () (interactive) (r:read-file im.srcdir nil #'vertico-sort-alpha)))
    ("d"   "Download.."     (lambda () (interactive) (r:read-file im.downdir)))]
   [("o"   "Open.."         im/open-externally)
    ("f"   "Find.."         find-dired)
    ("t"   "Trashed"        trashed)]])
