;;; itop.el --- Topics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ic/favorites nil
  "Favorite files, used by `im/view-favorites'."
  :type '(alist :key-type string))

(defcustom ic/favorite-filter nil
  "Extra filter. A function to filter current ITEM."
  :type 'string)

(defcustom ic/ideas-dir (let ((d (loco "x.ideas/"))) (and (file-exists-p d) d))
  "Directory to save files for todo ideas."
  :type 'string)


;;; backup and auto-save

(setq create-lockfiles nil) ;; .#lock-file

(setq backup-inhibited nil  ;; version backup file~~
      backup-by-copying t
      version-control t kept-new-versions 4 kept-old-versions 2 delete-old-versions t
      backup-directory-alist `(("\\.cache\\|\\.git\\|\\.ssh")
                               ("." . ,(locc "backup-files/"))))

(setq auto-save-default t   ;; save #periodically# to avoid crashes
      auto-save-interval 300 auto-save-timeout 30
      auto-save-file-name-transforms `((".*" ,(locc "auto-save/") t))
      auto-save-list-file-prefix (locc "auto-save/"))


;;; Favorites

(defvar favorites-default
  '(("notes/"                   org-directory  :project)
    ("emacs/"                   (loce "core/") :project)
    ("emacs.cache/"             (locc))
    ("emacs.share/"             (loco "x.share/emacs/"))
    ("conf: emacs.private-init" custom-file)
    ("conf: xmonad.hs"          (loce "share/xmonad/xmonad.hs"))

    ("host: Dropbox"            "https://dropbox.com")
    ("host: MvnRepository"      "https://mvnrepository.com")

    ((format "ssh: %s" (imup))  (format "/ssh:%s:~/" (imup))                         (imup))

    ("sys: /usr/local/"         "/sudo::/usr/local/"                                 IS-BSD)
    ("sys: /etc/systemd/"       "/sudo::/etc/systemd/system/multi-user.target.wants" (file-directory-p "/etc/systemd"))
    ("sys: pacman.conf"         "/sudo::/etc/pacman.conf"                            (file-exists-p "/etc/pacman.conf"))
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
    (let* ((selectrum-should-sort nil)
           (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                       (define-key map (kbd "TAB") 'selectrum-select-current-candidate)
                                       map))
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
               (call-interactively 'im/project-find-dir)))
            ((file-directory-p path)
             (let ((default-directory path))
               (call-interactively 'dired)))

            ((memq :readonly modes) (im/open-file-view path))
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


;;; Idea Files

(defun im/find-file-in-ideas-dir ()
  "Make `ic/ideas-dir' for temporay files that catch my thoughts."
  (interactive)
  (unless ic/ideas-dir
    (user-error "Custom `ic/idea-dir' first."))
  (let ((default-directory ic/ideas-dir))
    (call-interactively 'find-file)))


;;; References

(defmacro defreference (name &rest refs)
  "Make a function of NAME to browse the REFS."
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
                               collect label))
         (fun (intern (format (concat (unless (string-match-p "/" (symbol-name name)) "ref/") (if (cdr flatten) "%s*" "%s")) name))))
    `(defun ,fun ()
       ,(format "%s\n\nBrowser/Yank it." propertized)
       (interactive)
       (let* ((refs ',propertized)
              (selectrum-should-sort nil)
              (selectrum-minibuffer-map (let ((map (copy-keymap selectrum-minibuffer-map)))
                                          (define-key map (kbd "M-w")
                                            (selectrum-make-action (ref)
                                              (setq ref (car (split-string ref " ")))
                                              (kill-new ref)
                                              (message "Copy REF: %s" ref)))
                                          map))
              (ref (car (split-string (completing-read "REF: " refs nil t) " "))))
         (browse-url ref)))))

(defreference emacs
  ("Forum: https://www.reddit.com/r/emacs/"
   "Forum: https://emacs-china.org/"
   "Download: https://gnu.org/software/emacs/download.html"
   "Windows Alpha: https://alpha.gnu.org/gnu/emacs/pretest/windows/"
   "Windows Mirror: http://mirrors.ustc.edu.cn/gnu/emacs/windows/"
   "elc -> eln: http://akrl.sdf.org/gccemacs.html"
   "Config: hlissner/doom-emacs"
   "Source Repo: https://git.savannah.gnu.org/cgit/emacs.git/"
   "OrgMode Updates: https://updates.orgmode.org/"
   "OrgMode Maillist: https://lists.gnu.org/mailman/listinfo/emacs-orgmode"))

(defreference downloads
  ("SQLite3: https://sqlite.org/download.html"
   "Graphviz: http://graphviz.org/download/"))


;;; Tips

(defmacro deftips (name args format-string &rest str-args)
  "Open an org buffer to show the information.
Plist ARGS can be :buffer/line/pre/post/startup/title/notitle."
  `(defun ,name ()
     (interactive)
     (let ((bn ,(or (plist-get args :buffer) "*Help-Tip*")))
       (with-output-to-temp-buffer bn
         (with-current-buffer bn
           (let ((inhibit-read-only t))
             ,(aif (plist-get args :pre) `(insert ,(format "%s\n" it)))
             (insert ,(cl-loop for startup in (aif (plist-get args :startup) (if (listp it) it (list it)) nil)
                               concat (format "#+STARTUP: %s\n" startup)))
             ,(unless (plist-get args :notitle)
                `(insert ,(format "#+TITLE: %s\n\n" (or (plist-get args :title) name))))
             (insert
              ,(if (null str-args) format-string
                 (apply #'format format-string
                        (cl-loop for sa in str-args
                                 for s = (eval sa)
                                 collect (if (string-match "^\\(.*\\) \\(-+\\)$" s)
                                             (concat (match-string 1 s)
                                                     (cl-loop for i below (length (match-string 2 s)) concat "\n"))
                                           s)))))
             ,(aif (plist-get args :post) `(progn ,it))
             (set-buffer-modified-p nil)
             (goto-char (point-min))
             (forward-line ,(or (plist-get args :line) 4)))
           (org-mode)
           (view-mode 1)
           (make-local-variable 'view-mode-map)
           (define-key view-mode-map "q" 'View-kill-and-leave)
           (pop-to-buffer bn))))))

(deftips tip/emacs ()
  "
* Portable

- Download and unzip Emacs as ~$udisk/app/emacs~ (ref/emacs*).
- Copy the whole .emacs.d as ~$udisk/home/.emacs.d~.
- A batch file as launcher ~$udisk/app/emacs/runemacs.bat~:
  #+begin_src bat
    @echo off

    set HOME=%~dp0..\\..\\home
    \"%~dp0\\bin\\runemacs.exe\" %*
  #+end_src
- [Opt] execute =(s/windows-generate-extra-config-for-emacs)= for extra config.

")

(deftips tip/choco-install (:startup showeverything)
  "
* Install under Powershell

#+begin_src powershell
[environment]::SetEnvironmentvariable('ChocolateyInstall', 'C:\\Choco', 'Machine')
Set-ExecutionPolicy Bypass -Scope Process -Force; Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
#+end_src

* Basic setup for proxy

#+begin_src powershell
choco config set proxy http://127.0.0.1:8118
choco config set proxyBypassOnLocal true
#+end_src")

(deftips tip/keybinds ()
  "
Important:
- C-x i, Goto
- C-c i, Act
- C-c c, Gtd
- C-c p, Tpl
- C-c f, fold
- C-c m, Mode Act
- C-,  , Expand Region
- C-.  , Interactive current point
- F2/F3/F4, Macro

Normal:
- C-h s,      toggle *Scratch​*
- C-h e,      toggle *EShell*
- C-h w,      toggle *Messages*
- F6,         truncate long line
- C-x C-r,    favors
")

(deftips tip/demo.private-custom ()
  "
#+begin_src elisp
  (setq ic/up \"my@vps.host\")
  (setq ic/my-postgres `(\"dbname\" \"user\" \"passwd\" \"host\"))

  (setq ic/faces (list :theme 'one-dark ; 'manoj-dark;
                       :font '(\"JetBrains Mono\" . 108)
                       :font-unicode '(\"Source Han Sans CN\")))

  (setq ic/favorites
        '((\"xxx/\"       \"~/vvv/xxx/\" :project)))

  (setq ic/favorite-filter
        (lambda (item)
          (or (string-match-p \"mysql\" (car item)))))

  (>>init>>
   (add-to-list 'org-agenda-files \"/home/vip/vvv/dotnet.org\")
   )
#+end_src
")

(deftips tip/demo.emacs.service ()
  "
#+begin_src conf :file /usr/lib/systemd/system/emacs.service
[Unit]
Description=Emacs daemon service

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval \"(kill-emacs)\"
Environment=XMODIFIERS=@im=fcitx
Environment=GTK_IM_MODULE=fcitx
Restart=always

[Install]
WantedBy=default.target
#+end_src
")

(deftips tip/java ()
  "
* Babel of Java

#+BEGIN_SRC java :cmdline \"-cp .\" :cmpflag \"-cp .\" :classname Main :dir \"~/.cache\"
  class Main {
      public static void main(String[] args) {
          System.out.println(\"hello, world\");
      }
  }
#+END_SRC

Set the default arguments for Java Babel:
#+BEGIN_SRC elisp
  (setq org-babel-default-header-args:java
        '((:cmpflag . \"-cp .:my.jar:your.jar\")
          (:cmdline . \"-cp .:my.jar:your.jar\")
          (:dir . \"~/.cache/javacache\")))
#+END_SRC

* jdecomp

Read uncompressed x.class or classes in x.jar directly.

It uses idea's 'share/fernflower.jar' to do the uncompression.
")

(deftips tip/sql ()
  "
* Modes

Modes to connect DB:
- sql-xxx :: Common Plan, need client program installed.
- SQLPLUS :: Just for Oracle.
- pg library :: For Postgres, by tcp, it's very convenience.

You can custom `sql-connection-alist' for `sql-connect' command.

* Sql-Babel
:PROPERTIES:
:header-args:sql+: :engine mysql
:header-args:sql+: :dbuser root :dbpassword root
:header-args:sql+: :dbhost localhost :database mysql
:END:

#+BEGIN_SRC sql
  select * from user;
#+END_SRC

Or, you can set DB globally:
#+begin_src elisp
  (setq org-babel-default-header-args:sql
        '((:engine . \"mssql\")
          (:dbuser . \"sa\")
          (:dbpassword . \"sa\")
          (:dbhost . \"localhost\")
          (:database . \"xxx\")))
#+end_src
")

(deftips tip/restclient ()
  "
* Restclient
** Basic

#+begin_src restclient
  GET https://api.github.com
  DELETE https://jira.atlassian.com/rest/api/2/version/20
#+end_src

** With Headers

#+begin_src restclient
  GET https://api.github.com
  User-Agent: Emacs Restclient
  Accept-Encoding: compress, gzip
  Content-type: text/plain
  Cookie: name=restclient
#+end_src

** Post with data

#+begin_src restclient
  POST https://jira.atlassian.com/rest/api/2/search
  Content-Type: application/json

  {
      \"jql\": \"project = HCPUB\",
      \"startAt\": 0,
      \"maxResults\": 15
  }
#+end_src

** File upload

#+begin_src restclient
  POST http://httpbin.org/post
  Content-type: text/plain

  < /etc/passwd
#+end_src

* variable (:xxx)
:PROPERTIES:
:header-args:restclient: :var s=\"http://127.0.0.1/index.php?s\"
:END:

#+begin_src restclient
  :path = xxx
  :headers = <<
  Content-type: text/plain
  Accept-Encoding: compress, gzip
  #

  GET :s/:path
  :headers
#+end_src

#+begin_src restclient
  :example-auth = (format \"Basic %s\" (base64-encode-string (format \"%s:%s\" \"user\" \"password\")))

  GET http://httpbin.org/basic-auth/user/password
  Authorization: :example-auth
#+end_src

* Hooks and JQ Expression

#+begin_src restclient
  GET http://httpbin.org/response-headers?Content-Type=application/vnd.whatever%2Bjson;%20charset=UTF-8
  -> on-response (message \"dynamic hook called %s %s\" (random) (random))
  -> on-response (message \"another hook\")
#+end_src

#+begin_src restclient
  GET http://httpbin.org/ip
  -> jq-set-var :my-ip .origin
#+end_src

#+begin_src restclient
  :user = jack
  :password = secret
  :test-me := (format \"%s:%s\" \":user\" \":password\")
  :auth-token :=  (format \"Basic %s\" (base64-encode-string (restclient-get-var \":test-me\")))

  GET http://httpbin.org/basic-auth/jack/secret
  Authorization: :auth-token
  -> run-hook (message (format \"authenticated with '%s'\" (restclient-get-var \":auth-token\")))
#+end_src
")

(deftips tip/vcs-like-git-and-so-on (:startup showeverything)
  "
* Quick Tricks

- vc-annotate vs magit-blame
- vc-region-history
- C-x v l
- vc-resolve-conflicts

* Magit So Slow

#+begin_src sh
  git config --global core.preloadindex true   # default since v2.1
  git config --global core.fscache true        # default since v2.8
  git config --global gc.auto 256
#+end_src

Ref:
- [[https://magit.vc/manual/magit/Microsoft-Windows-Performance.html]]

")

(deftips tip/roam (:startup showall)
  "
* Capture Web Content

Add this to Browser's bookmark:
#+begin_src js
  // capture selection
  javascript:location.href = 'org-protocol://roam-ref?template=a&ref=' + encodeURIComponent(location.href) + '&title='+encodeURIComponent(document.title) + '&body='+encodeURIComponent(function(){var html = '';var sel = window.getSelection();if (sel.rangeCount) {var container = document.createElement('div');for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());}html = container.innerHTML;}var dataDom = document.createElement('div');dataDom.innerHTML = html;['p', 'h1', 'h2', 'h3', 'h4'].forEach(function(tag, idx){dataDom.querySelectorAll(tag).forEach(function(item, index) {var content = item.innerHTML.trim();if (content.length > 0) {item.innerHTML = content + '&#13;&#10;';}});});return dataDom.innerText.trim();}())

  // only capture URL
  javascript:location.href='org-protocol://store-link?url='+encodeURIComponent(location.href)
#+end_src

Then save it with this style:
#+begin_src elisp
 (add-to-list 'org-roam-capture-ref-templates
              '(\"a\" \"Annotation\" plain (function org-roam-capture--get-point)
                \"%U ${body}\\n\"
                :file-name \"${slug}\"
                :head \"#+title: ${title}\\n#+roam_key: ${ref}\\n#+roam_alias:\\n\\n[[抓取内容]]\\n\\n\"
                :immediate-finish t
                :unnarrowed t))
#+end_src

* Org Protocol Setup

Linux:
#+begin_src sh
  # .desktop
  cat < ~/.local/share/applications/org-protocol.desktop <<EOF
[Desktop Entry]
Name=Org-Protocol
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/org-protocol
EOF

  # active org-protocol://
  xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol

  # make chrome not show 'confirm'
  sudo mkdir -p /etc/opt/chrome/policies/managed/
  sudo tee /etc/opt/chrome/policies/managed/external_protocol_dialog.json >/dev/null <<'EOF'
{
  \"ExternalProtocolDialogShowAlwaysOpenCheckbox\": true
}
EOF
  sudo chmod 644 /etc/opt/chrome/policies/managed/external_protocol_dialog.json
#+end_src

Windows:
#+begin_src elisp
  ;; generate and run the reg file
  (s/windows-generate-extra-config-for-emacs)
#+end_src
")


(provide 'itop)

;;; itop.el ends here
