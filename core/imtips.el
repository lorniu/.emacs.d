;;; imtips.el --- Tips and Help -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (setq byte-compile-warnings '(not free-vars)))



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
- ~F1/M-h/C-ch~, Hydra Dash
- ~F12~, Toggle Views
- ~C-F12~, Change Window, etc
- ~F11~, Shell, etc
- ~F10~, Silly Scripts
- ~F2/F3/F4~, Macro, F4 event for transient-command

Normal:
- C-h s,      toggle *Scratch​*
- C-h e,      toggle *View​*
- F6,         truncate long line
- C-x i,      imenu
- C-x p,      pages
- C-x C-r,    favors

Org:
- C-c c ...
- C-c c c,    capture
- C-c c n,    publish

Fold:
- C-c f/F/CF
- C-c o/O/CO
- C-$ / C-u C-$
")

(deftips tip/demo.private-custom ()
  "
#+begin_src elisp
  (setq ic/up \"my@vps.host\")
  (setq ic/my-postgres `(\"dbname\" \"user\" \"passwd\" \"host\"))

  (setq ic/faces (list :theme 'one-dark ; 'manoj-dark;
                       :font '(\"JetBrains Mono\" . 108)
                       :font-u '(\"Source Han Sans CN\")))

  (setq ic/favorites
        '((\"xxx/\"       \"~/vvv/xxx/\" :project)))

  (setq ic/favorite-filter
        (lambda (item)
          (or (string-match-p \"mysql\" (car item)))))

  (>>init>>
   (elfeed-proxy \"socks5://127.0.0.1:1080\")
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


(provide 'imtips)

;;; imtips.el ends here
