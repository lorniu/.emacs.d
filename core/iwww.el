;;; -*- lexical-binding: t -*-

;;; Code:

(defreference fnet
  "v2fly/v2ray-core/releases"
  "v2fly/v2ray-examples"
  "https://[youthedomain]/fnet")

(setq request-storage-directory (locc "request"))



(x plz
   :ref ("alphapapa/plz.el"))

(x aria2
   "aria2c client in emacs."
   :ref ("repo/download: aria2/aria2"
         "usage: https://aria2c.com/usage.html"
         "aria2.el: https://gitlab.com/ukaszg/aria2")
   :commands (aria2 aria2-add-uris aria2-add-file))



(x eww
   :config
   (setopt eww-bookmarks-directory (locc "./")
           ;; https://www.google.com/search?q=
           eww-search-prefix "https://duckduckgo.com/html/?q=")
   (if IS-NG (setopt browse-url-browser-function 'eww-browse-url)))

(x restclient
   :ref "pashky/restclient.el")

(x httprepl
   :ref ("gregsexton/httprepl.el")
   :init
   (defalias 'restclient 'httprepl))



(eval-when-compile (require 'simple-httpd))

(x simple-httpd/+
   "Start local server with port 5555:
   "
   " - M-x im/http-here
   "
   "Define your own servlet:
   "
   "  (defservlet time text/html () (insert (format \"%s\" (time-str))))
   "
   "Then you can visit with 'http://host:5555/time'
   "
   :commands (httpd-start httpd-stop httpd-running-p httpd-serve-directory)
   :config
   (defservlet time text/html ()
     (insert (format "<h1>%s</h1>" (time-str)))))



(x engine-mode
   :init (engine-mode 1)
   :config
   (require 'format-spec)
   (define-key engine-mode-map (kbd "C-x /") nil)

   (defengine google         "https://google.com/search?q=%s")
   (defengine google-cn      "https://google.com/search?q=%s&lr=lang_zh-CN")
   (defengine dict-iciba     "https://www.iciba.com/%s")
   (defengine github         "https://github.com/search?ref=simplesearch&q=%s")
   (defengine stackoverflow  "https://stackoverflow.com/search?q=%s")
   (defengine wikipedia      "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
   (defengine arch-wiki      "https://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go" :browser 'eww-browse-url)
   (defengine iconify        "https://iconify.design/icon-sets/?query=%s")
   (defengine wolfram-alpha  "https://www.wolframalpha.com/input/?i=%s")
   (defengine youtube        "https://www.youtube.com/results?aq=f&oq=&search_query=%s"))



(defun im/httpd-here (&optional arg)
  (interactive "P")
  (let ((httpd-root default-directory)
        (httpd-port (if arg (read-number "Port: " 5555) 5555)))
    (httpd-start)
    (message "http://localhost:%s | %s" httpd-port httpd-root)))

(defun im/wifi-nmcli (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "(set -x; nmcli device wifi rescan)")
  (let* ((networks (cl-remove-if-not
                    (lambda (l) (string-match-p ":" l))
                    (split-string (shell-command-to-string "(set -x; nmcli device wifi list)") "\n")))
         (line (completing-read "Select network: " networks nil t initial-input)))
    (if (> (length line) 0)
        (pcase-let ((`(,ssid ,name) (split-string (string-trim (string-remove-prefix "*" line)) " " t)))
          (message "Connecting to '%s' (%s)..." ssid name)
          (let ((resp (shell-command-to-string (format "(set -x; nmcli device wifi connect %s)" ssid))))
            (cond ((string-match-p "ERROR:.*Secrets were required" resp)
                   (let* ((pwd (read-passwd (format "password for %s: " ssid)))
                          (cmd (format "(set -x; nmcli device wifi connect %s password %s)" ssid pwd)))
                     (async-shell-command cmd)
                     (message "%s..." cmd)))
                  (t (message
                      "%s"
                      (replace-regexp-in-string "^.*" "" (string-trim resp)))))))
      (user-error "No connection selected"))))

(defun im/network-ipinfo-query (ip)
  "Return ip info from ipinfo.io for IP."
  (interactive "sEnter IP to query (blank for own IP): ")
  (require 'plz)
  (plz 'get (concat "https://ipinfo.io/" ip)
    :headers '(("User-Agent" . "Emacs ipinfo.io Client")
               ("Accept" . "application/json")
               ("Content-Type" . "application/json;charset=utf-8"))
    :as 'json-read
    :then (lambda (data)
            (message
             (mapconcat
              (lambda (e)
                (format "%10s: %s" (capitalize (symbol-name (car e))) (cdr e)))
              data "\n")))
    :else (lambda (err)
            (message "Can't receive ipinfo. Error %S " arg))))

(defun im/network-dns-query (&optional host)
  (interactive (list (read-string "Host name: " nil 'im:network-dns-history)))
  (if (< (length host) 5) (user-error "Not valid host name: %s" (or host nil)))
  (if-let* ((ip (dns-query host)))
      (progn (kill-new ip) (message "%s" ip))
    (user-error "No available ip found for %s" host)))

(defun im/network-local-ip-address (&optional dev)
  (interactive (list (completing-read "Dev: " (delete-dups (mapcar #'car (network-interface-list))) nil t)))
  (let* ((info (network-interface-info dev))
         (ip (format-network-address (car info) t)))
    (kill-new ip)
    (message (concat "Local IP Address (" dev "): " (propertize ip 'face 'font-lock-keyword-face)))))

(defvar ic/local-servers
  '(liveload
    ("python" "-m http.server --directory %s -b 0.0.0.0 8000" "pacman -S python")
    ("ruby" "-run -e httpd %s -b 0.0.0.0 -p 8080" "pacman -S ruby && gem install webrick")
    ("http-server" "%s -a 0.0.0.0 -p 8080" "pacman -S nodejs && npm install http-server -g")
    ("LiveReloadServer" "--WebRoot %s --Host 0.0.0.0 --Port 5500" "dotnet tool install -g LiveReloadServer"))
  "Element: command or (program params error-message)")

(defun im/start-localhost ()
  "Run a local static server interactively."
  (interactive)
  (let* ((getval (lambda (item)
                   (or (assoc item ic/local-servers) (intern item))))
         (dispfn (lambda (item)
                   (let ((v (funcall getval item)))
                     (concat (make-string (- 30 (length item)) ? )
                             (if (or (symbolp v) (executable-find (car v))) "✓" (caddr v))))))
         (sortfn (lambda (items)
                   (cl-loop for item in items for val = (funcall getval item)
                            if (symbolp val) collect item into cmds
                            else if (executable-find (car val)) collect item into availables
                            else collect item into nonavaiables
                            finally (return (append cmds availables nonavaiables)))))
         (choosen (funcall getval
                           (completing-read "Run server of: "
                                            (lambda (input pred action)
                                              (if (eq action 'metadata)
                                                  `(metadata (annotation-function . ,dispfn)
                                                             (display-sort-function . ,sortfn))
                                                (complete-with-action action
                                                                      (cl-loop for s in ic/local-servers
                                                                               if (symbolp s) collect (symbol-name s)
                                                                               else collect (car s))
                                                                      input pred)))
                                            nil t))))
    (if (and (listp choosen) (not (executable-find (car choosen))))
        (message (concat "Server '" (car choosen) "' unavailable, you should: "
                         (propertize (caddr choosen) 'face 'font-lock-string-face)))
      (let ((dir (read-directory-name "Root directory: ")))
        ;; execute the command
        (if (symbolp choosen)
            (let ((default-directory dir)) (funcall choosen))
          ;; normalize path for special cases
          (if (memq system-type '(cygwin windows-nt ms-dos)) (setq dir (file-truename dir)))
          (if (string-match-p " " dir) (setq dir (concat "'" dir "'")))
          ;; prompt and run command to start server
          (let ((cmd (read-string "Command to run: " (format "%s %s" (car choosen) (format (cadr choosen) dir))))
                (buf (format "*webserver:%s*" (car choosen))))
            (if (zerop (length cmd)) (user-error "Command is invalid")
              (async-shell-command cmd buf buf))))))))

(defun im/network-interface ()
  (interactive)
  (im:run-choose-command
   "Network command to execute: "
   'ping 'ifconfig 'iwconfig 'dig 'nslookup 'nslookup-host 'dns-lookup-host 'whois
   'arp 'netstat 'smbclient 'smbclient-list-shares 'route 'traceroute 'finger 'network-connection
   'im/proxy 'im/network-ipinfo-query 'im/network-dns-query 'im/network-local-ip-address 'im/start-localhost))
