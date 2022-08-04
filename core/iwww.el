;;; iwww.el --- Network -*- lexical-binding: t -*-

;;; Code:

(defreference fnet
  "v2fly/v2ray-core/releases"
  "v2fly/v2ray-examples"
  "https://[youthedomain]/fnet")



(x eww
   :init
   (setq eww-bookmarks-directory (locc "./"))
   (setq eww-search-prefix "https://duckduckgo.com/html/?q=") ; https://www.google.com/search?q=
   (if IS-NG (setq browse-url-browser-function 'eww-browse-url)))

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
   :commands
   (httpd-start httpd-stop httpd-running-p httpd-serve-directory)

   :init
   (defun im/httpd-here (&optional arg)
     (interactive "P")
     (let ((root default-directory)
           (port (if arg (read-number "Port: " 5555) 5555)))
       (httpd-start :port port :root root)
       (message "http://localhost:%s | %s" port root)))

   :config
   (defservlet time text/html ()
     (insert (format "<h1>%s</h1>" (time-str)))))

(x livereload
   :commands (liveview liveload))



(x engine-mode
   :init (engine-mode 1)
   :defer-config
   (require 'format-spec)

   (defengine google         "https://google.com/search?q=%s")
   (defengine google-cn      "https://google.com/search?q=%s&lr=lang_zh-CN")
   (defengine dict-iciba     "http://www.iciba.com/%s")
   (defengine github         "https://github.com/search?ref=simplesearch&q=%s")
   (defengine stackoverflow  "http://stackoverflow.com/search?q=%s")
   (defengine iconify        "https://iconify.design/icon-sets/?query=%s")
   (defengine wikipedia      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
   (defengine arch-wiki      "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go" :browser 'eww-browse-url)
   (defengine wolfram-alpha  "http://www.wolframalpha.com/input/?i=%s")
   (defengine youtube        "http://www.youtube.com/results?aq=f&oq=&search_query=%s"))

(x sx
   "Stackoverflow. sx-authenticate / sx-tab-xxx"
   :ref ("vermiculus/sx.el"))



(defun im/wifi-nmcli (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "nmcli device wifi rescan")
  (let* ((networks (cl-remove-if-not
                    (lambda (l) (string-match-p ":" l))
                    (split-string (shell-command-to-string "nmcli device wifi list") "\n")))
         (line (completing-read "Select network: " networks nil t initial-input)))
    (if (> (length line) 0)
        (pcase-let ((`(,ssid ,name) (split-string (string-trim (string-remove-prefix "*" line)) " " t)))
          (message "Connecting to '%s' (%s)..." ssid name)
          (let ((resp (shell-command-to-string (format "nmcli device wifi connect %s" ssid))))
            (cond ((string-match-p "ERROR:.*Secrets were required" resp)
                   (let* ((pwd (read-passwd (format "password for %s: " ssid)))
                          (cmd (format "nmcli device wifi connect %s password %s" ssid pwd)))
                     (async-shell-command cmd)
                     (message "%s..." cmd)))
                  (t (message
                      "%s"
                      (replace-regexp-in-string "^.*" "" (string-trim resp)))))))
      (user-error "No connection selected"))))

(provide 'iwww)

;;; iwww.el ends here
