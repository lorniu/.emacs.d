;;; -*- lexical-binding: t -*-

;;; Code:

(defcustom ic/proxy nil
  "http://auth@localhost:1081 or sock://localhost:1080 style."
  :type 'string
  :group 'imfine)

(defvar ic/proxy-lighter "  ℘  ")

(defvar url-gateway-local-host-regexp
  (concat "^" (regexp-opt `("localhost" "127.0." "192.168." "10." ,ic/host))))

(defvar im:proxy-format
  '(:propertize ic/proxy-lighter
                local-map (keymap (mode-line keymap (mouse-1 . (lambda () (interactive) (im/proxy) (force-mode-line-update)))))
                face font-lock-comment-face
                mouse-face warning
                help-echo ic/proxy))

(defun im/proxy (&optional url)
  (interactive (list (completing-read "Proxy to enable: "
                                      (delq nil (list
                                                 ic/proxy
                                                 "sock://127.0.0.1:1080"
                                                 "http://127.0.0.1:1081"
                                                 "sock://10.1.1.1:1080"
                                                 "http://10.1.1.1:1081"
                                                 (if ic/host (format "http://%s:11181" ic/host)))))))
  (cond ((= (length url) 0)
         (setq url-gateway-method 'native
               url-proxy-services nil
               url-http-proxy-basic-auth-storage nil
               socks-server nil
               ic/proxy nil)
         (message "Proxy canceled."))
        ((string-match "^\\(http\\|sock\\)s?://\\(?:\\(.*\\)@\\)?\\(.+\\):\\([0-9]+\\)$" url)
         (let* ((auth (match-string 2 url))
                (host (match-string 3 url))
                (port (match-string 4 url))
                (host+port (concat host ":" port)))
           (pcase (match-string 1 url)
             ("http"
              (setq url-gateway-method 'native
                    url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp)
                                         ("http" . ,host+port)
                                         ("https" . ,host+port))
                    url-http-proxy-basic-auth-storage (if auth (list (list host+port (cons "Proxy" (base64-encode-string auth)))))
                    socks-server nil
                    ic/proxy url)
              (message (concat "[PROXY] "
                               (propertize (concat "http://" (if auth (concat (car (split-string auth ":")) "@")) host+port) 'face 'font-lock-string-face))))
             ("sock"
              (setq url-gateway-method 'socks
                    url-proxy-services nil
                    url-http-proxy-basic-auth-storage nil
                    socks-server (list "Default server" host (string-to-number port) 5)
                    ic/proxy url)
              (message (concat "[PROXY] " (propertize (concat "socks://" host+port) 'face 'font-lock-string-face) " enabled."))))))
        (t (user-error "Proxy URL should like 'http://auth@127.0.0.1:1081' or 'sock://127.0.0.1:1080'"))))

(if ic/proxy (im/proxy ic/proxy))
