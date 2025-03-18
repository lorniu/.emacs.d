;;; -*- lexical-binding: t -*-

;;; Code:

(defcustom ic/proxy nil
  "http://localhost:1081 or sock5://localhost:1080 style."
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
                                      (delq-nil
                                       (list
                                        ic/proxy
                                        "sock5://127.0.0.1:1080"
                                        "http://127.0.0.1:1081"
                                        "sock5://10.1.1.1:1080"
                                        "http://10.1.1.1:1081"
                                        (if ic/host (format "http://%s:11181" ic/host)))))))
  (if (= (length url) 0)
      (progn
        (setq url-gateway-method 'native
              url-proxy-services nil
              url-http-proxy-basic-auth-storage nil
              socks-server nil
              ic/proxy nil)
        (message "Proxy canceled."))
    (let* ((url-obj (url-generic-parse-url url))
           (type (url-type url-obj))
           (host (url-host url-obj))
           (port (url-port url-obj)))
      (pcase (substring type 0 4)
        ("http"
         (setq url-gateway-method 'native
               url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp)
                                    ("http" . ,(format "%s:%s" host port))
                                    ("https" . ,(format "%s:%s" host port)))
               socks-server nil
               ic/proxy url)
         (message (concat "[PROXY] " (propertize url 'face 'font-lock-string-face))))
        ("sock"
         (setq url-gateway-method 'socks
               url-proxy-services nil
               socks-server (list "Default server" host port 5)
               ic/proxy url)
         (message (concat "[PROXY] " (propertize url 'face 'font-lock-string-face) " enabled.")))
        (_ (user-error "Proxy URL should like 'http://auth@127.0.0.1:1081' or 'sock://127.0.0.1:1080'"))))))

(if ic/proxy (im/proxy ic/proxy))
