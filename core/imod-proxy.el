;;; -*- lexical-binding: t -*-

;;; Code:

(defcustom ln-proxy nil
  "http://localhost:1081 or sock5://localhost:1080 style."
  :type 'string
  :group 'imfine)

(defvar ln-proxy-lighter "  ℘  ")

(defvar ln:proxy-format
  '(:propertize ln-proxy-lighter
                local-map (keymap (mode-line keymap (mouse-1 . (lambda () (interactive) (ln/proxy) (force-mode-line-update)))))
                face font-lock-comment-face
                mouse-face warning
                help-echo ln-proxy))

(defun ln/proxy (&optional url)
  (interactive (list (completing-read "Proxy to enable: "
                                      (delq-nil
                                       (list
                                        ln-proxy
                                        "sock5://127.0.0.1:11180"
                                        "sock5://127.0.0.1:11181"
                                        "http://127.0.0.1:11182"
                                        "http://127.0.0.1:11183"
                                        (if ln-host (format "http://%s:11182" ln-host)))))))
  (if (= (length url) 0)
      (progn
        (setq url-gateway-method 'native
              url-proxy-services nil
              url-http-proxy-basic-auth-storage nil
              socks-server nil
              ln-proxy nil)
        (message "Proxy canceled."))
    (let* ((url-obj (url-generic-parse-url url))
           (type (url-type url-obj))
           (host (url-host url-obj))
           (port (url-port url-obj)))
      (pcase (substring type 0 4)
        ("http"
         (setq url-gateway-method 'native
               url-proxy-services `(("no_proxy" . ,(concat "^" (regexp-opt `("localhost" "127.0." "192.168." "10." ,ln-host))))
                                    ("http" . ,(format "%s:%s" host port))
                                    ("https" . ,(format "%s:%s" host port)))
               socks-server nil
               ln-proxy url)
         (message (concat "[PROXY] " (propertize url 'face 'font-lock-string-face))))
        ("sock"
         (setq url-gateway-method 'socks
               url-proxy-services nil
               socks-server (list "Default server" host port 5)
               ln-proxy url)
         (message (concat "[PROXY] " (propertize url 'face 'font-lock-string-face) " enabled.")))
        (_ (user-error "Proxy URL should like 'http://auth@127.0.0.1:1081' or 'sock://127.0.0.1:1080'"))))))

(if ln-proxy (ln/proxy ln-proxy))
