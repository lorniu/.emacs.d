;;; cust-proxy.el --- Proxy -*- lexical-binding: t -*-

;;; Code:

(defcustom ic/proxy-type nil
  "Which proxy to use, :http or :sock."
  :type 'keyword)

(defcustom ic/proxy-sock '("Default server" "127.0.0.1" 1080 5)
  "Socket proxy default value."
  :type 'list)

(defcustom ic/proxy-http '("127.0.0.1:1081" nil nil)
  "Http proxy default value: (url user password)"
  :type 'string )

(setq url-gateway-local-host-regexp
      (concat "^"
              (regexp-opt
               '("localhost"
                 "127.0.0.1"
                 "192.168." "10."))))

(defun im/curl-options--replace-proxy (&optional proxy)
  (if (and (boundp 'request-curl-options) request-curl-options)
      (setq request-curl-options
            (remove "-x"
                    (remove
                     (nth
                      (+ (cl-position "-x" request-curl-options :test 'string=) 1)
                      request-curl-options)
                     request-curl-options)))
    (setq request-curl-options nil))
  (when proxy
    (push proxy request-curl-options)
    (push "-x" request-curl-options)))

(defun im/proxy (&optional type)
  (interactive (list (intern (completing-read "type: " '(disable :sock :http) nil t))))
  (cond ((eq type :http)
         (let ((url (car ic/proxy-http))
               (user (cadr ic/proxy-http))
               (password (caddr ic/proxy-http)))
           (setq url-gateway-method 'native
                 socks-server nil
                 url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp)
                                      ("http" . ,url)
                                      ("https" . ,url)))
           (im/curl-options--replace-proxy url)
           (when user
             (setq url-http-proxy-basic-auth-storage `((,url (,user . ,password)))))
           (message "Http proxy %s enabled." url)))
        ((eq type :sock)
         (setq url-gateway-method 'socks
               socks-server ic/proxy-sock
               url-proxy-services nil)
         (im/curl-options--replace-proxy (format "socks5://%s:%d" (cadr ic/proxy-sock) (caddr ic/proxy-sock)))
         (message "Sock proxy %s enabled." ic/proxy-sock))
        (t
         (setq url-gateway-method 'native
               socks-server nil
               url-proxy-services nil)
         (im/curl-options--replace-proxy nil)
         (message "Proxy disabled."))))

(if ic/proxy-type (im/proxy ic/proxy-type)) ; initial proxy

(provide 'cust-proxy)

;;; cust-proxy.el ends here
