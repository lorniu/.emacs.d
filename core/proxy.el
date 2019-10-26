;;; proxy.el --- Proxy Setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defcustom ic/proxy-type nil
  "Which proxy to use, http or sock."
  :type 'symbol
  :group 'imfine)

(defcustom ic/proxy-sock '("Default server" "127.0.0.1" 1080 5)
  "Socket proxy default value."
  :type 'list :group 'imfine)

(defcustom ic/proxy-http '("127.0.0.1:8118" nil nil)
  "Http proxy default value: (url user password)"
  :type 'string :group 'imfine)


(setq url-gateway-local-host-regexp
      (concat "^" (regexp-opt '("localhost" "127.0.0.1" "192.168." "10."))))

(defun im/proxy (&optional type)
  (interactive (list (intern (completing-read "type: " '(disable SOCK HTTP) nil t))))
  (cond ((eq type 'HTTP)
         (let ((url (car ic/proxy-http)) (user (cadr ic/proxy-http)) (password (caddr ic/proxy-http)))
           (setq url-gateway-method 'native socks-server nil)
           (setq url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp) ("http" . ,url) ("https" . ,url)))
           (when user (setq url-http-proxy-basic-auth-storage `((,url (,user . ,password)))))
           (message "Http proxy %s enabled." url)))
        ((eq type 'SOCK)
         (setq url-gateway-method 'socks socks-server ic/proxy-sock url-proxy-services nil)
         (message "Sock proxy %s enabled." ic/proxy-sock))
        (t
         (setq url-gateway-method 'native socks-server nil url-proxy-services nil)
         (message "Proxy disabled."))))

(im/proxy ic/proxy-type)


(provide 'proxy)

;;; proxy.el ends here
