;;; bpmf.el --- Benchmark/Proxy and so on, most early ones. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar loaded-messages nil)



(defvar bm/time nil)
(defvar bm/last-time nil)

(defun time-subtract-seconds (b a)
  (float-time (time-subtract b a)))

(defun time-subtract-millis (b a)
  (* 1000.0 (time-subtract-seconds b a)))

(defun bm/show-time (&optional sortp)
  (ppp (if sortp
           (let ((time (copy-tree bm/time)))
             (sort time (lambda (a b) (> (cdr a) (cdr b)))))
         bm/time)))

(defun bm/differ ()
  (ppp
   (sort (seq-difference bm/time bm/last-time)
         (lambda (a b) (> (cdr a) (cdr b)))))
  (setq bm/last-time (copy-tree bm/time)) t)

(defun bm/require-statics-advice (oldfun feature &optional filename noerror)
  (let* ((already-loaded (memq feature features))
         (start-time (and (not already-loaded) (current-time))))
    (prog1
        (funcall oldfun feature filename noerror)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (time-subtract-millis (current-time) start-time)))
          (add-to-list 'bm/time (cons feature time) t))))))

(add-function :around (symbol-function 'require) 'bm/require-statics-advice)

(defun display-startup-echo-area-message ()
  (message "%s%s>> Loaded successfully in %.2f seconds."
           (mapconcat 'identity (reverse loaded-messages) "\n")
           (if loaded-messages "\n" "")
           (time-subtract-seconds after-init-time before-init-time)))



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

(defun im/curl-options--replace-proxy (&optional proxy)
  (if (and (boundp 'request-curl-options) request-curl-options)
      (setq request-curl-options
            (remove "-x"
                    (remove
                     (nth
                      (+ (position "-x" request-curl-options :test 'string=) 1)
                      request-curl-options)
                     request-curl-options)))
    (setq request-curl-options nil))
  (when proxy
    (push proxy request-curl-options)
    (push "-x" request-curl-options)))

(defun im/proxy (&optional type)
  (interactive (list (intern (completing-read "type: " '(disable SOCK HTTP) nil t))))
  (cond ((eq type 'HTTP)
         (let ((url (car ic/proxy-http)) (user (cadr ic/proxy-http)) (password (caddr ic/proxy-http)))
           (setq url-gateway-method 'native socks-server nil)
           (setq url-proxy-services `(("no_proxy" . ,url-gateway-local-host-regexp) ("http" . ,url) ("https" . ,url)))
           (im/curl-options--replace-proxy url)
           (when user (setq url-http-proxy-basic-auth-storage `((,url (,user . ,password)))))
           (message "Http proxy %s enabled." url)))
        ((eq type 'SOCK)
         (setq url-gateway-method 'socks socks-server ic/proxy-sock url-proxy-services nil)
         (im/curl-options--replace-proxy (format "socks5://%s:%d" (cadr ic/proxy-sock) (caddr ic/proxy-sock)))
         (message "Sock proxy %s enabled." ic/proxy-sock))
        (t
         (setq url-gateway-method 'native socks-server nil url-proxy-services nil)
         (im/curl-options--replace-proxy nil)
         (message "Proxy disabled."))))

(im/proxy ic/proxy-type)


(provide 'bpmf)

;;; bpmf.el ends here
