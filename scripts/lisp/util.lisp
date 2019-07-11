;; dexador

(ql:quickload :dexador)

(in-package :dexador)

(shadow 'trace)
(export 'trace)

(defun trace (uri &rest args
              &key version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects
                force-binary want-stream
                ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (let ((*verbose* t))
    (apply #'request uri :method :get args)
    (values)))
