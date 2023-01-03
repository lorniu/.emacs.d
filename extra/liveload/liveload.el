;;; liveload.el --- livereload.js -*- lexical-binding: t -*-

;;; Package-Requires: ((emacs "25") (websocket "1.3"))

;;; Commentary:

;; Hot loading and serve html file.

;;; Code:

(require 'websocket)

(defvar liveload-debug t)

(defvar liveload-process nil)

(defvar liveload-connections nil)

(defvar liveload-root nil)

(defvar liveload-port 35720)

(defvar liveload--package-directory (file-name-directory load-file-name))

(defun liveload-log (str &rest arg)
  (when liveload-debug
    (with-current-buffer (get-buffer-create " *liveload*")
      (let ((inhibit-read-only))
        (with-silent-modifications
          (goto-char (point-max))
          (insert (apply #'format (cons str arg)) "\n"))))))



(defun liveload ()
  (interactive)
  (liveload-close)
  (setq liveload-root default-directory)
  (message "Starting %s..." liveload-root)
  (setq liveload-process
        (make-network-process
         :name (format "liveload-%s" liveload-port)
         :family 'ipv4
         :host "0.0.0.0"
         :service liveload-port
         :server t
         :noquery t
         :filter-multibyte nil
         :log      'liveload--accept
         :filter   'liveload--filter
         :sentinel 'liveload--sentinel
         :plist (list :host "0.0.0.0"
                      :on-open #'liveload--ws-open
                      :on-message #'liveload--ws-message
                      :on-close #'liveload--ws-close
                      :on-error #'liveload--ws-error)))
  (add-hook 'after-save-hook #'liveload--notify-maybe 'append))

(defun liveload-close ()
  (interactive)
  (when liveload-process
    (message "Closing %s..." liveload-process)
    (websocket-server-close liveload-process)
    (setq liveload-process nil))
  (setq liveload-root nil)
  (mapc (lambda (conn) (websocket-close (process-get conn :websocket))) liveload-connections)
  (remove-hook 'after-save-hook #'liveload--notify-maybe))

(defun liveload-and-view ()
  (interactive)
  (liveload)
  (let* ((file (buffer-file-name))
         (ext (or (file-name-extension (or file "")) ""))
         (rel (cond ((or (string-equal "htm" ext) (string-equal "html" ext))
                     (file-name-nondirectory file))
                    ((string-equal "org" ext)
                     (let ((html-file (replace-regexp-in-string "org$" "html" file)))
                       (if (file-exists-p html-file)
                           (file-name-nondirectory html-file)
                         "")))
                    (t "")))
         (url (format "http://localhost:%d/%s" liveload-port (url-hexify-string rel))))
    (browse-url url)))


;;; Watch and notify

(defvar-local liveload--calculated-targets '())

(defvar liveload-potential-targets 'liveload-default-potential-targets
  "Identifies target URLs based on visited URL and current buffer.

Can be a list of strings, the symbol t, or a symbol or lambda
denoting a function of one argument producing one of the two
preceding types.

A list of strings identifies target URLs that liveload clients
connected to Emacs are told to reload. If it is empty (or nil)
saving the buffer will never cause clients to be notified.

If a function, the argument passed to the function is the URL of
the webpage that a liveload client is visiting.

If t, it is up to any functions in `liveload-notify-hook',
which see, to compute and perform any notifications.

This variable is most likely useful if set buffer-locally.")

(defvar liveload-notify-hook nil
  "Hooks to run before notifying liveload clients.")

(defun liveload--notify-maybe ()
  (let* ((calculated
          (cl-loop for conn in liveload-connections
                   for url = (process-get conn 'liveload--url)
                   for targets = (and url
                                      (if (and (not (eq t liveload-potential-targets))
                                               (or (symbolp liveload-potential-targets)
                                                   (functionp liveload-potential-targets)))
                                          (funcall liveload-potential-targets url)
                                        liveload-potential-targets))
                   when targets
                   collect (list url conn targets))))
    (setq-local liveload--calculated-targets calculated)
    (cond (calculated
           (let ((hookage (run-hook-with-args-until-success
                           'liveload-notify-hook
                           (mapcar #'car liveload--calculated-targets))))
             (unless hookage (liveload-notify))))
          (t
           (liveload-log "No one to notify for %s" (current-buffer))))))

(defun liveload-notify (&optional targets)
  (cl-loop
   for (_url conn calculated-targets) in liveload--calculated-targets
   do (cl-loop for target in (or targets
                                 (unless (eq calculated-targets t)
                                   calculated-targets))
               do (websocket-send-text
                   (process-get conn :websocket)
                   (json-encode `((command . :reload)
                                  (path . ,target)
                                  (liveCSS . t))))))
  ;; prevent accidental overcalling of `liveload-notify'
  (setq liveload--calculated-targets nil))

(defun liveload-default-potential-targets (url)
  (when (and (string-match "^\\(https?://\\(?:[a-z0-9.-]\\)\\(?::[[:digit:]]+\\)?\\)\\(.*\\)" url)
             buffer-file-name
             (save-match-data (string-match "\\(css\\|\\js\\|\\html?\\)" (or (file-name-extension buffer-file-name) ""))))
    (let* ((_address (match-string 1 url))
           (path (match-string 2 url))
           (project-dir (and buffer-file-name
                             (or (locate-dominating-file buffer-file-name ".git")
                                 (file-name-directory buffer-file-name))))
           (relative (and project-dir
                          (substring buffer-file-name
                                     (cl-mismatch project-dir buffer-file-name)))))
      (cond ((and relative (string-match "\\(css\\|\\js\\|\\html\\)" (or (file-name-extension relative))))
             (list relative))
            ((and relative (string-match "index.html?" (or (file-name-nondirectory relative) "")))
             (let ((relative-path (concat "/" (file-name-directory relative))))
               (when (string= (regexp-quote relative-path) path)
                 (list relative-path))))))))


;;; Process

(defun liveload--plain-http-p (process message)
  (and (string-match-p "^\\(GET\\|POST\\)" message)
       (not (string-match-p "^GET /livereload " message))))

(defun liveload--unjack-websocket (process)
  (let ((websocket (process-get process :websocket)))
    (unless websocket
      (error "Cannot unjack websocket process %s: no associated websocket!" process))
    (setq websocket-server-websockets (delq websocket websocket-server-websockets))
    (process-put process :websocket nil)
    (set-process-sentinel process #'liveload--sentinel)))

(defun liveload--accept (server client message)
  "Called when a client request coming."
  (websocket-server-accept server client message))

(defun liveload--filter (process output)
  (if (liveload--plain-http-p process output)
      (progn
        (liveload-log ">>> [http.filter]: %s"
                      (with-temp-buffer
                        (insert output)
                        (goto-char (point-min))
                        (buffer-substring (point) (line-end-position))))
        (liveload--unjack-websocket process)
        (liveload--http-filter process output))
    (websocket-server-filter process output)))

(defun liveload--sentinel (process change)
  (liveload-log ">>> sentinel: %s" change)
  (set-process-sentinel process nil)
  (delete-process process))


;;; Http Service

(defun liveload--http-filter (process output)
  (with-current-buffer (get-buffer-create " *liveload-http*")
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (let (method path len)
      (save-excursion
        (save-match-data
          (when (looking-at "\\([^ ]+\\) +\\([^ ]+\\) +\\([^\r]+\\)\r\n")
            (setq method (match-string 1)
                  path (cl-subseq
                        (car (split-string
                              (decode-coding-string (match-string 2) 'iso-8859-1)
                              "?"))
                        1)))))
      (save-excursion
        (re-search-forward "Content-Length: \\([0-9]+\\)\r\n" nil t)
        (setq len (match-string 1)))
      (when (and path (or (null len) (>= (buffer-size) (string-to-number len))))
        (erase-buffer)
        (when (file-directory-p path)
          (let* ((dir (file-name-as-directory path))
                 (indexes (cl-remove-if-not #'file-exists-p
                                            (list (expand-file-name "index.htm" dir)
                                                  (expand-file-name "index.html" dir)))))
            (setq path (or (car indexes) dir))))
        (cond
         ((file-directory-p path)
          (let ((title (concat "Directory listing for " path))
                (fpath (expand-file-name path liveload-root)))
            (with-temp-buffer
              (insert "<html>\n<head><title>" title "</title></head>\n")
              (insert "<body>\n<h2>" title "</h2>\n<hr/>\n<ul>")
              (dolist (file (directory-files fpath))
                (unless (eq ?. (aref file 0))
                  (let* ((full (expand-file-name file path))
                         (tail (if (file-directory-p full) "/" ""))
                         (f (url-insert-entities-in-string file))
                         (l (url-hexify-string file)))
                    (insert (format "<li><a href=\"%s%s\">%s%s</a></li>\n" l tail f tail)))))
              (insert "</ul>\n<hr/>\n</body>\n</html>")
              (liveload--http-ok process (buffer-string) path))))
         ((string-equal path "livereload.js")
          (with-temp-buffer
            (insert-file-contents-literally
             (concat liveload--package-directory "/livereload.js"))
            (liveload--http-ok process (buffer-string) path)))
         (t (let ((rp (expand-file-name path liveload-root)))
              (if (not (file-exists-p rp))
                  (liveload--http-err process 404)
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally rp)
                  (when (string-match-p "html?$" path)
                    (goto-char (point-max))
                    (re-search-backward "</html>" nil t)
                    (insert (format "<livereload>
  <script>
    !function () {
      let s = document.createElement('script');
      s.src = location.protocol + '//' + location.hostname + ':%d/livereload.js';
      document.querySelector('livereload').appendChild(s);
    }();
  </script>
</livereload>
" liveload-port)))
                  (liveload--http-ok process (buffer-string) path)))))))
      (erase-buffer))))

(defun liveload--http-ok (process resp path)
  (let ((mappings '(("htm" . "text/html")
                    ("html" . "text/html")
                    ("js" . "text/javascript")
                    ("txt" . "text/plain"))))
    (process-send-string process
                         (concat "HTTP/1.1 200 OK\r\nConnection: close\r\n"
                                 (format "Content-Length: %d\r\n" (string-bytes resp))
                                 (format "Content-Type: %s\r\n" (alist-get (file-name-extension path) mappings))
                                 "\r\n"
                                 resp))))

(defun liveload--http-err (process code)
  (let ((mappings '((404 . "Not Found"))))
    (process-send-string process
                         (format "HTTP/1.1 %s %s\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"
                                 code
                                 (or (alist-get code mappings) "OK")))))


;;; Websocket Service

(defun liveload--ws-open (ws)
  (liveload-log ">>> [ws.open]")
  (websocket-send-text ws
                       (json-encode '((command . "hello")
                                      (protocols . ["http://livereload.com/protocols/connection-check-1"
                                                    "http://livereload.com/protocols/official-7"])
                                      (serverName . "Emacs liveload"))))
  (cl-pushnew (websocket-conn ws) liveload-connections))

(defun liveload--ws-message (ws f)
  (let* ((conn (websocket-conn ws))
         (message (json-read-from-string (websocket-frame-payload f)))
         (command (alist-get 'command message)))
    (liveload-log ">>> [ws.message]: %s\n" message)
    (unless command
      (error "no command in %s!" message))
    (pcase command
      ("hello"
       (liveload-log "%s client says: hello, server!" conn)
       (process-put conn 'liveload--hello message)
       (process-put conn 'liveload--timestamp-string (current-time-string)))
      ("info"
       (liveload-log "%s sends info: %s!" conn message)
       (let ((url (alist-get 'url message)))
         (process-put conn 'liveload--url url)
         (liveload-log "%s registering url %s" conn url))))))

(defun liveload--ws-close (ws)
  (let ((conn (websocket-conn ws)))
    (setq liveload-connections (remove conn liveload-connections))
    (delete-process conn)
    (liveload-log ">>> [ws.closed] %s!" conn)))

(defun liveload--ws-error (ws &rest args)
  (liveload-log ">>> [ws.ERROR] `%S': %s" (car args) (cdr args)))

(provide 'liveload)

;;; liveload.el ends here
