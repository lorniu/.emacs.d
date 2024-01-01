;;; -*- lexical-binding: t -*-

;; Modes to connect DB:
;;
;; - sql-xxx :: Common Plan, need client program installed.
;; - SQLPLUS :: Just for Oracle.
;; - pg library :: For Postgres, by tcp, it's very convenience.
;;
;; You can custom `sql-connection-alist' for `sql-connect' command.
;;

;; For Sql-Babel Demo, You can set DB globally:
;;
;;   (setq org-babel-default-header-args:sql
;;         '((:engine . "mssql")
;;           (:dbuser . "sa")
;;           (:dbpassword . "sa")
;;           (:dbhost . "localhost")
;;           (:database . "xxx")))
;;
;; Or add DRAWER under header:
;;
;;   :PROPERTIES:
;;   :header-args:sql+: :engine mysql
;;   :header-args:sql+: :dbuser root :dbpassword root :dbhost localhost :database mysql
;;   :END:
;;
;; Then:
;;
;;   #+begin_src sql :database xxx
;;     select * from user;
;;   #+end_src
;;
;; SQLite:
;;
;;  #+begin_src :db test-sqlite.db
;;    create table xxx (id int);
;;  #+end_src
;;

;;; Code:

(defcustom im.postgres (list "postgres" "root")
  "Postgres connection info, (db user [password] [host] [port]) format."
  :type 'list
  :group 'imfine)

(xzz sql
  "M-x: sql-connect/sql-postgres"
  :init
  (defvar sql-connection-alist
    `((,(intern (format "postgres/%s" im.host))
       (sql-product 'postgres)
       (sql-server ,im.host)
       (sql-database "imdev")
       (sql-user im.host-user))
      (,(intern (format "mysql/%s" im.host))
       (sql-product 'mysql)
       (sql-server ,im.host)
       (sql-port 3306)
       (sql-database "test")
       (sql-user ,im.host-user))))

  ;; push to quicker
  (with-over
   (cl-loop for conn in sql-connection-alist
            do (add-to-list 'im.quicker-list-default
                            (list (format "db: %s" (car conn)) conn '(im:host)))))

  :config
  ;; for SQLServer (yay -S mssql-tools)
  (when (executable-find "sqlcmd")
    (setopt sql-ms-program "sqlcmd"
            sql-ms-options '("-w" "300" "-y" "30" "-Y" "30" "-k")))

  (when IS-WIN
    (setopt sql-mysql-options '("-t"))
    (add-hook 'sql-interactive-mode-hook 'im/local-encoding))

  ;; complete with corfu
  (defun:hook sql-interactive-mode-hook/complete ()
    (setq-local corfu-auto nil)
    (make-variable-buffer-local 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

(xzz pg
  "PG interface with its socket-level frontend/backend protocol.

Can use `with-pg-connection' or wrapped `im:with-pg' to access postgres.

    (with-pg-connection (conn user pass host port ...)
      (with-pg-transaction conn
        (pg:for-each conn ...)
        (pg:exec conn ...)))

    (im:with-pg 'sql')            ; sql
    (im:with-pg (pg:exec ...))    ; statement
    (im:with-pg (exec/query ...)) ; alias

    (log/db ..) ; util function use this to log

Custom `im.postgres' to specify the db used by `im:with-pg'.
   ")

(xzz sqlplus
  "M-x: sqlplus
   "
  "SQLi for Oracle is not enough, for example, bad output format.
   "
  "Use this instead for oracle, util some days, merge its features to SQLi.
   "
  :if (executable-find "sqlplus")
  :commands sqlplus
  :mode "\\.spl\\'"
  :config
  (when IS-WIN
    (setopt sqlplus-process-encoding 'gbk)
    (setenv "NLS_LANG" "AMERICAN_AMERICA.UTF8")))

(xzz sql-indent
  :ref "alex-hhh/emacs-sql-indent"
  :hook (sql-mode . sqlind-minor-mode))



(defmacro im:with-pg (&rest sql-or-stmts)
  "Shortcut macro for access my postgres db. Use `im.postgres' to specify private db connection."
  `(progn
     (or im.postgres
         (error "Please config `im.postgres' first."))
     (cl-multiple-value-bind (_db _user _password _host _port) im.postgres
       (with-pg-connection (conn _db _user _password _host _port)
                           ,(if (stringp (car sql-or-stmts))
                                `(pg:exec conn ,@sql-or-stmts)
                              `(cl-flet ((exec (apply-partially 'pg:exec conn))
                                         (query (apply-partially 'pg:for-each conn)))
                                 ,@sql-or-stmts))))))

(defun log/db (&optional msg cat ext table)
  "Log message to postgres. Should table \\='elog' and macro `im:with-pg' exist."
  (im:with-pg
   "insert into %s (cat, msg, ext) values (%s, %s, %s)"
   (list (or table "elog")
         (if cat (format "'%s'" cat) "null")
         (if msg (format "'%s'" msg) "null")
         (if ext (format "'%s'" ext) "null"))))

(defun log/current-to-db (&optional arg)
  "Send current selection or buffer to private postgres db."
  (interactive "P")
  (let ((send-string
         (if (and arg (y-or-n-p "Log current buffer to db?"))
             (buffer-string)
           (im:thing-at-region-or-point
            (lambda () (read-string "String to log: "))))))
    (message "%s" (log/db send-string))))
