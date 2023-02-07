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

(defcustom ln-postgres (list "postgres" "root")
  "Postgres connection info, (db user [password] [host] [port]) format."
  :type 'list
  :group 'imfine)

(x sql
   "M-x: sql-connect/sql-postgres"
   :init
   (defvar sql-connection-alist
     `((,(intern (format "postgres/%s" ln-host))
        (sql-product 'postgres)
        (sql-server ,ln-host)
        (sql-database "imdev")
        (sql-user ln-host-user))
       (,(intern (format "mysql/%s" ln-host))
        (sql-product 'mysql)
        (sql-server ,ln-host)
        (sql-port 3306)
        (sql-database "test")
        (sql-user ,ln-host-user))))

   ;; push to quicker
   (with-over
    (cl-loop for conn in sql-connection-alist
             do (add-to-list 'ln-quicker-list-default
                             (list (format "db: %s" (car conn)) conn '(ln:host)))))

   :config
   ;; for SQLServer (yay -S mssql-tools)
   (when (executable-find "sqlcmd")
     (setopt sql-ms-program "sqlcmd"
             sql-ms-options '("-w" "300" "-y" "30" "-Y" "30" "-k")))

   (when IS-WIN
     (setopt sql-mysql-options '("-t"))
     (add-hook 'sql-interactive-mode-hook 'ln/local-encoding))

   ;; complete with corfu
   (defun:hook sql-interactive-mode-hook/complete ()
     (setq-local corfu-auto nil)
     (make-variable-buffer-local 'completion-at-point-functions)
     (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

(x pg
   "PG interface with its socket-level frontend/backend protocol.

Can use `with-pg-connection' or wrapped `ln:with-pg' to access postgres.

    (with-pg-connection (conn user pass host port ...)
      (with-pg-transaction conn
        (pg:for-each conn ...)
        (pg:exec conn ...)))

    (ln:with-pg 'sql')            ; sql
    (ln:with-pg (pg:exec ...))    ; statement
    (ln:with-pg (exec/query ...)) ; alias

    (log/db ..) ; util function use this to log

Custom `ln-postgres' to specify the db used by `ln:with-pg'.
   ")

(x sqlplus
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

(x sql-indent
   :ref "alex-hhh/emacs-sql-indent"
   :hook (sql-mode . sqlind-minor-mode))



(defmacro ln:with-pg (&rest sql-or-stmts)
  "Shortcut macro for access my postgres db. Use `ln-postgres' to specify private db connection."
  `(progn
     (or ln-postgres
         (error "Please config `ln-postgres' first."))
     (cl-multiple-value-bind (_db _user _password _host _port) ln-postgres
       (with-pg-connection (conn _db _user _password _host _port)
         ,(if (stringp (car sql-or-stmts))
              `(pg:exec conn ,@sql-or-stmts)
            `(cl-flet ((exec (apply-partially 'pg:exec conn))
                       (query (apply-partially 'pg:for-each conn)))
               ,@sql-or-stmts))))))

(defun log/db (&optional msg cat ext table)
  "Log message to postgres. Should table \\='elog' and macro `ln:with-pg' exist."
  (ln:with-pg
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
           (ln:thing-at-region-or-point
            (lambda () (read-string "String to log: "))))))
    (message "%s" (log/db send-string))))
