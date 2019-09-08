;;; imdb.el --- Database/SQL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ic/my-postgres '("imdev" "vip")
  "Postgres connection info, (db user [password] [host] [port]) format."
  :type 'list
  :group 'imfine)


;;; Common

(x sql/w
   "M-x: sql-connect/sql-postgres"
   :init
   (setq sql-connection-alist
         `((,(intern (format "postgres/%s" *vps*))
            (sql-product 'postgres)
            (sql-server ,*vps*)
            (sql-database "imdev")
            (sql-user "vip"))
           (,(intern (format "mysql/%s" *vps*))
            (sql-product 'mysql)
            (sql-server ,*vps*)
            (sql-port 3306)
            (sql-database "test")
            (sql-user "root"))))

   :config
   (add-hook 'sql-interactive-mode-hook 'im/set-buffer-mono-font)
   (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) *\\[[^ ]*\\]> *")

   (env-windows
    (setq sql-mysql-options '("-t"))
    (add-hook 'sql-interactive-mode-hook 'im/local-encoding)))


;;; Oracle

(x sqlplus
   "M-x: sqlplus
   "
   "SQLi for Oracle is not enough, for example, bad output format.
   "
   "Use this instead for oracle, util some days, merge its features to SQLi.
   "
   :commands sqlplus
   :init
   (if (env-windows) (setq sqlplus-process-encoding 'gbk) (setenv "NLS_LANG" "AMERICAN_AMERICA.UTF8"))
   (add-to-list 'auto-mode-alist '("\\.spl\\'" . sqlplus-mode))
   (setq sqlplus-session-cache-dir (concat _CACHE_ "sqlplus/"))
   :config
   (im/make-face-mono 'sqlplus-table-head-face 'sqlplus-table-odd-rows-face 'sqlplus-table-even-rows-face))


;;; Postgres

(x pg
   "PG interface with its socket-level frontend/backend protocol.

Can use `with-pg-connection' or wrapped `with-my-pg' to access postgres.

    (with-pg-connection (conn user pass host port ...)
      (with-pg-transaction conn
        (pg:for-each conn ...)
        (pg:exec conn ...)))

    (with-my-pg 'sql')            ; sql
    (with-my-pg (pg:exec ...))    ; statement
    (with-my-pg (exec/query ...)) ; alias

    (log/db ..) ; util function use this to log

Custom `ic/my-postgres' to specify the db used by `with-my-pg'.
   "
   :init
   (require 'pg)

   (defmacro with-my-pg (&rest sql-or-stmts)
     "Shortcut macro for access my postgres db. Use `ic/my-postgres' to specify private db connection."
     `(progn
        (or ic/my-postgres
            (error "Please config `ic/my-postgres' first."))
        (multiple-value-bind (db user password host port) ic/my-postgres
          (with-pg-connection (conn db user password host port)
            ,(if (stringp (car sql-or-stmts))
                 `(pg:exec conn ,@sql-or-stmts)
               `(cl-flet ((exec (apply-partially 'pg:exec conn))
                          (query (apply-partially 'pg:for-each conn)))
                  ,@sql-or-stmts)))))))


;;; Utils

(defun log/db (&optional msg cat ext table)
  "Log message to postgres. Should table 'elog' and macro `with-my-pg' exist."
  (with-my-pg
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
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-string "String to log: ")))))
    (message "%s" (log/db send-string))))


(provide 'imdb)

;;; imdb.el ends here
