;;; -*- lexical-binding: t -*-

;; For csharp script.
;;
;; Install dotnet script first:
;;   dotnet tool install -g dotnet-script
;;

;;; Code:
(require 'ob)
(require 'csharp-mode)

(add-to-list 'org-babel-tangle-lang-exts '("csx" . "csx"))

(defvar org-babel-default-header-args:csx '())

(defun ob-csx--build-script-run-command (path)
  "Create run command according to the PATH."
  (format "dotnet script %s" path))

(defun org-babel-execute:csx (body params)
  (let* ((processed-params (org-babel-process-params params))
         (src-temp (org-babel-temp-file "csharp-src-" ".csx")))
    (with-temp-file src-temp (insert body))
    (let ((results (org-babel-eval (ob-csx--build-script-run-command src-temp) "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assoc :result-params params))
         (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "c-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))


(provide 'ob-csx)
