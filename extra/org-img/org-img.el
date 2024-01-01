;;; org-img.el --- Image drag-and-drop for Org-mode. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; URL: https://github.com/abo-abo/org-download
;;
;; `org-img-method': attach/directory
;; `org-img-timestamp': optionally add a timestamp to the file name.
;; `org-img-backend': use `url-retrieve' or `wget' or `curl'.
;;
;;; Code:

(require 'cl-lib)
(require 'async nil t)
(require 'url-parse)
(require 'url-http)
(require 'org)
(require 'org-attach)

(defgroup org-img nil
  "Image drag-and-drop for org-mode."
  :group 'org
  :prefix "org-img-")

(defcustom org-img-method 'directory
  "The way images should be stored."
  :type '(choice
          (const :tag "Directory" directory)
          (const :tag "Attachment" attach)
          (function :tag "Custom function")))

(defcustom org-img-backend nil
  "Method to use for downloading, default use url-retrieve."
  :type '(choice
          (const :tag "wget" "wget \"%s\" -O \"%s\"")
          (const :tag "curl" "curl \"%s\" -o \"%s\"")
          (const :tag "url-retrieve" nil)))

(defcustom org-img-edit-method nil
  "The tool to edit an image link."
  :type '(choice
          (const :tag "gimp" "gimp '%s'")
          (const :tag "krita" "krita '%s'")))

(defcustom org-img-clipboard-method nil
  "The tool to capture clipboard."
  :type '(choice
          (const :tag "xclip" "xclip -selection clipboard -t image/png -o > '%s'")
          (const :tag "win" "powershell -Command (Get-Clipboard -Format Image).save('%s')")))

(defcustom org-img-screenshot-method nil
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f '%s'")
          (const :tag "scrot" "scrot -s '%s'")
          (const :tag "flameshot" "flameshot gui --raw > '%s'")
          (const :tag "screencapture" "screencapture -i '%s'")))

(defcustom org-img-temporary-file (expand-file-name "oimg.png" temporary-file-directory)
  "The file to capture screenshots."
  :type 'string)

(defcustom org-img-timestamp "_%Y%m%d_%H%M%S"
  "This `format-time-string'-style string will be appended to the file name.
Set this to \"\" if you don't want time stamps."
  :type 'string)

(defcustom org-img-img-regex-list
  '("<img +src=\"" "<img +\\(class=\"[^\"]+\"\\)? *src=\"")
  "This regex is used to unalias links that look like images.
The html to which the links points will be searched for these
regexes, one by one, until one succeeds.  The found image address
will be used."
  :type '(repeat string))

(defcustom org-img-image-html-width 0
  "When non-zero add #+attr_html: :width tag to the image."
  :type 'integer)

(defcustom org-img-image-latex-width 0
  "When non-zero add #+attr_latex: :width tag to the image."
  :type 'integer)

(defcustom org-img-image-org-width 0
  "When non-zero add #+attr_org: :width tag to the image."
  :type 'integer)

(defcustom org-img-image-attr-list nil
  "Add attr info to the image.
For example:

  (\"#+attr_html: :width 80% :align center\"
   \"#+attr_org: :width 100px\")"
  :type '(repeat string))

(defcustom org-img-delete-image-after-download nil
  "When non-nil delete local image after download."
  :type 'boolean)

(defcustom org-img-display-inline-images t
  "When non-nil display inline images in org buffer after download."
  :type
  '(choice
    (const :tag "On" t)
    (const :tag "Off" nil)
    (const :tag "Posframe" posframe)))

(defcustom org-img-link-format-function #'org-img-link-format-function-default
  "Function that takes FILENAME and returns a org link."
  :type 'function)

(defcustom org-img-abbreviate-filename-function #'file-relative-name
  "Function that takes FILENAME and returns an abbreviated file name."
  :type '(choice
          (const :tag "relative" file-relative-name)
          (const :tag "absolute" expand-file-name)))



(defvar org-link-any-re nil)

(defvar org-img-last-save-file nil
  "Variable to hold the full path of the last downloaded file.")

(defvar org-img--file-content nil
  "When non-nil, store the file name of an already downloaded file.")

(defvar org-img-file-format-function #'org-img-file-format-default)

(defvar org-img-annotate-function #'org-img-annotate-default
  "Function that takes LINK and returns a string.
It's inserted before the image link and is used to annotate it.")

(defvar org-img-link-format "[[file:%s]]\n"
  "Format of the file link to insert.")

(defvar org-img-posframe-show-params
  '(;; Please do not remove :timeout or set it to large.
    :timeout 3
    :internal-border-width 1
    :internal-border-color "red"
    :min-width 40
    :min-height 10
    :poshandler posframe-poshandler-window-center)
  "List of parameters passed to `posframe-show'.")

(declare-function posframe-workable-p "ext:posframe")
(declare-function posframe-show "ext:posframe")

(declare-function org-attach-dir "org-attach")
(declare-function org-attach-attach "org-attach")
(declare-function org-attach-sync "org-attach")



(defun oi/url (link)
  "Save image at address LINK."
  (interactive "sUrl: " org-mode)
  (let* ((link-and-ext (org-img--parse-link link))
         (filename
          (cond ((eq org-img-method 'attach)
                 (apply #'org-img--fullname link-and-ext))
                ((fboundp org-img-method)
                 (funcall org-img-method link))
                (t
                 (apply #'org-img--fullname link-and-ext)))))
    (setq org-img-last-save-file filename)
    (org-img--image link filename)
    (when (org-img-org-mode-p)
      (when (eq org-img-method 'attach)
        (org-attach-attach filename nil 'none))
      (org-img--insert-link link filename))
    (when (and (eq org-img-delete-image-after-download t)
               (not (url-handler-file-remote-p (current-kill 0))))
      (delete-file link delete-by-moving-to-trash))))

(defun oi/url-yank ()
  "Call `org-img-url' with current kill."
  (interactive nil org-mode)
  (oi/url
   (replace-regexp-in-string "\n+$" "" (current-kill 0))))

(defun oi/clipboard ()
  "Capture from clipboard and insert the resulting file."
  (interactive nil org-mode)
  (let ((default-directory "~")
        (method (or org-img-clipboard-method (error "No proper tool, may config to use xclip, powershell or others."))))
    (make-directory (file-name-directory org-img-temporary-file) t)
    (shell-command (format method org-img-temporary-file)))
  (if (not (file-exists-p org-img-temporary-file))
      (error "May be capture clipboard failed? No file generated.")
    (oi/url org-img-temporary-file)
    (delete-file org-img-temporary-file)))

(defun oi/screenshot ()
  "Capture screenshot and insert the resulting file."
  (interactive nil org-mode)
  (let ((default-directory "~")
        (method (or org-img-screenshot-method (error "No suitable screen-shoot-tool configed."))))
    (make-directory (file-name-directory org-img-temporary-file) t)
    (shell-command (format method org-img-temporary-file)))
  (if (not (file-exists-p org-img-temporary-file))
      (error "May be screenshot failed? No file generated.")
    (oi/url org-img-temporary-file)
    (delete-file org-img-temporary-file)))

(defun oi/edit ()
  "Open the image at point for editing."
  (interactive nil org-mode)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (start-process-shell-command
       "org-img-edit"
       " *org-img-edit*"
       (format org-img-edit-method
               (shell-quote-wildcard-pattern
                (url-unhex-string (plist-get (cadr context) :path))))))))

(defun oi/rename ()
  "Rename file at point."
  (interactive nil org-mode)
  (let* ((path-info (org-img--link-paths))
         (new-path (let* ((path (read-file-name (format "Rename ’%s’ to: " (plist-get path-info :ofile))
                                                (let ((d (expand-file-name (plist-get path-info :odir))))
                                                  (if (string-match-p "/$" d) (cl-subseq d 0 (- (length d) 1)) d))
                                                (expand-file-name (plist-get path-info :full))))
                          (name (file-name-sans-extension (file-name-nondirectory path)))
                          (ext (file-name-extension path)))
                     (if (zerop (length name))
                         (setq path (expand-file-name (plist-get path-info :ofile) path))
                       (unless ext (setq path (concat path "." (plist-get path-info :ext)))))
                     path)))
    (make-directory (file-name-directory new-path) t)
    (rename-file (plist-get path-info :full) new-path)
    (org-img--replace-all (plist-get path-info :opath) (file-relative-name new-path))
    (org-img--display-inline-images)
    (message "Renamed to '%s'." new-path)))

(defun oi/delete ()
  "Delete inline image link on current line, and the file that it points to."
  (interactive nil org-mode)
  (cond ((org-img--at-comment-p)
         (delete-region (line-beginning-position) (line-end-position))
         (org-img--delete (line-beginning-position) nil 1))
        ((region-active-p)
         (org-img--delete (region-beginning) (region-end))
         (delete-region (region-beginning) (region-end)))
        ((looking-at org-link-any-re)
         (let ((fname (org-link-unescape (match-string-no-properties 2))))
           (when (file-exists-p fname)
             (delete-file fname)
             (delete-region (match-beginning 0) (match-end 0))
             (when (eolp) (delete-char 1)))))
        (t (org-img--delete (line-beginning-position) (line-end-position))))
  (when (eq org-img-method 'attach)
    (org-attach-sync)))

(defun oi/open-dired ()
  "Open in dired."
  (interactive nil org-mode)
  (find-file (plist-get (org-img--link-paths) :dir)))



(defun org-img--link-paths ()
  (let* ((context (org-element-context))
         (link-path (if (not (eq (car-safe context) 'link))
                        (user-error "Not on a link")
                      (org-element-property :path context)))
         (link-dir (or (file-name-directory link-path) ""))
         (link-name (file-name-nondirectory link-path))
         (dir (expand-file-name link-dir))
         (name (file-name-sans-extension link-name))
         (ext (file-name-extension link-name)))
    (list :dir dir :name name :ext ext :full (expand-file-name link-path) ; absolute
          :odir link-dir :ofile link-name :opath link-path                ; relative
          )))

(defun org-img-org-mode-p ()
  "Return `t' if major-mode or derived-mode-p equals 'org-mode, otherwise `nil'."
  (or (eq major-mode 'org-mode) (when (derived-mode-p 'org-mode) t)))

(defun org-img--display-inline-images ()
  (cond
   ((eq org-img-display-inline-images t)
    (org-display-inline-images))
   ((eq org-img-display-inline-images 'posframe)
    (require 'posframe)
    (when (posframe-workable-p)
      (let ((buffer (get-buffer-create " *org-img-view")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert-image-file org-img-last-save-file))
        (apply #'posframe-show
               buffer
               org-img-posframe-show-params))))))

(defun org-img--fullname (link &optional ext)
  (let* ((filename (file-name-nondirectory
                    (car (url-path-and-query
                          (url-generic-parse-url link)))))
         (last-dir (and org-img-last-save-file
                        (file-name-directory org-img-last-save-file)))
         (base (if (and last-dir (cl-search (expand-file-name default-directory) last-dir))
                   last-dir
                 default-directory))
         (path (read-file-name "File save as: " base base))
         (dir (file-name-directory path))
         (name (file-name-nondirectory path)))
    (if (not (file-exists-p dir)) (make-directory dir t))
    (if (not (string-blank-p name))
        (format "%s.%s" (file-name-sans-extension path) (or ext (file-name-extension filename)))
      (when (string-match ".*?\\.\\(?:png\\|jpe?g\\)\\(.+\\)$" filename)
        (setq filename (replace-match "" nil nil filename 1)))
      (when ext
        (setq filename (concat (file-name-sans-extension filename) "." ext)))
      (abbreviate-file-name
       (expand-file-name
        (funcall org-img-file-format-function filename)
        dir)))))

(defun org-img-file-format-default (filename)
  "It's affected by `org-img-timestamp'."
  (let ((f (file-name-sans-extension filename))
        (e (file-name-extension filename)))
    (concat f (format-time-string org-img-timestamp) "." e)))

(defun org-img-annotate-default (link)
  "Annotate LINK with the time of download."
  (format "#+OI: %s @ %s\n"
          (if (equal link org-img-temporary-file)
              "local"
            link)
          (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun org-img-link-format-function-default (filename)
  "The default function of `org-img-link-format-function'."
  (if (and (>= (string-to-number org-version) 9.3)
           (eq org-img-method 'attach))
      (format "[[attachment:%s]]\n"
              (org-link-escape
               (file-relative-name filename (org-attach-dir))))
    (format org-img-link-format
            (org-link-escape
             (funcall org-img-abbreviate-filename-function filename)))))

(defun org-img--image (link filename)
  "Save LINK to FILENAME asynchronously and show inline images in current buffer."
  (when (string= "file" (url-type (url-generic-parse-url link)))
    (setq link (url-unhex-string (url-filename (url-generic-parse-url link)))))
  (cond ((and (not (file-remote-p link))
              (file-exists-p link))
         (copy-file link (expand-file-name filename)))
        (org-img--file-content
         (copy-file org-img--file-content (expand-file-name filename))
         (setq org-img--file-content nil))
        ((null org-img-backend)
         (org-img--image/url-retrieve link filename))
        (t
         (org-img--image/command org-img-backend link filename))))

(defun org-img--image/command (command link filename)
  "Using COMMAND, save LINK to FILENAME.
COMMAND is a format-style string with two slots for LINK and FILENAME."
  (async-start
   `(lambda () (shell-command
                ,(format command link
                         (expand-file-name filename))))
   (let ((cur-buf (current-buffer)))
     (lambda (_x)
       (with-current-buffer cur-buf
         (org-img--display-inline-images))))))

(defun org-img--write-image (status filename)
  "Write current buffer STATUS to FILENAME."
  (let ((err (plist-get status :error)))
    (when err
      (error
       "HTTP error %s"
       (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
  (delete-region
   (point-min)
   (progn
     (re-search-forward "\n\n" nil 'move)
     (point)))
  (let ((coding-system-for-write 'no-conversion))
    (write-region nil nil filename nil nil nil 'confirm)))

(defun org-img--image/url-retrieve (link filename)
  "Save LINK to FILENAME using `url-retrieve'."
  (url-retrieve
   link
   (lambda (status filename buffer)
     (org-img--write-image status filename)
     (cond ((org-img-org-mode-p)
            (with-current-buffer buffer
              (org-img--display-inline-images)))
           ((eq major-mode 'dired-mode)
            (let ((inhibit-message t))
              (with-current-buffer (dired (file-name-directory filename))
                (revert-buffer nil t))))))
   (list
    (expand-file-name filename)
    (current-buffer))
   nil t))

(defun org-img--detect-ext (link buffer)
  (let (ext)
    (with-current-buffer buffer
      (cond ((let ((regexes org-img-img-regex-list)
                   lnk)
               (while (and (not lnk) regexes)
                 (goto-char (point-min))
                 (when (re-search-forward (pop regexes) nil t)
                   (backward-char)
                   (setq lnk (read (current-buffer)))))
               (when lnk
                 (setq link lnk))))
            ((progn
               (goto-char (point-min))
               (when (re-search-forward "^Content-Type: image/\\(.*\\)$" nil t)
                 (setq ext (match-string 1)))))
            ((progn
               (goto-char (point-min))
               (when (re-search-forward "^Content-Type: application/pdf" nil t)
                 (setq ext "pdf"))
               (re-search-forward "^%PDF")
               (beginning-of-line)
               (write-region
                (point) (point-max)
                (setq org-img--file-content "/tmp/oimg.pdf"))
               t))
            (t
             (error "Link %s does not point to an image; unaliasing failed" link)))
      (list link ext))))

(defun org-img--parse-link (link)
  (cond ((image-type-from-file-name link)
         (list link (format "%s" (image-type-from-file-name link))))
        ((string-match "^file:///" link)
         (list link nil))
        (t
         (let ((buffer (url-retrieve-synchronously link t)))
           (org-img--detect-ext link buffer)))))

(defun org-img--replace-all (oldpath newpath)
  "Function to search for the OLDPATH inside the buffer and replace it by the NEWPATH."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward oldpath nil t)
      (replace-match newpath))))

(defun org-img--insert-link (link filename)
  (let* ((beg (point))
         (line-beg (line-beginning-position))
         (indent (- beg line-beg))
         (in-item-p (org-in-item-p))
         str)
    (if (looking-back "^[ \t]+" line-beg)
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (insert (funcall org-img-annotate-function link))
    (dolist (attr org-img-image-attr-list)
      (insert attr "\n"))
    (insert (if (= org-img-image-html-width 0)
                ""
              (format "#+attr_html: :width %dpx\n" org-img-image-html-width)))
    (insert (if (= org-img-image-latex-width 0)
                ""
              (format "#+attr_latex: :width %dcm\n" org-img-image-latex-width)))
    (insert (if (= org-img-image-org-width 0)
                ""
              (format "#+attr_org: :width %dpx\n" org-img-image-org-width)))
    (insert (funcall org-img-link-format-function filename))
    (org-img--display-inline-images)
    (setq str (buffer-substring-no-properties line-beg (point)))
    (when in-item-p
      (indent-region line-beg (point) indent))
    str))

(defun org-img--at-comment-p ()
  "Check if current line begins with #+DOWLOADED:."
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "#\\+OI:")))

(defun org-img--delete (beg end &optional times)
  "Delete inline image links and the files they point to between BEG and END.

When TIMES isn't nil, delete only TIMES links."
  (unless times
    (setq times most-positive-fixnum))
  (save-excursion
    (goto-char beg)
    (while (and (>= (cl-decf times) 0)
                (re-search-forward "\\[\\[file:\\([^]]*\\)\\]\\]" end t))
      (let ((str (match-string-no-properties 1)))
        (delete-region beg
                       (match-end 0))
        (when (file-exists-p str)
          (delete-file str))))))

(defun org-img-dnd-fallback (uri action)
  (let ((dnd-protocol-alist
         (rassq-delete-all
          'org-img-dnd
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun org-img-dnd (uri action)
  "When in `org-mode' and URI points to image, download it.
Otherwise, pass URI and ACTION back to dnd dispatch."
  (cond ((org-img-org-mode-p)
         (condition-case nil
             (oi/url uri)
           (error
            (org-img-dnd-fallback uri action))))
        ((eq major-mode 'dired-mode)
         (org-img-dired uri))
        ;; redirect to someone else
        (t
         (org-img-dnd-fallback uri action))))

(defun org-img-dnd-base64 (uri _action)
  (when (org-img-org-mode-p)
    (when (string-match "^data:image/png;base64," uri)
      (let* ((me (match-end 0))
             (filename (org-img--fullname
                        (substring-no-properties uri me (+ me 10))
                        "png")))
        (with-temp-buffer
          (insert (base64-decode-string (substring uri me)))
          (write-file filename))
        (org-img--insert-link filename filename)))))

(defun org-img-dired (uri)
  "Download URI to current directory."
  (raise-frame)
  (oi/url uri))

;;;###autoload
(defun org-img-enable ()
  "Enable org-img."
  (unless (eq (cdr (assoc "^\\(https?\\|ftp\\|file\\|nfs\\):" dnd-protocol-alist))
              'org-img-dnd)
    (setq dnd-protocol-alist
          `(("^\\(https?\\|ftp\\|file\\|nfs\\):" . org-img-dnd)
            ("^data:" . org-img-dnd-base64)
            ,@dnd-protocol-alist))))

(defun org-img-disable ()
  "Disable org-img."
  (rassq-delete-all 'org-img-dnd dnd-protocol-alist))

(org-img-enable)


(provide 'org-img)

;;; org-img.el ends here
