;;; alert.el --- Growl-style notification system for Emacs  -*- lexical-binding: t; -*-

;; X-URL: https://github.com/jwiegley/alert


;;; Commentary:

;; Alert is a Growl-workalike notification system.
;;
;;   (alert "This is an alert" :title "My Alert" :category 'debug :severity 'high :timeout 33)
;;
;; Builtin alert styles:
;;
;;  - ignore/log4e/message/fringe/mode/gntp/growl
;;  - momentary/libnotify/notifications/notifier/osx/x11/toaster
;;
;;;
;;
;; Changed so lot, can't merge to upstream. imfine.
;;
;;; Code:

(require 'cl-lib)

(defgroup alert nil
  "Notification system for Emacs similar to Growl"
  :group 'emacs)

(defcustom alert-default-style 'message
  "The style to use if no rules match in the current configuration.
If a configured rule does match an alert, this style is not used;
it is strictly a fallback."
  :type 'symbol
  :group 'alert)

(defcustom alert-default-icon (concat data-directory "images/icons/hicolor/scalable/apps/emacs.svg")
  "Filename of default icon to show for libnotify-alerts."
  :type 'string
  :group 'alert)

(defcustom alert-default-title nil
  "Default title for alert window. if nil, use current-buffer's name instead."
  :type 'string
  :group 'alert)

(defcustom alert-log-level
  'normal
  "Minimum level of messages to log."
  :type 'symbol
  :group 'alert)

(defcustom alert-default-fade-time 5
  "If not idle, alerts disappear after this many seconds.
The amount of idle time is governed by `alert-persist-idle-time'."
  :type 'integer
  :group 'alert)

(defcustom alert-reveal-idle-time 15
  "If idle this many seconds, rules will match the `idle' property."
  :type 'integer
  :group 'alert)

(defcustom alert-persist-idle-time 900
  "If idle this many seconds, all alerts become persistent.
This can be overridden with the Never Persist option (:never-persist)."
  :type 'integer
  :group 'alert)

(defcustom alert-log-messages t
  "If non-nil, all alerts are logged to the *Alerts* buffer."
  :type 'boolean
  :group 'alert)

(defcustom alert-severity-faces
  '((urgent   . alert-urgent-face)
    (high     . alert-high-face)
    (moderate . alert-moderate-face)
    (normal   . alert-normal-face)
    (low      . alert-low-face)
    (trivial  . alert-trivial-face))
  "Faces associated by default with alert severities."
  :type '(alist :key-type symbol :value-type color)
  :group 'alert)

(defcustom alert-severity-colors
  '((urgent   . "red")
    (high     . "orange")
    (moderate . "yellow")
    (normal   . "green")
    (low      . "blue")
    (trivial  . "purple"))
  "Colors associated by default with alert severities.
This is used by styles external to Emacs that don't understand faces."
  :type '(alist :key-type symbol :value-type color)
  :group 'alert)

(defcustom alert-log-severity-functions
  '((urgent   . alert--log-fatal)
    (high     . alert--log-error)
    (moderate . alert--log-warn)
    (normal   . alert--log-info)
    (low      . alert--log-debug)
    (trivial  . alert--log-trace))
  "Log4e logging functions."
  :type '(alist :key-type symbol :value-type color)
  :group 'alert)

(defvar alert-styles nil)

(defvar alert-active-alerts nil)



(defface alert-urgent-face
  '((t (:foreground "Red" :bold t)))
  "Urgent alert face."
  :group 'alert)

(defface alert-high-face
  '((t (:foreground "Dark Orange" :bold t)))
  "High alert face."
  :group 'alert)

(defface alert-moderate-face
  '((t (:foreground "Gold" :bold t)))
  "Moderate alert face."
  :group 'alert)

(defface alert-normal-face
  '((t))
  "Normal alert face."
  :group 'alert)

(defface alert-low-face
  '((t (:foreground "Dark Blue")))
  "Low alert face."
  :group 'alert)

(defface alert-trivial-face
  '((t (:foreground "Dark Violet")))
  "Trivial alert face."
  :group 'alert)



(defsubst alert-encode-string (str)
  (encode-coding-string str (keyboard-coding-system)))

(defun alert-define-style (name &rest plist)
  "Define a new style for notifying the user of alert messages.
To create a new style, you need to at least write a \"notifier\",
which is a function that receives the details of the alert.
These details are given in a plist which uses various keyword to
identify the parts of the alert.  Here is a prototypical style
definition:

\(alert-define-style 'style-name :title \"My Style's title\"
                    :notifier
                    (lambda (info)
                      ;; The message text is :message
                      (plist-get info :message)
                      ;; The :title of the alert
                      (plist-get info :title)
                      ;; The :category of the alert
                      (plist-get info :category)
                      ;; The major-mode this alert relates to
                      (plist-get info :mode)
                      ;; The buffer the alert relates to
                      (plist-get info :buffer)
                      ;; Severity of the alert.  It is one of:
                      ;;   `urgent'
                      ;;   `high'
                      ;;   `moderate'
                      ;;   `normal'
                      ;;   `low'
                      ;;   `trivial'
                      (plist-get info :severity)
                      ;; Whether this alert should persist, or fade away
                      (plist-get info :persistent)
                      ;; Data which was passed to `alert'.  Can be
                      ;; anything.
                      (plist-get info :data))

                    ;; Removers are optional.  Their job is to remove
                    ;; the visual or auditory effect of the alert.
                    :remover
                    (lambda (info)
                      ;; It is the same property list that was passed to
                      ;; the notifier function.
                      ))"
  (add-to-list 'alert-styles (cons name plist)))

(defun alert-buffer-status (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((wind (get-buffer-window)))
      (if wind
          (if (eq wind (selected-window))
              (if (and (current-idle-time)
                       (> (float-time (current-idle-time))
                          alert-reveal-idle-time))
                  'idle
                'selected)
            'visible)
        'buried))))

(defun alert-remove-when-active (remover info)
  (let ((idle-time (and (current-idle-time)
                        (float-time (current-idle-time)))))
    (cond
     ((and idle-time (> idle-time alert-persist-idle-time)))
     ((and idle-time (> idle-time alert-reveal-idle-time))
      (run-with-timer (plist-get info :timeout) nil
                      #'alert-remove-when-active remover info))
     (t
      (funcall remover info)))))

(defun alert-remove-on-command ()
  (let (to-delete)
    (dolist (alert alert-active-alerts)
      (when (eq (current-buffer) (nth 0 alert))
        (push alert to-delete)
        (if (nth 2 alert)
            (funcall (nth 2 alert) (nth 1 alert)))))
    (dolist (alert to-delete)
      (setq alert-active-alerts (delq alert alert-active-alerts)))))

(defun alert-send-notification (alert-buffer info style-def &optional persist never-per)
  (let ((notifier (plist-get style-def :notifier)))
    (if notifier
        (funcall notifier info)))
  (let ((remover (plist-get style-def :remover)))
    (add-to-list 'alert-active-alerts (list alert-buffer info remover))
    (with-current-buffer alert-buffer
      (add-hook 'post-command-hook #'alert-remove-on-command nil t))
    (if (and remover (or (not persist) never-per))
        (run-with-timer (plist-get info :timeout) nil
                        #'alert-remove-when-active
                        remover info))))

;;;###autoload
(cl-defun alert (message &key (severity 'normal) title icon category
                         style buffer mode data persistent never-persist
                         id (timeout alert-default-fade-time))
  "Alert the user that something has happened.
MESSAGE is what the user will see.  You may also use keyword
arguments to specify additional details.  Here is a full example:

\(alert \"This is a message\"
       :severity \\='high          ;; The default severity is `normal'
       :title \"Title\"           ;; An optional title
       :timeout 5
       :category \\='example       ;; A symbol to identify the message
       :mode \\='text-mode         ;; Normally determined automatically
       :buffer (current-buffer) ;; This is the default
       :data nil                ;; Unused by alert.el itself
       :persistent nil          ;; Force the alert to be persistent;
                                ;; it is best not to use this
       :never-persist nil       ;; Force this alert to never persist
       :id \\='my-id)              ;; Used to replace previous message of
                                ;; the same id in styles that support it
       :style \\='fringe)          ;; Force a given style to be used;
                                ;; this is only for debugging!

If no :title is given, the buffer-name of :buffer is used.  If
:buffer is nil, it is the current buffer at the point of call.

:data is an opaque value which modules can pass through to their
own styles if they wish.

Here are some more typical examples of usage:

  ;; This is the most basic form usage
  (alert \"This is an alert\")

  ;; You can adjust the severity for more important messages
  (alert \"This is an alert\" :severity \\='high)

  ;; Or decrease it for purely informative ones
  (alert \"This is an alert\" :severity \\='trivial)

  ;; Alerts can have optional titles.  Otherwise, the title is the
  ;; buffer-name of the (current-buffer) where the alert originated.
  (alert \"This is an alert\" :title \"My Alert\")

  ;; Further, alerts can have categories.  This allows users to
  ;; selectively filter on them.
  (alert \"This is an alert\" :title \"My Alert\"
         :category \\='some-category-or-other)"
  (cl-destructuring-bind
      (alert-buffer current-major-mode current-buffer-status current-buffer-name)
      (with-current-buffer (or buffer (current-buffer))
        (list (current-buffer) (or mode major-mode) (alert-buffer-status) (buffer-name)))
    (let ((base-info (list :message message
                           :title (or title alert-default-title current-buffer-name)
                           :icon icon
                           :severity severity
                           :category category
                           :buffer alert-buffer
                           :mode current-major-mode
                           :id id
                           :timeout (or timeout alert-default-fade-time)
                           :data data))
          (style (or style alert-default-style)))

      (if alert-log-messages (alert-log-notify base-info))
      (alert-send-notification alert-buffer base-info (cdr (assq style alert-styles))))))


;;; Ignore

(alert-define-style 'ignore :title "Don't display alerts")


;;; log4e

(require 'log4e nil t)

(defun alert-log-notify (info)
  (let* ((mes (plist-get info :message))
         (sev (plist-get info :severity))
         (len (length mes))
         (func (cdr (assoc sev alert-log-severity-functions))))
    (if (not (featurep 'log4e))
        (alert-legacy-log-notify mes sev len)
      ;; when we get here you better be using log4e or have your logging functions defined
      (unless (fboundp func)
        (when (fboundp 'log4e:deflogger)
          (log4e:deflogger "alert" "%t [%l] %m" "%H:%M:%S")
          (when (functionp 'alert--log-set-level)
            (alert--log-set-level alert-log-level)))
        (alert--log-enable-logging))
      (when (fboundp func)
        (apply func (list mes))))))

(defun alert-legacy-log-notify (mes sev len)
  (with-current-buffer
      (get-buffer-create "*Alerts*")
    (goto-char (point-max))
    (insert (format-time-string "%H:%M %p - "))
    (insert mes)
    (set-text-properties (- (point) len) (point)
                         (list 'face (cdr (assq sev
                                                alert-severity-faces))))
    (insert ?\n)))

(defun alert-log-clear (info)
  (if (functionp 'alert--log-clear-log)
      (alert--log-clear-log)
    (if (bufferp "*Alerts*")
        (with-current-buffer
            (get-buffer-create "*Alerts*")
          (goto-char (point-max))
          (insert (format-time-string "%H:%M %p - ")
                  "Clear: " (plist-get info :message)
                  ?\n)))))

(alert-define-style 'log4e :title "Log to *Alerts* buffer"
                    :notifier #'alert-log-notify
                    ;;:remover #'alert-log-clear
                    )


;;; message

(defun alert-message-notify (info)
  (message "%s" (plist-get info :message)))

(defun alert-message-remove (_info)
  (message ""))

(alert-define-style 'message :title "Display message in minibuffer"
                    :notifier #'alert-message-notify
                    :remover #'alert-message-remove)


;;; momentary

(defun alert-momentary-notify (info)
  (save-excursion
    (with-current-buffer (or (plist-get info :buffer) (current-buffer))
      (momentary-string-display
       (format "%s: %s (%s/%s/%s)"
               (or (plist-get info :title) "untitled")
               (or (plist-get info :message) "no message")
               (or (plist-get info :severity) "no priority")
               (or (plist-get info :category) "no category")
               (or (plist-get info :mode) "no mode"))
       (progn
         (beginning-of-line)
         (point))))))

(alert-define-style 'momentary :title "Display message momentarily in buffer"
                    :notifier #'alert-momentary-notify
                    ;; explicitly, we don't need a remover
                    :remover #'ignore)


;;; fringe

(copy-face 'fringe 'alert-saved-fringe-face)

(defun alert-fringe-notify (info)
  (set-face-background 'fringe (cdr (assq (plist-get info :severity) alert-severity-colors))))

(defun alert-fringe-restore (_info)
  (copy-face 'alert-saved-fringe-face 'fringe))

(alert-define-style 'fringe :title "Change the fringe color"
                    :notifier #'alert-fringe-notify
                    :remover #'alert-fringe-restore)


;;; mode-line

(defun alert-mode-line-notify (info)
  (copy-face 'mode-line 'alert-saved-mode-line-face)
  (set-face-background 'mode-line (cdr (assq (plist-get info :severity) alert-severity-colors)))
  (set-face-foreground 'mode-line "white"))

(defun alert-mode-line-restore (_info)
  (copy-face 'alert-saved-mode-line-face 'mode-line))

(alert-define-style 'mode-line :title "Change the mode-line color"
                    :notifier #'alert-mode-line-notify
                    :remover #'alert-mode-line-restore)


;;; gntp

(defvar alert-gntp-icon "http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/emacs/etc/images/icons/hicolor/48x48/apps/emacs.png")

(defun alert-gntp-notify (info)
  (require 'gntp)
  (gntp-notify 'alert
               (alert-encode-string (plist-get info :title))
               (alert-encode-string (plist-get info :message))
               gntp-server nil
               (number-to-string
                (cdr (assq (plist-get info :severity)
                           alert-growl-priorities)))
               (if (eq (plist-get info :icon) nil)
                   alert-gntp-icon
                 (plist-get info :icon)))
  (alert-message-notify info))

(alert-define-style 'gntp :title "Notify using gntp" :notifier #'alert-gntp-notify)


;;; growl

(defvar alert-growl-command (executable-find "growlnotify"))

(defvar alert-growl-priorities
  '((urgent   . 2)
    (high     . 2)
    (moderate . 1)
    (normal   . 0)
    (low      . -1)
    (trivial  . -2))
  "A mapping of alert severities onto Growl priority values.")

(defun alert-growl-notify (info)
  (if alert-growl-command
      (let* ((title (alert-encode-string (plist-get info :title)))
             (priority (number-to-string
                        (cdr (assq (plist-get info :severity)
                                   alert-growl-priorities))))
             (args
              (cl-case system-type
                (windows-nt (mapcar
                             (lambda (lst) (apply #'concat lst))
                             `(
                               ;; http://www.growlforwindows.com/gfw/help/growlnotify.aspx
                               ("/i:" ,(file-truename (concat invocation-directory "../share/icons/hicolor/48x48/apps/emacs.png")))
                               ("/t:" ,title)
                               ("/p:" ,priority))))
                (t (list
                    "--appIcon"  "Emacs"
                    "--name"     "Emacs"
                    "--title"    title
                    "--priority" priority)))))
        (if (and (plist-get info :persistent)
                 (not (plist-get info :never-persist)))
            (cl-case system-type
              (windows-nt (nconc args (list "/s:true")))
              (t (nconc args (list "--sticky")))))
        (let ((message (alert-encode-string (plist-get info :message))))
          (cl-case system-type
            (windows-nt (nconc args (list message)))
            (t (nconc args (list "--message" message)))))
        (apply #'call-process alert-growl-command nil nil nil args))
    (alert-message-notify info)))

(alert-define-style 'growl :title "Notify using Growl" :notifier #'alert-growl-notify)


;;; libnotify

(defvar alert-libnotify-command (executable-find "notify-send"))

(defvar alert-libnotify-additional-args nil)

(defvar alert-libnotify-priorities
  '((urgent   . critical)
    (high     . critical)
    (moderate . normal)
    (normal   . normal)
    (low      . low)
    (trivial  . low))
  "A mapping of alert severities onto libnotify priority values.")

(defun alert-libnotify-notify (info)
  (if alert-libnotify-command
      (let* ((args
              (append
               (list "--icon"     (or (plist-get info :icon)
                                      alert-default-icon)
                     "--app-name" "Emacs"
                     "--urgency"  (let ((urgency (cdr (assq
                                                       (plist-get info :severity)
                                                       alert-libnotify-priorities))))
                                    (if urgency
                                        (symbol-name urgency)
                                      "normal")))
               (copy-tree alert-libnotify-additional-args)))
             (category (plist-get info :category)))
        (nconc args
               (list "--expire-time"
                     (number-to-string
                      (* 1000 ; notify-send takes msecs
                         (if (and (plist-get info :persistent)
                                  (not (plist-get info :never-persist)))
                             0 ; 0 indicates persistence
                           alert-fade-time)))))
        (when category
          (nconc args
                 (list "--category"
                       (cond ((symbolp category)
                              (symbol-name category))
                             ((stringp category) category)
                             ((listp category)
                              (mapconcat (if (symbolp (car category))
                                             #'symbol-name
                                           #'identity)
                                         category ","))))))
        (nconc args (list
                     (alert-encode-string (plist-get info :title))
                     (alert-encode-string (plist-get info :message))))
        (apply #'call-process alert-libnotify-command nil
               (list (get-buffer-create " *libnotify output*") t) nil args))
    (alert-message-notify info)))

(alert-define-style 'libnotify :title "Notify using libnotify" :notifier #'alert-libnotify-notify)


;;; dunstify

(defvar alert-dunstify-command (executable-find "dunstify"))

(defun alert-dunstify-notify (info)
  (if alert-dunstify-command
      (let* ((id (plist-get info :id))
             (args (list (alert-encode-string (plist-get info :title))
                         (alert-encode-string (plist-get info :message))
                         "-i" (or (plist-get info :icon) alert-default-icon)
                         "-a" "Emacs"
                         "-u" (let ((urgency (cdr (assq
                                                   (plist-get info :severity)
                                                   alert-libnotify-priorities))))
                                (if urgency (symbol-name urgency) "normal"))
                         "-t" (number-to-string
                               (* 1000 (if (and (plist-get info :persistent) (not (plist-get info :never-persist))) 0 (plist-get info :timeout)))))))
        (if id (nconc args (list "-r" (format "%s" id))))
        (apply #'call-process alert-dunstify-command nil
               (list (get-buffer-create " *dunstify output*") t) nil args))
    (alert-message-notify info)))

(alert-define-style 'dunstify :title "Notify using dunstify" :notifier #'alert-dunstify-notify)


;;; notifications

(defvar alert-notifications-priorities '((urgent   . critical)
                                         (high     . critical)
                                         (moderate . normal)
                                         (normal   . normal)
                                         (low      . low)
                                         (trivial  . low))
  "A mapping of alert severities onto Growl priority values.")

(defvar alert-notifications-ids (make-hash-table :test #'equal)
  "Internal store of notification ids returned by the `notifications' backend.
Used for replacing notifications with the same id.  The key is
the value of the :id keyword to `alert'.  An id is only stored
here if there `alert' was called with an :id keyword and handled
by the `notifications' style.")

(defun alert-notifications-notify (info)
  "Show the alert defined by INFO with `notifications-notify'."
  (require 'notifications nil t)
  (let ((id (notifications-notify :title (plist-get info :title)
                                  :body  (plist-get info :message)
                                  :app-icon (plist-get info :icon)
                                  :timeout (if (plist-get info :persistent) 0 -1)
                                  :replaces-id (gethash (plist-get info :id) alert-notifications-ids)
                                  :urgency (cdr (assq (plist-get info :severity)
                                                      alert-notifications-priorities))
                                  :actions '("default" "Open corresponding buffer")
                                  :on-action (lambda (_ action)
                                               (when (string= action "default")
                                                 (switch-to-buffer (plist-get info :buffer) 'norecord))))))
    (when (plist-get info :id)
      (puthash (plist-get info :id) id alert-notifications-ids)))
  (alert-message-notify info))

(defun alert-notifications-remove (info)
  "Remove the `notifications-notify' message based on INFO :id."
  (let ((id (and (plist-get info :id)
                 (gethash (plist-get info :id) alert-notifications-ids))))
    (when id
      (notifications-close-notification id)
      (remhash (plist-get info :id) alert-notifications-ids))))

(alert-define-style 'notifications :title "Notify using notifications"
                    :notifier #'alert-notifications-notify)


;;; notifier

(defvar alert-notifier-command (executable-find "terminal-notifier")
  "Path to the terminal-notifier command.")

(defvar alert-notifier-default-icon (concat data-directory "images/icons/hicolor/128x128/apps/emacs.png")
  "Filename of default icon to show for terminal-notifier alerts.")

(defun alert-notifier-notify (info)
  (if alert-notifier-command
      (let ((args
             (list "-title"   (alert-encode-string (plist-get info :title))
                   "-appIcon" (or (plist-get info :icon) alert-notifier-default-icon)
                   "-message" (alert-encode-string (plist-get info :message)))))
        (apply #'call-process alert-notifier-command nil nil nil args))
    (alert-message-notify info)))

(alert-define-style 'notifier :title "Notify using terminal-notifier"
                    :notifier #'alert-notifier-notify)


;;; osx-notifier

(defun alert-osx-notifier-notify (info)
  ;; Use built-in AppleScript support when possible.
  (if (fboundp 'do-applescript)
      (do-applescript (format "display notification %S with title %S"
                              (alert-encode-string (plist-get info :message))
                              (alert-encode-string (plist-get info :title))))
    (apply #'call-process "osascript" nil nil nil "-e"
           (list (format "display notification %S with title %S"
                         (alert-encode-string (plist-get info :message))
                         (alert-encode-string (plist-get info :title)))))))

(alert-define-style 'osx-notifier :title "Notify using native OSX notification" :notifier #'alert-osx-notifier-notify)


;;; x11

(defun alert-frame-notify (info)
  (let ((buf (plist-get info :buffer)))
    (if (eq (alert-buffer-status buf) 'buried)
        (let ((current-frame (selected-frame)))
          (with-selected-frame
              (make-frame '((width                . 80)
                            (height               . 20)
                            (top                  . -1)
                            (left                 . 0)
                            (left-fringe          . 0)
                            (right-fringe         . 0)
                            (tool-bar-lines       . nil)
                            (menu-bar-lines       . nil)
                            (vertical-scroll-bars . nil)
                            (unsplittable         . t)
                            (has-modeline-p       . nil)
                            (minibuffer           . nil)))
            (switch-to-buffer buf 'norecord)
            ;;(set (make-local-variable 'mode-line-format) nil)
            (nconc info (list :frame (selected-frame))))
          (select-frame current-frame)))))

(defun alert-frame-remove (info)
  (unless (eq this-command 'handle-switch-frame)
    (delete-frame (plist-get info :frame) t)))

(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for FRAME to ARG.

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency
setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            source nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current Emacs frame as requiring urgent attention.

With non-nil ARG, remove the urgency flag (which might or might
not change display, depending on the window manager)."
  (interactive "P")
  (let ((frame (car (car (cdr (current-frame-configuration))))))
    (x-urgency-hint frame (not arg))))

(defun alert-x11-notify (_info)
  "Call `x-urgent'."
  (x-urgent))

(alert-define-style 'x11 :title "Set the X11 window property" :notifier #'alert-x11-notify)


;;; toaster

(defvar alert-toaster-default-icon
  (let ((exec-bin (executable-find "emacs")))
    (cond (exec-bin
           (concat (file-name-directory exec-bin) "../share/icons/hicolor/128x128/apps/emacs.png"))
          (t nil))))

(defvar alert-toaster-command (executable-find "toast"))

(defun alert-toaster-notify (info)
  (if alert-toaster-command
      (let ((args (list
                   "-t" (alert-encode-string (plist-get info :title))
                   "-m" (alert-encode-string (plist-get info :message))
                   "-p" (expand-file-name (or (plist-get info :icon) alert-toaster-default-icon))
                   )))
        (apply #'call-process alert-toaster-command nil nil nil args))
    (alert-message-notify info)))

(alert-define-style 'toaster :title "Notify using Toaster" :notifier #'alert-toaster-notify)


;;; termux

(defvar alert-termux-command (executable-find "termux-notification"))

(defun alert-termux-notify (info)
  (if alert-termux-command
      (let ((args (nconc
                   (when (plist-get info :title)
                     (list "-t" (alert-encode-string (plist-get info :title))))
                   (list "-c" (alert-encode-string (plist-get info :message))))))
        (apply #'call-process alert-termux-command nil
               (list (get-buffer-create " *termux-notification output*") t)
               nil args))
    (alert-message-notify info)))

(alert-define-style 'termux :title "Notify using termux" :notifier #'alert-termux-notify)


;;; powershell

(defvar alert-powershell-script (expand-file-name (loce "bin/win/ps-notify.ps1"))
  "Script to send-notify under powershell.")

(defun powershell-send-notify (title message &optional timeout)
  (cl-flet ((normalize (input)
                       (encode-coding-string
                        (replace-regexp-in-string "'" "`" input) (keyboard-coding-system))))
    (let ((cmd (format "powershell %s -title '%s' -message '%s' -timeout %d"
                       alert-powershell-script
                       (normalize title)
                       (normalize message)
                       (or timeout alert-default-fade-time))))
      (shell-command cmd))))

(defun alert-powershell-notify (info)
  (let ((title (plist-get info :title))
        (message (plist-get info :message))
        (timeout (plist-get info :timeout)))
    (powershell-send-notify title message timeout)))

(alert-define-style 'powershell :title "Notify using powershell under Windows" :notifier #'alert-powershell-notify)


(provide 'alert)

;;; alert.el ends here
