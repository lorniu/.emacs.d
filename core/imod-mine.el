;;; -*- lexical-binding: t -*-

;;; Code:

(xzz dropbox
  :ref ("https://www.dropbox.com/home"
        "https://www.dropbox.com/developers/apps"
        "https://www.dropbox.com/developers/documentation")
  :init (add-to-list 'file-name-handler-alist
                     `("\\`/db:" . dropbox-handler))
  :commands (dropbox-find dropbox-browser dropbox-handler))

(xzz go-translate
  :ref "lorniu/go-translate"
  :commands ( gt-do-translate gt-do-setup gt-do-speak
              gt-qrcode gt-setup gt-speak gt-translate ))

(xzz bilibili
  :ref "lorniu/bilibli.el"
  :commands ( bilibili-login bilibili-insert-popular
              bilibili-insert-ranking bilibili-insert-precious
              bilibili-insert-recommend bilibili-insert-upper-videos
              bilibili-insert-upper-season-videos bilibili-insert-favs
              bilibili-insert-search bilibili-fav-it bilibili-triple-it))


;;; Hi AI

(defvar im:hi-ai-prompts
  (list "润色、优化文本。返回最终结果"
        "请认真分析代码，修复存在的错误，并给出改进建议"))

(defvar im:hi-ai-models
  (list "deepseek-v3" "gpt-4o-mini" "deepseek-r1" "gemini-2.5-pro" "grok-3"))

(defvar im:hi-ai-last-model nil)

(defvar im:hi-ai-history nil)

(defun im/hi-ai-oneshot ()
  (interactive)
  (require 'go-translate)
  (let ((prompt nil)
        (model (or im:hi-ai-last-model (setq im:hi-ai-last-model (or (car im:hi-ai-models) gt-chatgpt-model)))))
    (cl-flet ((get-modes ()
                (cl-delete-duplicates
                 (append im:hi-ai-history im:hi-ai-prompts) :from-end t :test #'equal))
              (change-model (&optional prev)
                (let* ((pos (or (cl-position im:hi-ai-last-model im:hi-ai-models :test #'equal) -1))
                       (next (if prev (max 0 (1- pos)) (min (1- (length im:hi-ai-models)) (1+ pos)))))
                  (setq im:hi-ai-last-model (nth next im:hi-ai-models))
                  (setq model im:hi-ai-last-model)
                  (overlay-put (car (overlays-at 1)) 'after-string im:hi-ai-last-model)))
              (delete-history ()
                (interactive)
                (when-let* ((current (im:completion-compat :current)))
                  (setq im:hi-ai-history (cl-remove current im:hi-ai-history :test #'equal))
                  (throw 'delete-and-retry 'del))))
      (while (progn
               (setq prompt
                     (catch 'delete-and-retry
                       (minibuffer-with-setup-hook
                           (lambda ()
                             (local-set-key (kbd "C-,") (lambda () (interactive) (change-model)))
                             (local-set-key (kbd "C-.") (lambda () (interactive) (change-model t)))
                             (local-set-key (kbd "C-c C-k") #'delete-history)
                             (overlay-put (make-overlay 1 9) 'after-string model)
                             (use-local-map (make-composed-keymap nil (current-local-map))))
                         (completing-read "Prompt (): " (get-modes) nil nil nil 'im:hi-ai-history))))
               (eq prompt 'del)))
      (gt-start (gt-translator
                 :taker (gt-taker
                         :text 'point :pick nil
                         :prompt (lambda (translator)
                                   (let ((text (car (oref translator text))))
                                     (oset translator text
                                           (list (if (string-blank-p text)
                                                     prompt
                                                   (let ((str (if (string-blank-p prompt) text
                                                                (format "%s\n\n内容如下:\n\n%s\n" prompt text))))
                                                     (if current-prefix-arg (read-string "Ensure: " str) str))))))
                                   (message "Processing...")))
                 :engines (gt-chatgpt-engine
                           :cache nil
                           :stream t
                           :model model
                           :timeout 300
                           :prompt #'identity)
                 :render (gt-buffer-render
                          :name (format "*hi-ai-%s*" model)
                          :mode 'markdown-mode
                          :init (lambda () (markdown-toggle-markup-hiding 1))
                          :dislike-header t
                          :dislike-source t
                          :window-config '((display-buffer-below-selected))))))))


;;; Speak buffer

(defvar im:speak-buffer-task nil)

(defvar im.speak-buffer-engine 'native)

(defvar im.speak-buffer-interval 0.1)

(defvar im.speak-buffer-step-pred #'<)

(defvar im.speak-buffer-step-action
  (lambda ()
    (let ((beg (point)))
      (forward-sentence)
      (skip-syntax-forward ".")
      ;; not too short
      (while (and (char-after) (not (eq (char-after) ?\n))
                  (< (- (point) beg) 20))
        (forward-sentence)
        (skip-syntax-forward ".")))))

(defvar im.speak-buffer-step-final nil)

(defvar im.speak-buffer-text-filter
  (lambda (text) (replace-regexp-in-string "[ \t\n\r]" "" text)))

(defvar im:speak-buffer-face 'font-lock-warning-face)

(defvar-keymap im:speak-buffer-map
  "<mouse-3>" 'im/speak-buffer-interrupt "C-g" 'im/speak-buffer-interrupt)

(defun im/speak-buffer-interrupt ()
  (interactive)
  (when im:speak-buffer-task
    (pdd-signal im:speak-buffer-task 'cancel)
    (setq im:speak-buffer-task nil)))

(defun im/speak-buffer ()
  (interactive)
  (require 'go-translate)
  (im/speak-buffer-interrupt)

  (let ((buf (current-buffer))
        (ov (make-overlay 1 1 nil nil t)))
    (overlay-put ov 'face im:speak-buffer-face)
    (overlay-put ov 'keymap im:speak-buffer-map)

    (cl-labels
        ((play-from (pos)
           (if-let* ((bounds-list
                      (save-excursion
                        (goto-char pos)
                        (skip-chars-forward " \t\n\r")
                        (setq pos (point))
                        (cl-loop repeat 3 ; 3 = 1 + 2: read 1, cache 2
                                 for beg = (point) then (point)
                                 for end = (save-excursion (funcall im.speak-buffer-step-action) (point))
                                 while (funcall im.speak-buffer-step-pred beg end)
                                 collect (cons beg end)
                                 do (funcall im.speak-buffer-step-action))))
                     (text-list (mapcar
                                 (lambda (bds)
                                   (funcall (or im.speak-buffer-text-filter #'identity)
                                            (buffer-substring-no-properties (car bds) (cdr bds))))
                                 bounds-list))
                     (current (car bounds-list)) (gt-tts-cache-ttl 60))

               (pdd-chain (car text-list)
                 (lambda (text)
                   (with-current-buffer buf
                     ;; 0. scroll & highlight
                     (when-let* ((win (get-buffer-window buf))
                                 (idle (float-time (or (current-idle-time) 0))))
                       (when (or (not (eq (car (window-list)) win)) (> idle 5))
                         (if (not (pos-visible-in-window-p (cdr current) win))
                             ;; scroll only when the buffer is idle and not visible
                             (with-selected-window win (goto-char pos) (recenter t))
                           (goto-char pos))))
                     (move-overlay ov (car current) (cdr current))
                     (redisplay t)
                     ;; 1. prefetch nexts
                     (mapc (lambda (c)
                             (let ((pdd-fail #'ignore))
                               (gt-speech im.speak-buffer-engine c 'zh #'ignore)))
                           (cdr text-list))
                     ;; 2. play the current
                     (setq im:speak-buffer-task (gt-speech im.speak-buffer-engine text 'zh))))
                 (lambda (_)
                   (with-current-buffer buf
                     ;; 3. next loop
                     (pdd-cacher-clear gt-tts-cache-store)
                     (move-overlay ov (cdr current) (cdr current))
                     (setq im:speak-buffer-task
                           (pdd-delay (if (eq (char-after (cdr current)) ?\n)
                                          (* 3 im.speak-buffer-interval) ; more delay time for paragraph end
                                        im.speak-buffer-interval)
                             (lambda ()
                               ;; post: play next paragraph
                               (with-current-buffer buf (play-from (cdr current)))
                               ;; post: decoupe promise chain
                               nil)))))
                 :fail
                 (lambda (r)
                   (with-current-buffer buf (delete-overlay ov))
                   (setq im:speak-buffer-task nil)
                   (unless (string-match-p "cancel" (format "%s" r))
                     (message "Speak buffer error: %s" r))
                   (signal (car r) (cdr r))))

             ;; --- reach the end ---
             (delete-overlay ov)
             (setq im:speak-buffer-task nil)
             (if (functionp im.speak-buffer-step-final)
                 (funcall im.speak-buffer-step-final)
               (message "Speak buffer finished.")))))

      ;; play from current point
      (play-from (point)))))
