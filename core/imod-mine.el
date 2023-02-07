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


(defvar im:speak-buffer-task nil)

(defvar im:speak-buffer-engine 'local)

(defvar im:speak-buffer-interval 0.3)

(defvar im:speak-buffer-face 'font-lock-warning-face)

(defvar-keymap im:speak-buffer-map
  "<mouse-3>" 'im/speak-buffer-interrupt "C-g" 'im/speak-buffer-interrupt)

(defun im/speak-buffer-interrupt ()
  (interactive)
  (when im:speak-buffer-task
    (pdd-signal im:speak-buffer-task 'cancel)
    (setq im:speak-buffer-task nil)))

(defun im/speak-buffer-by-paragraphs ()
  (interactive)
  (require 'go-translate)
  (im/speak-buffer-interrupt)
  (let ((buf (current-buffer))
        (ov (make-overlay 1 1 nil nil t)))
    (overlay-put ov 'face im:speak-buffer-face)
    (overlay-put ov 'keymap im:speak-buffer-map)
    (letrec ((play-from
              (lambda (pos)
                (if-let* ((bounds (and (goto-char pos) (not (eobp))
                                       (bounds-of-thing-at-point 'paragraph)))
                          (beg (max pos (car bounds)))
                          (end (save-excursion
                                 (goto-char (cdr bounds))
                                 (skip-chars-forward " \t\n\r\f")
                                 (point))))
                    ;; --- play paragraph ---
                    (pdd-chain (buffer-substring-no-properties beg (cdr bounds))
                      (lambda (text) ; fetch audio data
                        (setq im:speak-buffer-task
                              (gt-speech im:speak-buffer-engine text 'zh #'identity)))
                      (lambda (data) ; highlight and play
                        (with-current-buffer buf
                          (move-overlay ov beg end)
                          (setq im:speak-buffer-task (gt-play-audio data))))
                      (lambda (_) ; unhighlight and play next
                        (with-current-buffer buf
                          (move-overlay ov end end)
                          (setq im:speak-buffer-task
                                (pdd-delay im:speak-buffer-interval
                                  (lambda ()
                                    ;; post: play next paragraph
                                    (with-current-buffer buf (funcall play-from end))
                                    ;; post: decoupe promise chain
                                    nil)))))
                      :fail
                      (lambda (r)
                        (with-current-buffer buf (delete-overlay ov))
                        (setq im:speak-buffer-task nil)
                        (unless (string-match-p "cancel" (format "%s" r))
                          (message "Speak buffer error: %s" r))))
                  ;; --- reach the end ---
                  (delete-overlay ov)
                  (setq im:speak-buffer-task nil)
                  (message "Speak buffer finished.")))))
      ;; play from current point
      (funcall play-from (point)))))
