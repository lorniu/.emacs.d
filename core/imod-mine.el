;;; -*- lexical-binding: t -*-

;;; Code:

(xzz dropbox
  :ref ("https://www.dropbox.com/home"
        "https://www.dropbox.com/developers/apps"
        "https://www.dropbox.com/developers/documentation")
  :init (add-to-list 'file-name-handler-alist
                     `("\\`/db:" . dropbox-handler))
  :commands (dropbox-find dropbox-browser dropbox-handler))

(xzz gt
  :ref "lorniu/gt.el"
  :commands ( gt-translate gt-setup gt-speak gt-qrcode gt-setup ))

(xzz bilibili
  :ref "lorniu/bilibli.el"
  :commands ( bilibili-login bilibili-insert-popular
              bilibili-insert-ranking bilibili-insert-precious
              bilibili-insert-recommend bilibili-insert-upper-videos
              bilibili-insert-upper-season-videos bilibili-insert-favs
              bilibili-insert-search bilibili-fav-it bilibili-triple-it))

(xzz mpvi
  :ref "lorniu/mpvi"
  :config
  ;;(setq mpvi-ytdlp-extra-options '("--proxy" "socks5://127.0.0.1:11181"))
  (add-to-list 'savehist-additional-variables 'mpvi-play-history)
  (mpvi-websocket-start)
  :commands (mpvi-play mpvi-control mpvi-seek mpvi-insert mpvi-export mpvi-add-emms mpvi-add-playlist mpvi-org-link-init mpvi-websocket-start))

(xzz speak-buffer
  :ref "lorniu/speak-buffer.el"
  :commands ( speak-buffer speak-buffer-interrupt ))


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
  (require 'gt)
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
