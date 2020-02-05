;;; imod-ime.el --- Input Methods -*- lexical-binding: t -*-

;; Emacs native:
;;
;;  - chinese-py (builtin)
;;  - pyim (pure elisp)
;;  - emacs-rime (dynamic + librime)
;;
;; OS native, auto-switch:
;;
;;  - emacs-smart-input-source.el (Smart switch OS ims)
;;
;; Get librime:
;;
;;  - pacman -S librime-or-fcitx5-rime (arch)
;;
;;  - scoop install gcc   (windows)
;;    scoop bucket add wsw0108 https://github.com/wsw0108/scoop-bucket.git
;;    scoop install librime
;;

;;; Code:

(defvar ic/ime-default "pyim")

(defvar ic/ism-default nil)

(x pyim
   :ref "tumashu/pyim"
   :defer-config
   (when IS-G
     (setq pyim-page-tooltip 'posframe)
     (setq pyim-posframe-border-width 6))
   (pyim-basedict-enable))

(x rime
   "1) install librime 2) set `ic/ime-default' to 'rime'."
   :ref ("DogLooksGood/emacs-rime"
         "Install: DogLooksGood/emacs-rime/blob/master/INSTALLATION.org"
         "Rime Guide: rime/home/wiki/CustomizationGuide")
   :bind ((rime-active-mode-map ("<tab>" . rime-inline-ascii)))
   :custom
   (rime-title . "R ")
   :init
   (when IS-LINUX
     (setq rime-user-data-dir "~/.local/share/fcitx5/rime/"))
   (setq rime-inline-ascii-trigger 'shift-l)
   (setq mode-line-mule-info '((:eval (rime-lighter))))
   (if IS-G (setq rime-show-candidate 'posframe)))

(x sis
   "Auto-switch OS input methods.\n
To enable, set ic/ism-default like:\n
  Linux: (list 1 2 'fcitx5)) (list nil rime 'native)
  Windows: (list 1033 2052 'im-select)"
   :ref ("laishulu/emacs-smart-input-source"
         "im-select.exe: daipeihust/im-select")
   :if ic/ism-default
   :init
   (apply 'sis-ism-lazyman-config ic/ism-default)
   (sis-global-cursor-color-mode t)
   (sis-global-respect-mode t)
   (sis-global-context-mode t)
   (sis-global-inline-mode t))

(setq default-input-method ic/ime-default)



(defmacro is-ime-with-rime-file (file content)
  (declare (indent 1))
  `(defun ,(intern (format "is/ime-rime:%s" file)) ()
     (interactive)
     (with-current-buffer (find-file (expand-file-name ,(symbol-name file) (im-ensure-dir rime-user-data-dir)))
       (goto-char (point-max))
       (insert ,content))))

(is-ime-with-rime-file default.custom.yaml
  "patch:
  menu/page_size: 6
  switcher/hotkeys:
    - Control+grave
  ascii_composer/switch_key:
    Caps_Lock: clear
    Shift_L: commit_code
    Shift_R: inline_ascii
    Control_L: noop
    Control_R: noop")

(is-ime-with-rime-file weasel.custom.yaml
  "patch:
  \"style/color_scheme\": google # 皮肤风格
  \"style/layout/border_width\": 0
  \"style/layout/border\": 0
  \"style/horizontal\": true
  #\"style/font_face\": Microsoft YaHei
  \"style/font_point\": 12")

(is-ime-with-rime-file luna_pinyin.custom.yaml
  "patch:
  switches:
    - name: ascii_mode
      reset: 0
      states: [ 中文, 西文 ]
    - name: full_shape
      states: [ 半角, 全角 ]
    - name: simplification
      reset: 1
      states: [ 漢字, 汉字 ]")

(provide 'imod-ime)

;;; imod-ime.el ends here
