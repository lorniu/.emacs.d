;;; -*- lexical-binding: t -*-

;;  - chinese-py (builtin)
;;  - pyim (pure elisp) : for Emergency Usage
;;  - native fcitx5 + librime + rime-ice

;;; Code:

(defvar ic/ime-default "pyim")

(x pyim
   :ref ("tumashu/pyim" "redguardtoo/pyim-tsinghua-dict" "tumashu/pyim-greatdict")
   :config
   (when IS-G
     (setq pyim-page-tooltip 'posframe pyim-posframe-border-width 6))
   (if-let* ((dict (cl-find-if (lambda (d) (file-exists-p (locc (car d)))) ime:pyim-extra-dicts))
             (file (locc (car dict))))
       (setq pyim-extra-dicts `((:name ,(file-name-nondirectory file) :file ,file)))
     (message "No extra dicts found, download via `ime/pyim:download-dict' if necessary."))
   (define-key pyim-mode-map "." 'pyim-page-next-page)
   (define-key pyim-mode-map "," 'pyim-page-previous-page))

(defvar ime:pyim-extra-dicts
  `(("pyim/greatdict~80M.dict" . "https://raw.githubusercontent.com/tumashu/pyim-greatdict/refs/heads/master/pyim-greatdict.pyim.gz")
    ("pyim/tsinghua~2M.dict"   . "https://raw.githubusercontent.com/redguardtoo/pyim-tsinghua-dict/refs/heads/master/pyim-tsinghua-dict.pyim")))

(defun ime/pyim:download-dict (&optional dict)
  "Download extra dicts for pyim (only when necessary)."
  (interactive (list (completing-read "Dict to download: " ime:pyim-extra-dicts nil t)))
  (let ((url (alist-get dict ime:pyim-extra-dicts nil nil #'equal))
        (file (locc dict)))
    (unless url
      (user-error "Invalid dict name `%s'" dict))
    (if (file-exists-p file)
        (when (called-interactively-p 'any)
          (message "Dict `%s' already exists." file))
      (pdd-with-progress-reporter url
        (lambda (r)
          (let ((coding-system-for-write 'no-conversion)
                (target (concat file (if (string-suffix-p ".gz" url) ".gz"))))
            (write-region r nil target)
            (when (string-suffix-p ".gz" url)
              (shell-command (format "gzip -d %s" target)))
            (message "Dict `%s' is ready." file)))))))

(setq default-input-method ic/ime-default)



(defreference rime
  "https://rime.im/"
  "schema: iDvel/rime-ice"
  "gramma: amzxyz/RIME-LMDG"
  "config: https://github.com/LEOYoon-Tsaw/Rime_collections/blob/master/Rime_description.md")

(defvar ime:fcitx-user-data-dir "~/.config/fcitx5/")

(defvar ime:rime-user-data-dir
  (cond (IS-MAC "~/Library/Rime/")
        (t "~/.local/share/fcitx5/rime/")))

(cl-defmacro ime:with-file (file content &optional (enable? t))
  (declare (indent 1))
  (pcase-let* ((`(,category ,name) (split-string (format "%s" file) ":"))
               (fn `(defun ,(intern (format "ime/%s" file)) ()
                      (interactive)
                      (let ((dir ,(pcase category
                                    ("fcitx" 'ime:fcitx-user-data-dir)
                                    ("rime" '(im:ensure-dir ime:rime-user-data-dir)))))
                        (with-current-buffer (find-file (expand-file-name ,name dir))
                          (erase-buffer)
                          (insert ,content))))))
    (if (eq enable? t) fn `(when ,enable? ,fn))))

(ime:with-file fcitx:profile
  "[Groups/0]
Name=Default
Default Layout=us
DefaultIM=rime

[Groups/0/Items/0]
Name=rime
Layout=

[GroupOrder]
0=Default"
  IS-LINUX)

(ime:with-file fcitx:conf/classicui.conf
  "Vertical Candidate List=False
PerScreenDPI=False
Font=\"Noto Sans Mono 15\"
Theme=Material-Color-Red
DarkTheme=Material-Color-Yellow"
  IS-LINUX)

(ime:with-file rime:default.custom.yaml
  "patch:
  schema_list:
    - schema: rime_ice
    - schema: double_pinyin
  menu/page_size: 6
  ascii_composer/switch_key:
    Caps_Lock: clear
    Shift_L: commit_code
    Shift_R: inline_ascii
    Control_L: noop
    Control_R: commit_text
  key_binder/bindings:
    - { when: has_menu, accept: minus,  send: Page_Up }
    - { when: has_menu, accept: equal,  send: Page_Down }
    - { when: paging,   accept: comma,  send: Page_Up }
    - { when: has_menu, accept: period, send: Page_Down }
    - { when: composing, accept: Shift+Tab, send: Shift+Left }
    - { when: composing, accept: Tab, send: Shift+Right }
    - { when: composing, accept: Control+n, send: Down }
    - { when: composing, accept: Control+p, send: Up }
    - { when: composing, accept: Control+f, send: Right }
    - { when: composing, accept: Control+b, send: Left }
    - { when: composing, accept: Control+a, send: Home }
    - { when: composing, accept: Control+e, send: End }
    - { when: composing, accept: Control+d, send: Delete }
    - { when: composing, accept: Control+k, send: Shift+Delete }
    - { when: composing, accept: Control+h, send: BackSpace }
    - { when: composing, accept: Control+g, send: Escape }
    - { when: composing, accept: Control+v, send: Page_Down }
    - { when: composing, accept: Alt+v,     send: Page_Up }
    - { when: has_menu,  accept: Control+m, send: Return }
    - { when: composing, accept: Control+m, send: Return }
    - { when: always, accept: Control+Shift+f9, toggle: full_shape }
  switcher/hotkeys: [Control+Shift+F8]")

(ime:with-file rime:squirrel.custom.yaml
  "patch:
  style/candidate_list_layout: linear
  show_notifications_when: never"
  IS-MAC)

(ime:with-file rime:rime_ice.custom.yaml
  "patch:
  switches:
    - name: ascii_mode
      states: [中, A]
      reset: 1
    - name: ascii_punct
      states: [¥, $]
      reset: 0
    - name: traditionalization
      states: [简, 繁]
      reset: 0
    - name: emoji
      states: [💀, 😄]
      reset: 0
    - name: full_shape
      states: [半角, 全角]
      reset: 0
  grammar:
    language: wanxiang-lts-zh-hans
    collocation_max_length: 5
    collocation_min_length: 2
  translator/contextual_suggestions: true
  translator/max_homophones: 7
  translator/max_homographs: 7")

(ime:with-file rime:luna_pinyin.custom.yaml
  "patch:
  switches:
    - name: ascii_mode
      states: [ 中文, 西文 ]
      reset: 1
    - name: simplification
      states: [ 漢字, 汉字 ]
      reset: 1
    - name: full_shape
      states: [ 半角, 全角 ]
      reset: 0")

(defun ime/rime:wanxiang-lmdg.gram ()
  (interactive)
  (pdd-let*
      ((file (expand-file-name "wanxiang-lts-zh-hans.gram" ime:rime-user-data-dir))
       (md5sum1 (pdd "https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/md5sum.txt"
                  :done (lambda (r) (car (split-string r)))))
       (_ (await (if (file-exists-p file)
                     (message "File %s already exists" file)
                   (pdd-with-progress-reporter "https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/wanxiang-lts-zh-hans.gram"
                     (lambda (r)
                       (let ((coding-system-for-write 'no-conversion))
                         (write-region r nil file)))))))
       (md5sum2 (pdd-exec t `[md5sum ,file]
                  :done (lambda (r) (car (split-string r))))))
    (unless (equal (await md5sum1) (await md5sum2))
      (user-error "MD5SUM check failed, maybe you should delete %s and download again." file))
    (message "MD5SUM check: the gramma file is correct.")))

(defun ime/display-rime-installation ()
  (interactive)
  (im:with-current-view-buffer (get-buffer-create "*Input Method Tips*")
    :focus t :keywords '(("#.*" . 'font-lock-comment-face))
    (cond
     (IS-LINUX (insert
                "Install packages:

  pacman -S fcitx5-im fcitx5-rime rime-ice-git
  pacman -S fcitx5-material-color # skins: yay -Ss fcitx5-skin

Add to .xinitrc or .profile:

  export GTK_IM_MODULE=fcitx
  export QT_IM_MODULE=fcitx
  export SDL_IM_MODULE=fcitx
  export XMODIFIERS=@im=fcitx

Execute commands to generate files:

  M-x ime/rime:default.custom.yaml   # Global
  M-x ime/rime:rime_ice.custom.yaml  # Rime Schema
  M-x ime/rime:wanxiang-lmdg.gram    # Rime Gramma (Download)
  M-x ime/fcitx:profile              # Fcitx
  M-x ime/fcitx:conf/classicui.conf  # Fcitx Theme

Others:

  $ fcitx5-configtool
  $ fcitx5-remote -r

Then reboot, all will be ok."))

     (IS-MAC (insert
              "Install packages:

  brew install --cask squirrel
  rm -rf ~/Library/Rime
  git clone https://github.com/iDvel/rime-ice.git ~/Library/Rime --depth 1

Execute commands to generate files:

  M-x ime/rime:default.custom.yaml   # Global
  M-x ime/rime:squirrel.custom.yaml  # Style
  M-x ime/rime:rime_ice.custom.yaml  # Schema
  M-x ime/rime:wanxiang-lmdg.gram    # Gramma (Download)

That's all."))

     (t (insert "NO Tips for current OS.")))))

(defun ime/display-ziranma-shuangpin ()
  (interactive)
  (im:with-current-view-buffer (get-buffer-create "*ShuangPin/ZiRanMa*")
    :focus t :wc '(display-buffer-at-bottom (window-height . 0.2))
    :keywords '(("[A-Z]" . 'font-lock-keyword-face) ("^a.*" . 'font-lock-comment-face))
    (insert "
Q(iu) W(ia,ua) E(e) R(uan) T(ue,ve) Y(ing,uai) U(sh,u) I (ch,i) O(o,uo) P(un)\n
  A(a) S(iong,ong) D(iang,uang) F(en) G(eng) H(ang) J(an) K(ao) L(ai)\n
   Z(ei) X(ie) C(iao) V(zh,ui,v) B(ou) N(in) M(ian)\n\n
a aa | ang ah | e ee | eng eg | o oo | ai an ao ei en er ou")))

(when IS-LINUX
  (defun ime/reload-rime ()
    "Helper to reload/redeploy fcitx/rime."
    (interactive)
    (pdd-chain t
      (lambda () (pdd-exec [fcitx5-remote -c]))
      (lambda () (pdd-exec [rime_deploy --build]))
      (lambda () (pdd-exec [fcitx5-remote -c]))
      :fail (lambda (r) (message "> %s" r)))))
