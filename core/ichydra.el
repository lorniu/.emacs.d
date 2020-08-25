;;; ichydra.el --- Hydras -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; plist: :pre :post :exit :foerign-keys :timeout :hint :bind :base-map

;; :color for shortcuts

;; red      |
;; blue     | :exit t
;; amaranth | :foreign-keys warn
;; teal     | :foreign-keys warn :exit t
;; pink     | :foreign-keys run


(defmacro -hydra-expr (child parent)
  `(progn (,(intern (concat "imdra-" (symbol-name child) "/body")))
          (hydra-push '(,(intern (concat "imdra-" (symbol-name parent) "/body"))))))


;;; Main Dashes

(defhydra imdra-overview (:foreign-keys warn :exit t)
  "\n"
  ("q" nil nil)
  ("I" info "Info" :column "Basic")
  ("i" (im/open-file-view (or (loco "x.share/emacs/INFO.org" t) (user-error "INFO file not found"))) "INFO.org")
  ("," (-hydra-expr buffer overview) "Buffer Edit")
  ("d" (-hydra-expr desktop overview) "Desktop Save")
  ("h" treemacs "Treemacs" :column "Programable")
  ("p" (-hydra-expr projectile overview) "Projectile")
  ("y" (-hydra-expr yasnippet overview) "Yasnippet")
  ("o" (-hydra-expr org overview) "Org-Mode" :column "Modes")
  ("e" elfeed "Elfeed")
  ("a" ascii-table-show "ASCII Table")
  ("s" (progn (yas-describe-tables) (pop-to-buffer "*YASnippet Tables*")) "Snippet Table")
  ("<f1>" nil nil)
  ("M-h" nil nil))


;;; Desktop Save

(defvar desktop-save-default-dir (locc ""))
(defhydra imdra-desktop (:exit t)
  "Desktop"
  ("l" (let ((default-directory desktop-save-default-dir)) (call-interactively 'desktop-change-dir)) "load")
  ("s" (let ((default-directory desktop-save-default-dir)) (call-interactively 'desktop-save)) "save")
  ("d" (desktop-save desktop-save-default-dir) "save default")
  ("r" desktop-revert "revert")
  ("c" desktop-clear "clear")
  ("q" hydra-pop nil))


;;; Super Window

(x super-window/e
   "Some extension for window operation."
   :config (ip/sw-mode 1))

(defun my-split-window-below ()
  (interactive)
  (call-interactively 'split-window-below)
  (imdra-window/body))

(defun my-split-window-right ()
  (interactive)
  (call-interactively 'split-window-right)
  (imdra-window/body))

(defun im/change-window-split-layout ()
  "Switch between vertical and horizontal split. It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defvar imdra-show-body nil)

(defhydra imdra-window
  (:body-pre
   (progn
     (setq hydra-is-helpful imdra-show-body)
     (set-face-attribute 'mode-line nil :slant 'italic))
   :pre
   (progn
     (setq hydra-is-helpful nil)
     (set-face-attribute 'mode-line nil :slant 'italic))
   :post
   (progn
     (setq hydra-is-helpful t)
     (set-face-attribute 'mode-line nil :slant 'normal))
   :after-exit
   (hydra-timeout 2))
  "\n"

  ;; better way?
  ("1" self-insert-command nil :exit t)
  ("2" self-insert-command nil :exit t)
  ("3" self-insert-command nil :exit t)
  ("4" self-insert-command nil :exit t)
  ("5" self-insert-command nil :exit t)
  ("6" self-insert-command nil :exit t)
  ("7" self-insert-command nil :exit t)
  ("8" self-insert-command nil :exit t)
  ("9" self-insert-command nil :exit t)
  ("0" self-insert-command nil :exit t)

  ("w" ace-window "Ace" :exit t)
  ("C-w" ace-window nil :exit t)
  ("o" ip/other-window- nil)
  ("O" other-window nil)

  ("s" window-swap-states nil)
  ("t" im/change-window-split-layout nil)

  ("b" im/switch-buffer+ "Switch Buffer" :exit t)
  ("C-o" imdra-window/body "Show This" :exit nil)
  ("?" imdra-window/body nil :exit nil))

(defhydra imdra-window-extra (:hint nil :columns 3)
  ("s" (if (> (length (window-list)) 2) (ace-swap-window) (window-swap-states)) "Swap Window")
  ("|" im/change-window-split-layout "Switch Layout")
  ("o" imdra-window/ip/other-window- nil :exit t)

  ("d" desktop-save "desktop save" :exit t)

  ("x" ip/other-window-skip-this "skip this (xX)" :exit t)
  ("X" ip/other-window-skip-regexp nil :exit t)

  ("=" balance-windows-area "balance area (=-)")
  ("-" balance-windows nil)

  ("j" (enlarge-window -2 nil) "resize (jkhl)")
  ("k" (enlarge-window  2 nil) nil)
  ("h" (enlarge-window -2 t)   nil)
  ("l" (enlarge-window  2 t)   nil)

  ("<left>" (progn (winner-undo) (setq this-command 'winner-undo)) "win-undo")
  ("<right>" winner-redo "win-redo"))

(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "C-x 3") 'my-split-window-right)
(global-set-key (kbd "C-x o") 'imdra-window/other-window)
(global-set-key (kbd "C-x O") 'imdra-window/ip/other-window+)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (let ((imdra-show-body t)) (imdra-window/body))))


;;; Super Buffer

(defhydra imdra-buffer (:columns 4 :exit t)
  "\n"

  ;; search
  ("t" go-translate "Google Translate")
  ("C-t" go-translate nil)
  ("y" go-translate-popup nil)
  ("T" youdao-dictionary-search-from-input "Youdao Translate")
  ("gt" engine/search-dict-iciba nil)
  ("gg" engine/search-google "Google")
  ("gc" engine/search-google-cn "Google-CN")
  ("F" engine/search-wolfram-alpha "Wolfram Alpha")
  ("G" engine/search-github "Github")
  ("S" engine/search-stackoverflow "StackOverflow")
  ("W" engine/search-wikipedia "Wikipedia")
  ("L" engine/search-arch-wiki "ArchWiki")
  ("i" engine/search-iconify "Iconify")

  ;; mark
  ("ml" (progn (back-to-indentation) (set-mark (line-end-position))) "line" :exit nil)
  ("mp" mark-page "page" :exit nil)
  ("mh" mark-paragraph "Paragraph" :exit nil)
  ("ms" mark-sexp "sexp" :exit nil)

  ;; expand mark
  ("," er/expand-region "Expand Region" :exit nil)
  ("." er/contract-region "Contract Region" :exit nil)
  ("C-," er/expand-region nil :exit nil)
  ("C-." er/contract-region nil :exit nil)

  ;; mark nav
  ("SPC" (progn (set-mark-command nil) (deactivate-mark)) "Mark Here")
  ("b" im/mark-ring+ "Mark Ring" :exit t)

  ;; misc
  ("w" im/wrap-current "Wrap word"))

(defhydra imdra-yank-pop
  (:hint nil :pre (hydra-set-property 'imdra-yank-pop :verbosity 0))
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("M-l" im/yank-pop+ "list" :exit t))

(defhydra imdra-dired (:hint nil :foreign-keys run)
  "
Mkdir(_+_)/_C_opy/_D_elete/_R_ename/_Z_ip
Ch_M_od/Ch_G_rp/Ch_O_wn/_S_ymlink/RelaSymlink(_Y_)
_m_ark/_t_oggleMark/_u_mark/_U_markAll/RegexpMark(％)

_A_ find regexp   _F_ind marked   _Q_ repl regexp
_i_ insert-subdir _$_ hide-subdir _w_ kill-subdir
_s_ sort          _=_ diff        _e_ ediff
_o_ open other    _l_ redisplay   _g_ revert buf
_(_ details       _)_ omit-mode
_6_ updir  _r_ to wdired  _y_ rsync  _z_ size

_._ toggle hydra   _?_ summary

T tag prefix   :e/s/e Encrypt/Sign/Verify
"
  ("+" dired-create-directory) ("C" dired-do-copy) ("D" dired-do-delete) ("R" dired-do-rename) ("Z" dired-do-compress)
  ("M" dired-do-chmod) ("G" dired-do-chgrp) ("O" dired-do-chown) ("S" dired-do-symlink) ("Y" dired-do-relsymlink) ;; admin
  ("F" dired-do-find-marked-files) ("A" dired-do-find-regexp) ("Q" dired-do-find-regexp-and-replace) ;; action
  ("v" dired-view-file) ("o" dired-find-file-other-window) ("l" dired-do-redisplay) ("g" revert-buffer) ;;view
  ("m" dired-mark) ("t" dired-toggle-marks) ("u" dired-unmark) ("U" dired-unmark-all-marks) ("E" dired-mark-extension) ;; mark
  ("i" dired-maybe-insert-subdir) ("w" dired-kill-subdir) ("$" dired-hide-subdir)  ;; subdir
  ("s" dired-sort-toggle-or-edit) ("(" dired-hide-details-mode) (")" dired-omit-mode) ("e" dired-ediff-files) ("=" diredp-ediff) ;; util
  ("z" idp/dired-du-size) ( "y" idp/dired-rsync) ("6" dired-up-directory) ("r" wdired-change-to-wdired-mode)
  ("." nil :color blue) ("?" dired-summary) ("q" nil))

(defhydra imdra-flycheck
  (:pre
   (progn (setq hydra-lv t) (flycheck-list-errors))
   :post
   (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
   :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

(defhydra imdra-next-error (global-map "C-x")
  "next-error"
  ("`" next-error "next")
  ("j" next-error "next" :bind nil)
  ("k" previous-error "previous" :bind nil))

(key-chord-define-global ",," 'imdra-buffer/body)
(global-set-key (kbd "C-,") 'imdra-buffer/body)
(global-set-key (kbd "C-y") 'imdra-yank-pop/yank)
(global-set-key (kbd "M-y") 'imdra-yank-pop/yank-pop)


;;; Reactangle

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :foerign-keys run
                                     :hint nil
                                     :post (deactivate-mark))
  "
 _w_ copy         _o_pen       copy-to-_R_egister
 _k_ kill         s_t_ring     _i_nsert-resister
 _y_ank           _d_elete     _e_xchange-point
 _N_umber-lines   _c_lear      _r_eset-region-mark
"
  ("k" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("d" delete-register)
  ("R" copy-rectangle-to-register)
  ("i" insert-register)
  ("q" nil))

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)


;;; Miscellaneous

(defhydra imdra-yasnippet (:exit t :hint nil)
  "
 ^^           ^YASnippets^   ^^
---------------------------------------
 Modes:     Load/Visit:  Actions:

 _g_lobal     _d_irectory    _i_nsert
 _m_inor      _f_ile         _t_ryout
 _e_xtra      _l_ist         _n_ew
 ^^           _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all)
  ("q" hydra-pop "exit"))

(defhydra imdra-projectile (:foreign-keys warn :exit t :columns 4)
  "Projectile"
  ("f"   projectile-find-file                "Find File")
  ("r"   projectile-recentf                  "Recent Files")
  ("z"   projectile-cache-current-file       "Cache Current File")
  ("x"   projectile-remove-known-project     "Remove Known Project")

  ("d"   projectile-find-dir                 "Find Directory")
  ("b"   projectile-switch-to-buffer         "Switch to Buffer")
  ("c"   projectile-invalidate-cache         "Clear Cache")
  ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

  ("o"   projectile-multi-occur              "Multi Occur")
  ("s"   projectile-switch-project           "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers")
  ("q"   hydra-pop "Cancel" :exit t))


(provide 'ichydra)

;;; ichydra.el ends here
