;;; ic-hydra.el --- Hydras -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defmacro -hydra-expr (child parent)
  `(progn (,(intern (concat "imdra-" (symbol-name child) "/body")))
          (hydra-push '(,(intern (concat "imdra-" (symbol-name parent) "/body"))))))



;;; Indexes

(defhydra imdra-overview (:color teal)
  "\n"
  ("q" nil nil)
  ("i" info "Info" :column "Documentation")
  ("d" (-hydra-expr desktop overview) "Desktop Save" :column "Programable")
  ("p" (-hydra-expr projectile overview) "Projectile")
  ("y" (-hydra-expr yasnippet overview) "Yasnippet")
  ("o" (-hydra-expr org overview) "Org-Mode" :column "Modes")
  ("c" (-hydra-expr dash-counsel overview) "Dash(Counsel)" :column "Fast Way")
  ("l" (imdra-favors/body) "Favor Files")
  ("s" yas-describe-tables "Snippet helper")
  ("<f1>" nil nil))

(defhydra imdra-desktop (:color blue)
  "Desktop"
  ("l" desktop-change-dir "load")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("c" desktop-clear "clear")
  ("q" hydra-pop nil))

(defhydra imdra-dash-counsel (:columns 3 :exit t)
  "\nDash:"
  ("cw" counsel-colors-web "Color(Web)")
  ("ce" counsel-colors-emacs "Color(Emacs)")
  ("cu" counsel-unicode-char "Unicode Char")
  ("lf" list-faces-display "List Faces")
  ("lh" counsel-hydra-heads "Hydras")
  ("la" ascii-table-show "Ascii Table")
  ("ag" counsel-ag "Search with ag"))

(global-set-key (kbd "M-h") 'imdra-overview/body)
(key-chord-define-global ",c" 'imdra-dash-counsel/body)



;;; Super *Window*

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

  ("b" ivy-switch-buffer "Switch Buffer" :exit t)
  ("C-o" imdra-window/body "Show This" :exit nil)
  ("?" imdra-window/body nil :exit nil))

(defhydra imdra-window-extra (:hint nil)
  ("<f1>" imdra-overview/body nil :exit t)

  ("s" window-swap-states "swap")
  ("t" im/change-window-split-layout "change")
  ("o" imdra-window/ip/other-window- nil :exit t)

  ("x" ip/other-window-skip-this "skip" :exit t)
  ("X" ip/other-window-skip-regexp nil :exit t)

  ("=" balance-windows-area "balance")
  ("C-=" balance-windows nil)

  ("C-j" (enlarge-window -2 nil) "resize")
  ("C-k" (enlarge-window  2 nil) nil)
  ("C-h" (enlarge-window -2 t)   nil)
  ("C-l" (enlarge-window  2 t)   nil)

  ("<left>" (progn (winner-undo) (setq this-command 'winner-undo)) "win-undo")
  ("<right>" winner-redo nil))

(defvar imdra-show-body nil)

(x super-window/e
   "Some extension for window operation."
   :config (ip/sw-mode 1))

;; Split Window+

(defun my-split-window-below ()
  (interactive)
  (call-interactively 'split-window-below)
  (imdra-window/body))

(defun my-split-window-right ()
  (interactive)
  (call-interactively 'split-window-right)
  (imdra-window/body))

;; Change layout

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

(global-set-key (kbd "C-,") 'imdra-buffer/body)
(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "C-x 3") 'my-split-window-right)
(global-set-key (kbd "C-x o") 'imdra-window/other-window)
(global-set-key (kbd "C-x O") 'imdra-window/ip/other-window+)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (let ((imdra-show-body t)) (imdra-window/body))))
(global-set-key [f1] 'imdra-window-extra/body)



;;; Super *Buffer*

(defhydra imdra-buffer (:columns 4 :exit t)
  "\n"

  ;; search
  ("t" youdao-dictionary-search-from-input "Translate")
  ("T" (if (env-g) (youdao-dictionary-search-at-point-tooltip) (youdao-dictionary-search-at-point+)) nil)
  ("C-t" youdao-dictionary-search-at-point-tooltip nil)
  ("gt" engine/search-dict-iciba nil)
  ("gg" engine/search-google "Google")
  ("gc" engine/search-google-cn "Google-CN")
  ("F" engine/search-wolfram-alpha "Wolfram Alpha")
  ("G" engine/search-github "Github")
  ("S" engine/search-stackoverflow "StackOverflow")
  ("W" engine/search-wikipedia "Wikipedia")
  ("L" engine/search-arch-wiki "ArchWiki")

  ;; mark
  ("ml" (progn (back-to-indentation) (set-mark (line-end-position))) "line" :exit nil)
  ("mp" mark-page "page" :exit nil)
  ("mh" mark-paragraph "Paragraph" :exit nil)
  ("ms" mark-sexp "sexp" :exit nil)
  ("SPC" (progn (set-mark-command nil) (deactivate-mark)) "Mark Here")
  ("r" counsel-mark-ring "Mark Ring" :exit nil)

  ;; expand mark
  ("," er/expand-region "Expand Region" :exit nil)
  ("." er/contract-region "Contract Region" :exit nil)
  ("C-," er/expand-region nil :exit nil)
  ("C-." er/contract-region nil :exit nil)

  ;; misc
  ("n" (if (use-region-p) (call-interactively 'narrow-to-region) (message "No region, no narrow")) "Narrow Region")
  ("w" im/wrap-current "Wrap word")
  ("h" im/host-viewer "Favorites")
  ("*" im/isearch-regexp "Search Next")
  )

(defhydra imdra-yank-pop
  (:hint nil :pre (hydra-set-property 'imdra-yank-pop :verbosity 0))
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("M-l" counsel-yank-pop "list" :color blue))

(defhydra imdra-hideshow (:foreign-keys run :hint nil)
  "
Hide^^            ^Show^            ^Toggle^
-------------------------------------------------
_H_ hide all      _S_ show all      _f_ toggle
_h_ hide block    _s_ show block    _F_ toggle all
_l_ hide level    _SPC_ cancel
"
  ("S" hs-show-all)
  ("H" hs-hide-all)
  ("s" hs-show-block)
  ("h" hs-hide-block)
  ("f" hs-toggle-hiding)
  ("F" my-hs-toggle-all)
  ("l" hs-hide-level)
  ("q" nil)
  ("SPC" nil))

(defhydra imdra-narrow (:hint nil :exit t :idle 0.8)
  "
 narrow  _d_ → defun   _b_ → org-block    _w_ → widen
         _n_ → region  _e_ → org-element
         _p_ → page    _s_ → org-subtree
"
  ("b" org-narrow-to-block)
  ("e" org-narrow-to-element)
  ("s" org-narrow-to-subtree)
  ("d" narrow-to-defun)
  ("n" narrow-to-region)
  ("p" narrow-to-page)
  ("w" widen))

(global-set-key (kbd "C-y") 'imdra-yank-pop/yank)
(global-set-key (kbd "M-y") 'imdra-yank-pop/yank-pop)
(global-set-key (kbd "C-x n") 'imdra-narrow/body)
(key-chord-define-global ",," 'imdra-buffer/body)



;;; Miscellaneous

;; Dired

(defhydra imdra-dired (:hint nil :color pink)
  "
Mkdir(_+_)/_C_opy/_D_elete/_R_ename/_Z_ip

Ch_M_od/Ch_G_rp/Ch_O_wn/_S_ymlink/RelaSymlink(_Y_)

_m_ark/_t_oggleMark/_u_mark/_U_markAll/RegexpMark(％)

_A_ find regexp   _F_ind marked   _Q_ repl regexp
_i_ insert-subdir _$_ hide-subdir _w_ kill-subdir
_s_ sort          _=_ diff        _e_ ediff
_o_ open other    _l_ redisplay   _g_ revert buf
_(_ details       _)_ omit-mode
_6_ updir         _r_ to wdired   _z_ DirUsage

T tag prefix      :e/s/e Encrypt/Sign/Verify

_._ toggle hydra   _?_ summary

"
  ("+" dired-create-directory) ("C" dired-do-copy) ("D" dired-do-delete) ("R" dired-do-rename) ("Z" dired-do-compress)
  ("M" dired-do-chmod) ("G" dired-do-chgrp) ("O" dired-do-chown) ("S" dired-do-symlink) ("Y" dired-do-relsymlink) ;; admin
  ("F" dired-do-find-marked-files) ("A" dired-do-find-regexp) ("Q" dired-do-find-regexp-and-replace) ;; action
  ("v" dired-view-file) ("o" dired-find-file-other-window) ("l" dired-do-redisplay) ("g" revert-buffer) ;;view
  ("m" dired-mark) ("t" dired-toggle-marks) ("u" dired-unmark) ("U" dired-unmark-all-marks) ("E" dired-mark-extension) ;; mark
  ("i" dired-maybe-insert-subdir) ("w" dired-kill-subdir) ("$" dired-hide-subdir)  ;; subdir
  ("s" dired-sort-toggle-or-edit) ("(" dired-hide-details-mode) (")" dired-omit-mode) ("e" dired-ediff-files) ("=" diredp-ediff) ;; util
  ("z" dired-du-mode) ("6" dired-up-directory) ("r" wdired-change-to-wdired-mode)
  ("." nil :color blue) ("?" dired-summary) ("q" nil))

;; Projectile

(defhydra imdra-projectile (:color teal :columns 4)
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
  ("q"   hydra-pop "Cancel" :color blue))

;; Yasnippet

(defhydra imdra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all)
  ("q" hydra-pop "exit"))

;; Flycheck

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

;; Next-Error

(defhydra imdra-next-error (global-map "C-x")
  "next-error"
  ("`" next-error "next")
  ("j" next-error "next" :bind nil)
  ("k" previous-error "previous" :bind nil))


(provide 'ic-hydra)

;;; ic-hydra.el ends here
