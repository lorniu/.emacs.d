;;; imkeys.el --- Keybinds
;;; Commentary:

;;; Code:

;;;; Key-Chord

(key-chord-define-global ",," 'imdra-buffer/body)
(key-chord-define-global ",f" 'im/go-to-char)
(key-chord-define-global ",t" (lambda () (interactive) (im/go-to-char t)))
(key-chord-define-global ",r" 'counsel-mark-ring)
(key-chord-define-global ",c" 'imdra-dash-counsel/body)


;;;; Keybinds

(global-unset-key (kbd "C-x m"))

(bind-keys
 ( [f1]          . imdra-overview/body  )
 ( [f6]          . toggle-truncate-lines)
 ( [f8]          . calendar             )
 ( [f10]         . shrink-window        )
 ( [f11]         . enlarge-window       )
 ( [f12]         . ivy-push-view        )
 ( [C-f12]       . ivy-pop-view         )
 ( "%"           . his-match-paren      )
 ( "C-,"         . imdra-buffer/body    )
 ( "C-s"         . im/isearch-regexp    )
 ( "C-x O"       . imdra-window/body    )
 ( "C-x o"       . imdra-window/other-window )
 ( "C-x C-o"     . imdra-window/other-window+ )
 ( "C-x 2"       . -my/split-window-below)
 ( "C-x 3"       . -my/split-window-right)
 ( "C-x n"       . imdra-narrow/body    )
 ( "C-x C-r"     . imdra-favors/body    )
 ( "C-c C-j"     . ffap                 )
 ( "M-w"         . im/yank-more         )
 ( "M-q"         . im/tiny-code         )
 ( "M-h"         . imdra-overview/body  )
 ( "ESC <down>"  . im/copy-lines        )
 ( "ESC <up>"    . (lambda () (interactive) (im/copy-lines 1)))
 ( "ESC <right>" . im/kill-lines        )
 ( "<C-backspace>" . im/backward-kill-word )
 ( "<insertchar>". undo                 )
 ( "<select>"    . im/toggle-dedicated  )

 ( "C-c a"       . org-agenda           )
 ( "C-c c"       . imdra-org/body       )

 ( "M-x"         . counsel-M-x          )
 ( "C-r"         . counsel-grep-or-swiper )
 ( "C-x b"       . ivy-switch-buffer    )
 ( "C-x C-b"     . ibuffer              )
 ( "C-x d"       . counsel-projectile-find-dir )
 ( "C-x C-d"     . dired                )
 ( "C-x f"       . counsel-projectile-find-file )
 ( "C-x C-f"     . counsel-find-file    )
 ( "C-x i"       . counsel-imenu        )
 ( "C-x p"       . ivy-pages            )
 ( "C-h b"       . counsel-descbinds    )
 ( "C-h v"       . counsel-describe-variable )
 ( "C-h f"       . counsel-describe-function )
 ( "C-h S"       . counsel-info-lookup-symbol)
 ( "C-#"         . cua-rectangle-mark-mode ))


;;;; Index

(defmacro hydra-exp (child parent)
  `(progn (,(intern (concat "imdra-" (symbol-name child) "/body")))
          (hydra-push '(,(intern (concat "imdra-" (symbol-name parent) "/body"))))))

;;; Overview

(defhydra imdra-overview (:color teal)
  "\n"
  ("q" nil nil)
  ("i" info "Info" :column "Documentation")
  ("d" (hydra-exp desktop overview) "Desktop Save" :column "Programable")
  ("p" (hydra-exp projectile overview) "Projectile")
  ("y" (hydra-exp yasnippet overview) "Yasnippet")
  ("o" (hydra-exp org overview) "Org-Mode" :column "Modes")
  ("c" (hydra-exp dash-counsel overview) "Dash(Counsel)" :column "Fast Way")
  ("l" (imdra-favors/body) "Favor Files")
  ("<f1>" nil nil)
  )

;;; Favor Files

(defhydra imdra-favors (:exit t :columns 3)
  "My Favor Files"
  ("C-r" (im/open-file-view "~/.emacs.d/core/immor.el") "immor.el")
  ("x"   (find-file "~/.notes/x.misc/posix/xmonad.hs") "xmonad.hs")
  ("v"   (find-file "~/vvv/") "vvv")
  ("n"   (find-file (car note-dir)) ".notes/")
  ("c"   (find-file _CACHE_) "cache/emacs   ")
  ("s"   (find-file "/sudo::/etc/systemd/system/multi-user.target.wants/") "systemd/"))

;;; Dash Counsel

(defhydra imdra-dash-counsel (:columns 3 :exit t)
  "\nDash:"
  ("cw" counsel-colors-web "Color(Web)")
  ("ce" counsel-colors-emacs "Color(Emacs)")
  ("cu" counsel-unicode-char "Unicode Char")
  ("lf" list-faces-display "List Faces")
  ("lh" counsel-hydra-heads "Hydras")
  ("la" ascii-table-show "Ascii Table")
  ("ag" counsel-ag "Search with ag"))

;;; Org-Mode

(defhydra imdra-org (:color blue :foreign-keys warn :hint nil)
  "
   ^
   ^Org^             ^Clock^          ^Misc^
   ^──^──────────────^──^─────────────^─────^────────
   _c_ Capture       _d_ display      _q_ quit
   ^^                _i_ in           ^^
   _l_ Store link    _o_ out          ^^
   _L_ Stored Link   _j_ jump         ^^
   ^^                _r_ report       ^^
   _n_ Publish       _e_ effort       ^^
   _m_ Publish<f>    _C_ cancel
   ^^                ^^
    "
  ("q" nil)
  ("a" org-agenda)

  ("d" org-clock-display)
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("j" org-clock-goto)
  ("r" org-clock-report)
  ("e" org-clock-modify-effort-estimate)
  ("C" org-clock-cancel :color pink)

  ("l" org-store-link)
  ("L" org-insert-link)
  ("c" org-capture :exit t)
  ("n" im/org-publish-note :exit t)
  ("m" im/org-publish-note-force :exit t))


;;;; Super Window

(defhydra imdra-window
  (:pre
   (progn
     (setq hydra-is-helpful nil)
     (set-face-attribute 'mode-line nil :slant 'italic))
   :post
   (progn
     (setq hydra-is-helpful t)
     (set-face-attribute 'mode-line nil :slant 'normal)))
  "\n"

  ("h" windmove-left "Left" :column "Switch")
  ("j" windmove-down "Down")
  ("k" windmove-up "Up")
  ("l" windmove-right "Right")

  ("C-j" (enlarge-window -2 nil) "v-" :column "Resize")
  ("C-k" (enlarge-window  2 nil) "v+")
  ("C-h" (enlarge-window -2 t)   "h-")
  ("C-l" (enlarge-window  2 t)   "h+")

  ("0" delete-window "Hide" :column "Action")
  ("1" delete-other-windows "Maximum")
  ("2" split-window-below "Split<h>")
  ("3" split-window-right "Split<v>")
  ("b" ivy-switch-buffer "Switch Buffer" :exit t)

  ("a" ace-window "Ace" :exit t :column "Manage")
  ("o" other-window nil)
  ("C-o" other-window+ nil)
  ("=" balance-windows-area "Balance")
  ("C-=" balance-windows nil)

  ("<left>" (progn (winner-undo) (setq this-command 'winner-undo)) "Winner ←")
  ("<right>" winner-redo "Winner →")

  ("S" (hydra-exp window-config window) "Config" :exit t :column "Misc")
  ("D" imdra-desktop/body "Desktop" :exit t)
  ("H" imdra-window/body "Help" :exit nil)
  ("M-h" imdra-window/body "Help" :exit nil)
  )

;;; Window Config

(defhydra imdra-window-config (:columns 1)
  "Config\n"
  ("o" im/other-window-set-exclude-regexp "Config Other Window Exclude Regexp" :exit t)
  ("q" hydra-pop "Go Back"))

;;; Other Window+

(defvar im/other-window-exclude-regexp "^\\*")

(defun other-window+ ()
  (interactive)
  (let* ((window-list (delq (selected-window) (window-list)))
         (filtered-window-list (remove-if
                                (lambda (w)
                                  (string-match-p im/other-window-exclude-regexp (buffer-name (window-buffer w))))
                                window-list)))
    (if filtered-window-list
        (select-window (car filtered-window-list)))))

(defun im/other-window-set-exclude-regexp ()
  (interactive)
  (setq im/other-window-exclude-regexp
        (read-from-minibuffer "Buffer to exclude (regexp): " im/other-window-exclude-regexp)))

;;; Split Window+

(defun -my/split-window-below ()
  (interactive)
  (call-interactively 'split-window-below)
  (imdra-window/body))

(defun -my/split-window-right ()
  (interactive)
  (call-interactively 'split-window-right)
  (imdra-window/body))

;;; Desktop

(defhydra imdra-desktop (:color blue)
  "Desktop"
  ("l" desktop-change-dir "load")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("c" desktop-clear "clear")
  ("q" hydra-pop nil))


;;;; Super Buffer

(defhydra imdra-buffer (:columns 8 :exit t)
  "\n"

  ;; search
  ("g" engine/search-google "Google")
  ("t" youdao-dictionary-search-from-input "Translate")
  ("T" (if (env-g) (youdao-dictionary-search-at-point-tooltip) (youdao-dictionary-search-at-point+)) nil)
  ("C-t" youdao-dictionary-search-at-point-tooltip nil)
  ("G" engine/search-github "Github")
  ("S" engine/search-stackoverflow "StackOverflow")
  ("W" engine/search-wikipedia "Wikipedia")
  ("L" engine/search-arch-wiki "ArchWiki")
  ("F" engine/search-wolfram-alpha "Wolfram Alpha")
  ("Y" engine/search-youtube "Youtube")

  ;; mark
  ("m<" mark-beginning-of-buffer "buf-min")
  ("m>" mark-end-of-buffer "buf-max")
  ("ma" (let ((o (point))) (back-to-indentation) (push-mark) (goto-char o)) "line-beg")
  ("me" (push-mark (line-end-position)) "line-end")
  ("ml" (progn (back-to-indentation) (set-mark (line-end-position))) "line")
  ("mp" mark-page "page")
  ("mh" mark-paragraph "Paragraph" :exit nil)
  ("ms" mark-sexp "sexp" :exit nil)
  ("p" (set-mark-command 1) "Pop Mark" :exit nil)
  ("SPC" (progn (set-mark-command nil) (deactivate-mark)) "Set Mark")

  ;; expand mark
  ("," er/expand-region "Expand Region" :exit nil)
  ("." er/contract-region "Contract Region" :exit nil)
  ("C-," er/expand-region nil :exit nil)
  ("C-." er/contract-region nil :exit nil)

  ;; action
  ("ii" (ahs-onekey-edit-function 'whole-buffer nil) "edit<b>")
  ("if" (ahs-onekey-edit-function 'beginning-of-defun nil) "edit<f>")
  ("N" narrow-to-region "Narrow")
  ("f" im/go-to-char "GoToChar")
  )

;;; HideShow Mode

(defhydra imdra-hs (:foreign-keys run :hint nil)
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
  ("F" -my/hs-toggle-all)
  ("l" hs-hide-level)
  ("q" nil)
  ("SPC" nil))

;;; Yank

(defhydra imdra-yank-pop
  (:hint nil :pre (hydra-set-property 'imdra-yank-pop :verbosity 0))
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("M-l" counsel-yank-pop "list" :color blue))

(global-set-key (kbd "C-y") 'imdra-yank-pop/yank)
(global-set-key (kbd "M-y") 'imdra-yank-pop/yank-pop)

;;; Narrow

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


;;;; Miscellaneous

;;; Projectile

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

;;; Dired

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

;;; Yasnippet

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

;;; Flycheck

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

;;; Next-Error

(defhydra imdra-next-error (global-map "C-x")
  "next-error"
  ("`" next-error "next")
  ("j" next-error "next" :bind nil)
  ("k" previous-error "previous" :bind nil))

;;; Macro

(defhydra imdra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                               (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))

(provide 'imkeys)

;;; imkeys.el ends here
