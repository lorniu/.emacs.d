;;; face-themes.el --- Themes -*- lexical-binding: t -*-

;;; Code:

(x nano-theme
   :ref "rougier/nano-theme"
   :init
   (setq nano-light-highlight "#eeeeee"))

(x gruvbox-theme
   :ref "greduan/emacs-theme-gruvbox")

(x modus-themes
   :ref "protesilaos/modus-themes"
   :init
   (setq modus-themes-mode-line '(1 borderless)))

(x zero-dark-theme
   :ref "NicolasPetton/zerodark-theme"
   :init
   (defvar zero-dark-use-paddings-in-mode-line 3))

(if-let (theme (f/get :theme)) (load-theme theme t))

(provide 'face-themes)

;;; face-themes.el ends here
