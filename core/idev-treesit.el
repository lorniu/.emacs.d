;;; -*- lexical-binding: t -*-

;; M-x treesit-install-language-grammar

;;; Code:

(xzz treesit
  :ref ("TreeSitter Home: https://github.com/tree-sitter/tree-sitter"
        "Precompiled Grammers: https://github.com/emacs-tree-sitter/tree-sitter-langs/releases"))

(defvar ic.enable-treesiter (and (not IS-WIN) (treesit-available-p)))

(setq treesit-extra-load-path (list (locate-user-emacs-file "tree-sitter")))

(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (ocaml      "https://github.com/tree-sitter/tree-sitter-ocaml")
        (scala      "https://github.com/tree-sitter/tree-sitter-scala")
        (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (swift      "https://github.com/tree-sitter/tree-sitter-swift")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
        (make       "https://github.com/alemuller/tree-sitter-make")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
        (erlang     "https://github.com/WhatsApp/tree-sitter-erlang")
        (clojure    "https://github.com/sogaiu/tree-sitter-clojure")
        (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
        (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(when ic.enable-treesiter
  (setq major-mode-remap-alist
        (append
         (delq-nil
          `((c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (yaml-mode . yaml-ts-mode)
            (bash-mode . bash-ts-mode)
            (js2-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (lua-mode . lua-ts-mode)
            (java-mode . java-ts-mode)
            (csharp-mode . csharp-ts-mode)
            (python-mode . python-ts-mode)
            ,(if (> emacs-major-version 30) '(markdown-mode . markdown-ts-mode))))
         major-mode-remap-alist)))
