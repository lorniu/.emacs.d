;;; icod-dotnet.el --- .Net -*- lexical-binding: t -*-

;; LSP Server for CSharp:
;;
;;  1. yay -S omnisharp-roslyn-bin
;;  2. dotnet tool install -g csharp-ls
;;

;;; Code:

(defreference dotnet
  "babel: samwdp/ob-csharp"
  "Server: OmniSharp/omnisharp-roslyn/releases"
  "Server: razzmatazz/csharp-language-server"
  "Decompile: https://github.com/icsharpcode/ILSpy")

(x csharp-mode
   :init
   (defun:hook csharp-mode-hook()
     (electric-pair-local-mode 1)
     (local-set-key (kbd "C-c C-c") 'recompile)))

(x fsharp-mode
   :defer-config
   (when (executable-find "dotnet")
     (setq inferior-fsharp-program "dotnet fsi --readline-")))

(x sharper
   :commands (sharper-main-transient)
   :ref "sebasmonia/sharper")

(defun im/csharp-repl ()
  "Switch to the CSharpRepl buffer, creating it if necessary."
  (interactive)
  (if-let ((buf (get-buffer "*CSharpRepl*")))
      (pop-to-buffer buf)
    (when-let ((b (make-comint "CSharpRepl" "csharp")))
      (switch-to-buffer-other-window b))))

(transient-define-prefix imtt/transient-csharp-mode ()
  [ ("m" "CSharp Repl" im/csharp-repl) ]
  (interactive)
  (if (eq major-mode 'csharp-mode)
      (transient-setup 'imtt/transient-csharp-mode)
    (user-error "Sorry, but this is not csharp-mode")))

(provide 'icod-dotnet)

;;; icod-dotnet.el ends here
