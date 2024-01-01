;;; -*- lexical-binding: t -*-

;; use Ensime to support both Java and Scala Dev.
;; choose one build tool, maven/gradle or sbt
;; 2019-11-18, use lsp/Metals instead.

;; use jdecomp-mode and cfr/fernflower to auto decompile/show .class file

;; Babel of Java
;;
;; #+BEGIN_SRC java :cmdline "-cp ." :cmpflag "-cp ." :classname Main :dir "~/.cache"
;;   class Main {
;;       public static void main(String[] args) {
;;           System.out.println("hello, world");
;;       }
;;   }
;; #+END_SRC
;;
;; Set the default arguments for Java Babel:
;; #+BEGIN_SRC elisp
;;   (setq org-babel-default-header-args:java
;;         '((:cmpflag . "-cp .:my.jar:your.jar")
;;           (:cmdline . "-cp .:my.jar:your.jar")
;;           (:dir . "~/.cache/javacache")))
;; #+END_SRC

;;; Code:

(with-eval-after-load 'cc-mode
  (c-add-style "java-9" '("java"
                          (c-basic-offset . 4)
                          (c-hanging-braces-alist
                           ((substatement-open)))
                          (c-offsets-alist
                           (case-label . +)
                           (inexpr-class . 0)
                           (arglist-cont-nonempty . (im:arglist-cont-lambda c-lineup-gcc-asm-reg c-lineup-arglist))
                           )))

  (defun im:arglist-cont-lambda (_arg)
    "Make java lambda indent right."
    (when (or (save-excursion
                (forward-line -1)
                (re-search-forward "-> *{ *$" (line-end-position) t))
              (save-excursion
                (re-search-forward "})" (line-end-position) t)))
      0))

  (defun:hook java-mode-hook ()
    (c-set-style "java-9")))

(xzz scala-mode
  "Can use [metals+lsp] to support Scala development:

First, build metals (https://scalameta.org/metals/):

  curl -L -o coursier https://git.io/coursier
  chmod +x coursier
  ./coursier bootstrap --java-opt -Xss4m --java-opt -Xms100m -130va-opt -Dmetals.client=emacs org.scalameta:metals_2.12:0.7.6 -r bintray:scalacenter/releases -r sonatype:snapshots -o /usr/local/bin/metals-emacs -f

  # or use ~/.emacs.d/bin/build-metals-for-scala.sh to build.

Then, enable it with 'lsp' command.

note: Ensime is deprecated.
   ")

(xzz groovy-mode
  :mode "\\.g\\(?:radle\\|roovy\\)$")

(xzz kotlin-mode)

(xzz android-mode
  :ref "remvee/android-mode"
  :init
  (defun im:maybe-android-h ()
    (when-let* ((d (project-root (project-current))))
      (when (cl-some (lambda (f) (file-exists-p (expand-file-name f d)))
                     (list "AndroidManifest.xml" "src/main/AndroidManifest.xml"))
        (android-mode +1))))
  (cl-loop for h in '(java-mode-hook groovy-mode-hook nxml-mode-hook)
           do (add-hook h #'im:maybe-android-h)))

(xzz jdecomp
  :init
  (setq jdecomp-decompiler-type 'fernflower)
  (setq jdecomp-decompiler-paths (list (cons 'fernflower (loce "share/fernflower.jar"))))
  (defun:override jdecomp--fernflower-decompile-file (file &optional extracted-p)
    (jdecomp--ensure-decompiler 'fernflower)
    (with-temp-buffer
      (let* ((classpath (or (file-name-directory file) default-directory))
             (destination (if extracted-p
                              (file-name-directory file)
                            (jdecomp--make-temp-file (concat "jdecomp"
                                                             (replace-regexp-in-string "\\(\\w\\):" "/\\1" (file-name-sans-extension file))) t))))
        ;; See: http://stackoverflow.com/a/39868281/864684
        (apply #'call-process "java" nil nil nil
               `(,@(jdecomp--decompiler-options 'fernflower)
                 "-cp" ,classpath
                 "-cp" ,(expand-file-name (jdecomp--decompiler-path 'fernflower))
                 "org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler"
                 ,file
                 ,destination))
        (insert-file-contents (cl-first (jdecomp--java-files destination)))
        (buffer-string))))
  (defun:override jdecomp-archive-hook-function ()
    "Fixup the path for Windows."
    (let ((arr (split-string (buffer-file-name) ":")) jar file)
      (setq file (car (last arr)))
      (setq jar (string-join (butlast arr) ":"))
      (when (and jdecomp-mode
                 (jdecomp--classfile-p file))
        (kill-buffer (current-buffer))
        (jdecomp-decompile-and-view file jar))))
  (defun:around jdecomp--classfile-p//null-case (fn file)
    "If file is nil, return false."
    (and file (funcall fn file)))
  (define-derived-mode class-mode fundamental-mode "" "")
  (jdecomp-mode 1))



(defun im/run-gradlew (command)
  "Run gradlew in this project."
  (interactive "sCommand: ")
  (let ((default-directory (locate-dominating-file buffer-file-name "gradlew"))
        (compilation-read-command nil)
        (compile-command (format "sh gradlew %s" command)))
    (call-interactively #'compile)))
