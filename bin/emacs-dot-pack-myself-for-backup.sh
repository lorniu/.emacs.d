#!/bin/sh

from=~/

if [ ! -d $from/.emacs.d ]; then
    echo "Directory '.emacs.d' Not Exists! I can't do anything...";
else
    cd $from

    dest=emacs-dot-`date +%Y%m%d`.tar.gz

    including=(
        ./.emacs.d/.git
        ./.emacs.d/.gitignore
        ./.emacs.d/.gitmodules
        ./.emacs.d/bin
        ./.emacs.d/core
        ./.emacs.d/extra
        ./.emacs.d/themes
        ./.emacs.d/elpa
        ./.emacs.d/share
        ./.emacs.d/snippets
        ./.emacs.d/init.el
        ./.emacs.d/mini-init.el
        ./.emacs.d/early-init.el
        ./.emacs.d/README.md
        ./.emacs.d/Makefile
        ./.emacs.d/plantuml.jar
    )

    echo "Begin to pack...";
    echo
    tar cvzf .emacs.d/$dest ${including[@]}
    echo
    echo "Pack myself sucessfully. [$dest]";
fi;
