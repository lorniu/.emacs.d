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
        ./.emacs.d/core
        ./.emacs.d/extra
        ./.emacs.d/packages
        ./.emacs.d/resource
        ./.emacs.d/scripts
        ./.emacs.d/snippets
        ./.emacs.d/init.el
        ./.emacs.d/plantuml.jar
        ./.emacs.d/README.md
    )

    echo "Begin to pack...";
    echo
    tar cvzf .emacs.d/$dest ${including[@]}
    echo
    echo "Pack myself sucessfully. [$dest]";
fi;
