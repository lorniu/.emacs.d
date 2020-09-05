#!/bin/sh

#.(read-directory-name "Binary destination: " "~" nil t)

dst=${1:-~/bin}

cd ~/.emacs.d/

# coursier
if [ -f coursier ]; then
    echo Coursier already existed, use it.
else
    curl -L -o coursier https://git.io/coursier && chmod +x coursier
    if [ $? -ne 0 ]; then
        echo
        echo Coursier download error? Please check.
        exit
    else
        echo
        echo get Coursier successfully.
    fi
fi

# build metals
./coursier bootstrap --java-opt -Xss4m \
 --java-opt -Xms100m \
 --java-opt -Dmetals.client=emacs org.scalameta:metals_2.12:0.7.6 \
 -r bintray:scalacenter/releases \
 -r sonatype:snapshots \
 -o $dst/metals-emacs -f

echo "build Metals successfully ($dst/metals-emacs)".
