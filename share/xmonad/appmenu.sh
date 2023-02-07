#!/bin/sh


if [ -x /usr/bin/xfce4-appfinder ]; then
    xfce4-appfinder
elif [ -x /usr/bin/jgmenu ]; then
    killall jgmenu
    jgmenu --at-pointer
else
    dmenu_run
fi
