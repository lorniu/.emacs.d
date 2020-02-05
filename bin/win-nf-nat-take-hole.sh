#!/bin/sh

# Take hole on the NAT, from SCHOOL to OUTSIDE.

#. (if (string= *vps* "not-set-yet") "" *vps*)

if [ -z $1 ]; then
    echo Should given a proper ip address as argument.
    echo
    echo or you should config \*vps\* in emacs first.
    exit
fi

connects=(
    '*:20022:127.0.0.1:22'
    '*:21521:192.168.0.168:1521'
    '*:28028:192.168.0.168:8028'
)

if [[ -n `ps |grep autossh` ]]; then
    echo "Process *autossh* already running?!";
    echo
    echo "You may want to kill all ssh processes? Like this:";
    echo
    echo "  get-process | where { \$_.name -match \"ssh$\" } | stop-process"
    echo
    echo "Go... then go back."
else
    randport=55551
    remote=root@$1

    for conn in ${connects[@]}; do
        randport=$[$randport+2]
        autossh -p 22 -M $randport -fNR $conn $remote;
    done;

    echo "Hole on the wall !"
    echo
    echo ">> $1 <<"
    echo
    echo ${connects[*]}
fi;
