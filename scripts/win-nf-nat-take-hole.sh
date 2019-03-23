#!/bin/sh

# Take hole on the NAT, from SCHOOL to OUTSIDE.

island=45.63.55.2

connects=(
    '*:20022:127.0.0.1:22'
    '*:1521:192.168.0.168:1521'
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
    remote=root@$island

    for conn in ${connects[@]}; do
        randport=$[$randport+2]
        autossh -p 22 -M $randport -fNR $conn $remote;
    done;

    echo "Hole on the wall !"
    echo
    echo ">> $island <<"
    echo
    echo ${connects[*]}
fi;
