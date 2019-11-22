#!/bin/sh

#.(read-from-minibuffer "Domain name: " "imfine.cc")

if [ -z "$1" ]; then
    echo HOST must not be empty.
    exit 0
fi

host=$1
www=$HOME/.notes/x.http-server/html
dest=$HOME/.notes/x.http-server/config/ssl-cert

if [ ! -d $dest ]; then
    echo Directory $dest is not exist.
    exit 0
fi

# check envirnment
if [ ! -f "$HOME/.acme.sh/acme.sh" ]; then
    echo No acme.sh found, initializing...
    curl https://get.acme.sh | sh
fi

if [ $? -ne 0 ]; then
    echo ----------------------
    echo Init acme.sh failed
    echo ----------------------
    exit
fi

# regist or renew cert
if [ ! -d "~/.acme.sh/$host" ]; then
    echo Make new key cert files...
    ~/.acme.sh/acme.sh --issue -d $host --webroot $www  # --standalone
else
    echo Renew cert files...
    ~/.acme.sh/acme.sh --renew -d $host --force
fi

if [ $? -ne 0 ]; then
    echo ----------------------
    echo Make cert files failed
    echo ----------------------
    exit
fi

# copy certs
mkdir $dest/$host
~/.acme.sh/acme.sh --installcert -d $host --fullchainpath $dest/$host/server.cer --keypath $dest/$host/server.key

if [ $? -eq 0 ]; then
    echo
    echo Copy success !
    chmod +r $dest/$host/server.key
else
    echo
    echo Copy failed.
fi
