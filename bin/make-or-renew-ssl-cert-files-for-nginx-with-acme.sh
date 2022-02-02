#!/usr/bin/env bash

#.(read-from-minibuffer "Domain name: " "*.imfine.cc")
echo 1

if [ -z "$1" ]; then
    echo HOST must not be empty.
    exit 0
fi

host=$1
hostdir=$1
acme="$HOME/.acme.sh/acme.sh "
env_file=$HOME/.notes/share/ssl-cert/env
www=$HOME/.notes/webapp/html
dest=$HOME/.notes/share/ssl-cert

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

# api key/password
if [ -r $env_file ]; then
    source $env_file
fi

# regist or renew cert
if [[ $host == "*"* ]]; then
    # dns-01 style for wildcard cert
    # Make sure that env file exists (CF_Email/CF_key), it is used to manipulate DNS of cf.
    if [[ $host =~ .(gq|cf|ml|ga|tk)$ ]]; then
        # cloudflare, banned api for gq/ml/etc now, so use DNS-Alias-Mode instead.
        # first, you shold add a CNAME: _acme-challenge.a.gq => _acme-challenge.aliasDomain.com
        echo "Make new key cert files (dns/alias)..."
        ( set -x; $acme --issue -d ${host:2} -d $host --dns dns_cf --force --challenge-alias $dns_alias_host )
    else
        echo "Make new key cert files (dns)..."
        ( set -x; $acme --issue -d ${host:2} -d $host --dns dns_cf --force )
    fi
    hostdir="x.${host:2}"
else
    # http-01 style, make sure nginx is runing
    if [ ! -d "~/.acme.sh/$host" ]; then
        echo "Make new key cert files (http)..."
        ( set -x; $acme --force --issue -d $host --webroot $www )  # --standalone
    else
        echo Renew cert files...
        echo "Renew cert files (http)..."
        ( set -x; $acme --renew -d $host --force )
    fi
fi

if [ $? -ne 0 ]; then
    echo ----------------------
    echo Make cert files failed, maybe you should start a pure config nginx and try again.
    echo ----------------------
    exit
fi

# copy certs
if [ ! -d $dest/$hostdir ]; then mkdir $dest/$hostdir; fi
$acme --installcert -d ${host:2} --fullchainpath $dest/$hostdir/server.cer --keypath $dest/$hostdir/server.key

if [ $? -eq 0 ]; then
    echo
    echo Copy success !
    chmod +r $dest/$hostdir/server.key
else
    echo
    echo Copy failed.
fi
