#!/bin/sh

#.(read-directory-name "Where is nginx: " "/opt/" nil t)

src=/home/vip/.notes/x.share/nginx
dst=${1:-/opt/nginx/}

if [ ! -d $dst ]; then
    echo Nginx Home error specified.
    exit 0;
fi

# config file

sudo mv -f $dst/conf/nginx.conf $dst/conf/nginx_bk.conf
sudo ln -s $src/nginx.conf $dst/conf/nginx.conf

if [ $? -eq 0 ]; then echo Link nginx.conf success.; fi

# command rc

ngrc=/usr/local/etc/rc.d/nginx
sudo cp -f $src/rc-copy-after-compile $ngrc
sudo sed -i -e "s|PATH_TO_REPLACE|$dst|g" $ngrc
sudo chmod +x $ngrc

if [ $? -eq 0 ]; then echo Copy nginx.conf success.; fi
