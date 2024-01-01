# public init

[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap &
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources &

# input method

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export SDL_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# session

case ${1:-xmonad} in
    fce|xfce)
        exec startxfce4
        ;;
    kde|plasma)
        exec startplasma-x11
        ;;
    term|xterm)
        exec xterm
        ;;
    *)
        export XMONAD_WALLPAPER=/home/vip/Image/wallpaper.png
        xmonad
        ;;
esac
