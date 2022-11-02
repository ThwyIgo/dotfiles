#!/usr/bin/env bash

setxkbmap -layout br &
dex -a -e qtile -s $HOME/.config/autostart/ &
numlockx on &
picom -b --experimental-backends &
feh --bg-scale --randomize ~/Imagens/Papéis\ de\ parede/* &
gtk-launch org.kde.kdeconnect.daemon &
gtk-launch org.kde.kdeconnect.nonplasma & # aur/indicator-kdeconnect / bin: kdeconnect-indicator
## O indicator do KDE Connect não é mais mantido

#fcitx5 -d &
#env XMODIFIERS= /usr/bin/emacs --daemon &
