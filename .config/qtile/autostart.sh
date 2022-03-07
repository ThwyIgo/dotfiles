#!/usr/bin/env bash
setxkbmap -layout br &
fcitx5 -d &
#setxkbmap -model "" -layout br,cn -variant "" -option grp:win_space_toggle &
numlockx on &
picom &
feh --bg-scale --randomize ~/Imagens/Papéis\ de\ parede/* &
env XMODIFIERS= /usr/bin/emacs --daemon &
gtk-launch org.kde.kdeconnect.daemon &
gtk-launch org.kde.kdeconnect.nonplasma &
# Apps
#volctl &
#keepassxc &
#telegram-desktop &
dex -a &
