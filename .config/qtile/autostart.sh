#!/usr/bin/env bash
setxkbmap -layout br &
dex -a -s $HOME/.config/autostart/ &
#fcitx5 -d &
#setxkbmap -model "" -layout br,cn -variant "" -option grp:win_space_toggle &
numlockx on &
picom -b --experimental-backends &
feh --bg-scale --randomize ~/Imagens/Papéis\ de\ parede/* &
#env XMODIFIERS= /usr/bin/emacs --daemon &
gtk-launch org.kde.kdeconnect.daemon &
gtk-launch org.kde.kdeconnect.nonplasma & # aur/indicator-kdeconnect / bin: kdeconnect-indicator
## O indicator do KDE Connect não é mais mantido

# Kill apps from other DEs
#killall polkit-kde-authentication-agent
