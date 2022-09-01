#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

[ -f "/home/thiago/.ghcup/env" ] && source "/home/thiago/.ghcup/env" # ghcup-env

# Created by user
PATH=$PATH:~/.local/bin
alias emacs="env XMODIFIERS= emacsclient -c -a 'emacs'"
alias nvidia="sudo nvidia-settings"
alias ipwebcam="/home/thiago/.builds/manual/ipwebcam-gst/run-videochat.sh -v -i"
alias btrfs-disk="sudo btrfs filesystem usage /"
alias ventoy="sudo ~/Applications/ventoy-1.0.64/VentoyGUI.x86_64"
alias dfgit="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
exec fish
