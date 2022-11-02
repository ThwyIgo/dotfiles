# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

[ -f "/home/thiago/.ghcup/env" ] && source "/home/thiago/.ghcup/env" # ghcup-env

# Created by user
PATH=$PATH:~/.local/bin
