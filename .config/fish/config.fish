set -gx GTK_IM_MODULE fcitx
set -gx QT_IM_MODULE fcitx
set -gx SDL_IM_MODULE fcitx
set -gx XMODIFIERS '@im=fcitx'

## Auto start tbsm after login on first two VTs
#  [[ $XDG_VTNR -le 2 ]] && tbsm
if test "$DISPLAY" = "" -a "$XDG_VTNR" = 1
    tbsm
end

if status is-interactive
    # Commands to run in interactive sessions can go here
    alias emacsc="emacsclient -c -a 'emacs'"
    alias nvidia="sudo nvidia-settings"
    alias ipwebcam="/home/thiago/.builds/manual/ipwebcam-gst/run-videochat.sh -v -i"
    alias btrfs-disk="sudo btrfs filesystem usage /"
    alias ventoy="sudo ~/Applications/ventoy-1.0.64/VentoyGUI.x86_64"
    alias dfgit="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
end

## Env vars
source /home/thiago/.ghcup/env
fish_add_path ~/.local/bin
set fish_color_user 5cf brblue
set fish_color_cwd brgreen
set -gx VISUAL emacsclient -c -a 'emacs'
set -gx EDITOR micro

## Functions
function fish_right_prompt
    # Time on the right
    echo [(date '+%H:%M')]
end