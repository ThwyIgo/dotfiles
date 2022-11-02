## Auto start tbsm after login on first two VTs
#  [[ $XDG_VTNR -le 2 ]] && tbsm
if test "$DISPLAY" = "" -a "$XDG_VTNR" = 1
    tbsm
end

if status is-interactive
    # Commands to run in interactive sessions can go here
    alias emacs="emacsclient -c -a 'emacs'"
    alias nvidia="sudo nvidia-settings"
    alias ipwebcam="/home/thiago/.builds/manual/ipwebcam-gst/run-videochat.sh -v -i"
    alias btrfs-disk="sudo btrfs filesystem usage /"
    alias ventoy="sudo ~/Applications/ventoy-1.0.64/VentoyGUI.x86_64"
    alias dfgit="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
end

## Env vars
set fish_color_user 5cf brblue
set fish_color_cwd brgreen

## Functions
function fish_right_prompt
    # Time on the right
    echo [(date '+%H:%M')]
end