if status is-interactive
    # Commands to run in interactive sessions can go here
    alias emacs="emacsclient -c -a 'emacs'"
    alias nvidia="sudo nvidia-settings"
    alias ipwebcam="/home/thiago/.builds/manual/ipwebcam-gst/run-videochat.sh -v -i"
    alias btrfs-disk="sudo btrfs filesystem usage /"
    alias ventoy="sudo ~/Applications/ventoy-1.0.64/VentoyGUI.x86_64"
    alias dfgit="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
end
## Auto start tbsm after login on first two VTs
#  [[ $XDG_VTNR -le 2 ]] && tbsm
  if test "$DISPLAY" = "" -a "$XDG_VTNR" = 1
     tbsm
  end

## Fish fcitx5
set GTK_IM_MODULE fcitx
set QT_IM_MODULE fcitx
set SDL_IM_MODULE fcitx
set XMODIFIERS '@im=fcitx'