#!/usr/bin/env bash
defaultIp=192.168.1
defaultPort=8080
terminal=alacritty
ipwebcam="/home/thiago/.builds/manual/ipwebcam-gst/run-videochat.sh -v -i" # Path to https://github.com/bluezio/ipwebcam-gst script

IP=$(rofi -dmenu -p "Insira o ip [default: ${defaultIp}.x]:")
if [ ${#IP} -le 3 ]; then # If only 1 number was given as IP,  then complete it with default IP
    xdg-open http://${defaultIp}.${IP}:$defaultPort &
    $terminal -e $ipwebcam ${defaultIp}.$IP; exit
elif [ "$(echo "$IP" | grep -o ":")" = ":" ]; then # If port was given
    IPnoPORT=$(echo "${IP}" | grep -Eo "^[^:]*")
    PORT=$(echo "${IP}" | grep -Eo "[^:]*$")
    if [ ${#IPnoPORT} -le 3 ]; then # If port was given, but $IP length is <= 3
	xdg-open http://${defaultIp}.${IPnoPORT}:$PORT
	$terminal -e $ipwebcam ${defaultIp}.$IPnoPORT -p $PORT; exit
    else
	xdg-open http://$IP # No need to insert port, because it's already in $IP
	$terminal -e $ipwebcam $IPnoPORT -p $PORT; exit
    fi
else
    $terminal -e $ipwebcam $IP; exit
fi
