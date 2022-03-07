#!/usr/bin/env bash
DIR=~/.config/rofi/scripts/
SCRIPT=$(ls $DIR | rofi -dmenu -p "Script:")
echo $SCRIPT
bash ${DIR}$SCRIPT
