#! /usr/bin/env sh
set -e
playerctl metadata &> /dev/null &&
    printf "%s - %s " "$(playerctl metadata artist)" "$(playerctl metadata title)"
