#!/usr/bin/env sh
rofi -show-icons -show window -hover-select -me-select-entry '' -me-accept-entry 'MousePrimary' -theme ~/.config/rofi/themes/launchpad.rasi && if [ "$(qtile cmd-obj -o window -f is_visible)" != "True" ]; then qtile cmd-obj -o window -f toggle_minimize;  fi
