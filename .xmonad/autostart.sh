#!/bin/bash
pgrep  compton && exit
#xhost +
compton -cCGf -D 3 -b
#chromium &
nitrogen --restore &
xfce4-power-manager &
~/.xmonad/lightsOn.sh 120 &
gnome-keyring-daemon &
nm-applet &
parcellite &
xbindkeys &
xscreensaver -no-splash &
yandex-disk start &
#xautolock -locker "slock" -time 5 &
~/.local/share/Telegram/Telegram &
lxpolkit &
#xinput set-prop 13 "Device Enabled" 0
xsetroot -cursor_name left_ptr
