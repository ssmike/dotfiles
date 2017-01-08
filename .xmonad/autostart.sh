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
#xscreensaver -no-splash &
yandex-disk start &
Telegram &
xautolock -locker "slock" -time 10 -detectsleep &
/usr/libexec/polkit-gnome-authentication-agent-1 &
#xinput set-prop 13 "Device Enabled" 0
