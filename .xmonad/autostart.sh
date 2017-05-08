#!/bin/bash
pgrep  compton && exit
#xhost +
redshift &
compton -f -D 3 -b
nitrogen --restore &
xfce4-power-manager &
~/.xmonad/lightsOn.sh 120 &
gnome-keyring-daemon &
nm-applet &
parcellite &
xbindkeys &
#xscreensaver -no-splash &
yandex-disk start &
vivaldi &
thunderbird &
Telegram &
#xautolock -locker "slock" -time 10 -detectsleep &
/usr/libexec/polkit-gnome-authentication-agent-1 &
#xinput set-prop 13 "Device Enabled" 0

/usr/libexec/tracker-miner-fs &
/usr/libexec/tracker-extract &
/usr/libexec/tracker-miner-user-guides &
/usr/libexec/tracker-store &
