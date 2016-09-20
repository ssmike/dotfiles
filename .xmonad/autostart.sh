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
ssh-agent &
xscreensaver -no-splash &
yandex-disk start &
#xautolock -locker "slock" -time 5 &
setxkbmap us,ru 'grp:caps_toggle'
~/Downloads/Telegram/Telegram &
lxpolkit &
setxkbmap -layout us,ru -variant -option grp:caps_toggle
xinput set-prop 15 "Device Enabled" 0
