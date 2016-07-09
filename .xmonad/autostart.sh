#!/bin/bash
pgrep  compton && exit
#xhost +
compton -cCGf -D 3 -b
#chromium &
nitrogen --restore &
#krunner &
xfce4-power-manager &
~/.xmonad/dzen-auto.sh &
~/.xmonad/lightsOn.sh 120 &
gnome-keyring-daemon &
nm-applet &
parcellite &
xbindkeys &
ssh-agent &
xscreensaver -no-splash &
#xautolock -locker "slock" -time 5 &
setxkbmap us,ru 'grp:caps_toggle'
Telegram --startintray &
setxkbmap -layout us,ru -variant -option grp:caps_toggle
