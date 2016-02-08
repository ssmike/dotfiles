#!/bin/bash
pgrep  compton && exit
#xhost +
compton -cCGf -D 3 -b
cdemu-daemon &
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
klipper &

xscreensaver -no-splash &
#xautolock -locker "slock" -time 5 &
~/scripts/reminders.sh
yandex-disk start &
~/.dropbox-dist/dropboxd &
setxkbmap us,ru 'grp:caps_toggle'
lxpolkit &
#~/.mocpscrob/mocp-scrobbler.py --daemon &
#steam &
#syndaemon -t -i 1 -d 
