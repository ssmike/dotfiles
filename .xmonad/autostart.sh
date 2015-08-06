#!/bin/bash
xhost +
compton -cCGf -D 3 -b
cdemu-daemon &
#chromium &
nitrogen --restore &
krunner &
xfce4-power-manager &
~/.xmonad/dzen-auto.sh &
~/.xmonad/lightsOn.sh 120 &
gnome-keyring-daemon &
kwalletd &
nm-applet &
yakuake &
xbindkeys &
klipper &
kmail &
xautolock -locker "slock" -time 15 &
#~/app-copy/x86_64/CopyAgent &
#yandex-disk start &
#~/.mocpscrob/mocp-scrobbler.py --daemon &
#steam &
#syndaemon -t -i 1 -d 
