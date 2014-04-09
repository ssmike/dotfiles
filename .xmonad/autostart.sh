#!/bin/bash
xsetroot -cursor_name left_ptr
xbindkeys &
nm-applet &
blueman-applet &
#xhost +
nitrogen --restore
wmname "LG3D"
compton -cCGf -D 3 -b
yandex-disk start &
synclient RTCornerButton=2
synclient TapButton1=1
synclient TapButton2=3
synclient Palmdetect=1
synclient PalmMinZ=80
synclient HorizTwoFingerScroll=1
setxkbmap -layout "us,ru" -option "grp:alt_shift_toggle"
/usr/libexec/polkit-gnome-authentication-agent-1  &
~/.xmonad/dzen-auto.sh &
#syndaemon -t -i 1 -d 
