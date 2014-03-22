#!/bin/bash
xsetroot -cursor_name left_ptr
xbindkeys &
nm-applet &
blueman-applet &
xhost +
nitrogen --restore
wmname "LG3D"
compton -cCGf -D 3 -b
yandex-disk start &
synclient TapButton1=1
setxkbmap -layout "us,ru" -option "grp:alt_shift_toggle"
/usr/libexec/polkit-gnome-authentication-agent-1  &
~/.xmonad/dzen-auto.sh

