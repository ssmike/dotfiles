#!/bin/bash
xsetroot -cursor_name left_ptr
xbindkeys &
nm-applet &
blueman-applet &
xhost +
nitrogen --restore
wmname "LG3D"
/usr/libexec/polkit-gnome-authentication-agent-1  &
~/.xmonad/dzen-auto.sh
