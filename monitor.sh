#!/bin/bash
killall conky
killall stalonetray
if xrandr  | grep "HDMI3 connected"; then
    xrandr --auto --output HDMI3  --mode 1920x1080 --right-of LVDS1 
    cp ~/.config/nitrogen/multiple.cfg ~/.config/nitrogen/bg-saved.cfg
else
    xrandr --auto
    cp ~/.config/nitrogen/onemonitor.cfg ~/.config/nitrogen/bg-saved.cfg
fi

nitrogen --restore
xmonad --restart
setxkbmap -layout us,ru -variant -option grp:caps_toggle
