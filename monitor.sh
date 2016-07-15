#!/bin/bash
killall conky
killall stalonetray
if xrandr  | grep "HDMI3 connected"; then
    xrandr --auto --output HDMI3  --mode 1920x1080 --right-of LVDS1 
else
    xrandr --auto
fi
xmonad --restart
