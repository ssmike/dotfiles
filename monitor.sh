#!/bin/bash
killall dzen2
killall stalonetray
if xrandr  | grep "DP1-1 connected"; then
    echo multiple
    xrandr --auto --output DP1-1 --mode 1920x1080 --right-of eDP1
    cp ~/.config/nitrogen/multiple.cfg ~/.config/nitrogen/bg-saved.cfg
    dzen2 -geometry '+0-0' -dock -e "onstart=lower" -ta l -xs 1 &
    nitrogen --restore
    xmonad --restart
else
    echo one
    xrandr --auto
    nitrogen --restore
    cp ~/.config/nitrogen/onemonitor.cfg ~/.config/nitrogen/bg-saved.cfg
    xmonad --restart
fi

setxkbmap -layout us,ru -variant -option grp:caps_toggle
