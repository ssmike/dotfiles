#!/bin/bash
killall dzen2
killall stalonetray
killall nm-applet
if xrandr  | grep "DP1-1 connected"; then
    echo multiple
    if [ "$1" = "one" ]; then
        xrandr --output DP1-1 --off --output eDP1 --auto
    else
        xrandr --output eDP1 --auto --output DP1-1 --auto --scale 1.3x1.3 --right-of eDP1 --panning 2496x1404+1920+0
        cp ~/.config/nitrogen/multiple.cfg ~/.config/nitrogen/bg-saved.cfg
        mkfifo ~/.info
        tail -f -n1 ~/.info | dzen2 -geometry '+0-0' -dock -e "onstart=lower" -ta l -xs 2 &
    fi
else
    echo one
    xrandr --output DP1-1 --off --output eDP1 --auto
    cp ~/.config/nitrogen/onemonitor.cfg ~/.config/nitrogen/bg-saved.cfg
fi

nitrogen --restore
xmonad --restart

nm-applet &
