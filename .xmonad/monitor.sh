#!/bin/bash
if [ "x$1" != "xstart" ]; then
    killall dzen2
    killall nm-applet
fi

if xrandr  | grep "DP1-1 connected"; then
    if [ "$1" = "one" ]; then
        xrandr --output HDMI2 --off\
            --output HDMI1 --off\
            --output DP1 --off\
            --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal\
            --output VIRTUAL1 --off \
            --output DP1-1 --off \
            --output DP1-2 --off
    else
        echo multiple
        xrandr --output eDP1 --mode 1920x1080 --pos 2496x0 --verbose
        xrandr --output DP1-1 --primary --mode 1920x1080 --pos 0x0 --verbose
        xrandr --output DP1-2 --mode 1920x1080 --pos 4416x0 --verbose
    fi
else
    echo one
    xrandr --output HDMI2 --off\
        --output HDMI1 --off\
        --output DP1 --off\
        --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal\
        --output VIRTUAL1 --off \
        --output DP1-1 --off \
        --output DP1-2 --off
fi

if [ "x$1" != "xstart" ]; then
    nitrogen --restore
    xmonad --restart

    nm-applet &
fi
