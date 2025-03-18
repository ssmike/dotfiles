#!/bin/bash
if [ "x$1" != "xstart" ]; then
    killall dzen2
    killall nm-applet
    killall redshift
    killall update-notifier-tray
fi

if xrandr  | grep "HDMI-1-0 connected"; then
    if [ "$1" = "one" ]; then
        xrandr --output HDMI2 --off\
            --output HDMI-1-0 --off\
            --output DP1 --off\
            --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal\
            --output VIRTUAL1 --off \
            --output DP1-1 --off \
            --output DP1-2 --off
    else
        echo multiple
        xrandr --output eDP-1 --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-1-0 --off --output DP-1-1 --off --output HDMI-1-0 --mode 1920x1080 --pos 0x0 --rotate normal
    fi
else
    echo one
    xrandr --output HDMI2 --off\
        --output HDMI-1-0 --off\
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
