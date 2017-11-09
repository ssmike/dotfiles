#!/bin/bash
if xrandr  | grep "DP1-1 connected"; then
    echo multiple
    if [ "$1" = "one" ]; then
        xrandr --output DP1-1 --off --output eDP1 --auto
    else
        xrandr --output eDP1 --auto --output DP1-1 --auto --scale 1.3x1.3 --right-of eDP1 --panning 2496x1404+1920+0
    fi
else
    echo one
    xrandr --output DP1-1 --off --output eDP1 --auto
fi
