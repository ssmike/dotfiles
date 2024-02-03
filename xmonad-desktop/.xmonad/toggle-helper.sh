#!/bin/bash
if [ `xrandr | grep '*' | wc -l` = "2" ]; then
    echo force one;
    ~/.xmonad/monitor.sh one;
else
    echo don\'t force;
    ~/.xmonad/monitor.sh;
fi
