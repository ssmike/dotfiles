#!/bin/bash
if pidof steam; then
    xdotool key ctrl+alt+F2;
else
    startx :1;
fi
