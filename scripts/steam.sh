#!/bin/bash
if pidof steam; then
    xdotool key ctrl+alt+F3;
else
    startx :1;
fi
