#!/bin/bash
if pidof steam; then
    xdotool key ctrl+alt+F8;
else
    startx :1;
fi
