#!/bin/bash
id=`xinput | grep 'Magic Trackpad' | sed -e 's/.*id=\([0-9]*\)\s.*/\1/g'`
if [[ $id != '' ]]; then 
    xinput set-prop $id 'Synaptics Tap Action' 2 0 0 0 1 0 0
fi
