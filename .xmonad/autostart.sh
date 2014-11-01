#!/bin/bash
#xhost +
compton -cCGf -D 3 -b
cdemu-daemon &
#~/.mocpscrob/mocp-scrobbler.py --daemon &
#steam &
#syndaemon -t -i 1 -d 
