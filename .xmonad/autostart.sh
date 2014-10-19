#!/bin/bash
#xhost +
compton -cCGf -D 3 -b
yandex-disk start &
~/app-copy/x86_64/CopyAgent &
cdemu-daemon &
~/.mocpscrob/mocp-scrobbler.py --daemon &
claws-mail &
clementine &
#gcdemu &
#steam &
#syndaemon -t -i 1 -d 
