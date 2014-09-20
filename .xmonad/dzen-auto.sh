#!/bin/bash
#conky | sh | /usr/bin/dzen2 -dock -x 610 -y 0 &
function take {
    while true 
    do
        cat $1
        echo ""
    done
}

pgrep conky && exit
#mkfifo ~/.info
#take ~/.info | dzen2 -y 1000 -dock & 
conky -c ~/.conkyrc | /bin/sh | dzen2 -y 2000 -dock -e - -ta l & 
#echo "" > ~/.info
#conky -c ~/.conky > ~/.info
