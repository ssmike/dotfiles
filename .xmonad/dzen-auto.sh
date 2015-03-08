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
#take ~/.info | dzen2 -y 1000 -dock & # -y 2000
conky -c ~/.conkyrc | /bin/sh | dzen2 -geometry +0-0 -dock -e - -ta l & 
sleep 1
stalonetray --grow-gravity NE --geometry 1x1-0-0 -i 20 --window-strut bottom -bg "#000000" --icon-size=19 --kludges=force_icons_size &
#stalonetray --grow-gravity NE --geometry 1x1-0-0 -i 20 --window-strut bottom -bg "#000000" &
#echo "" > ~/.info
#conky -c ~/.conky > ~/.info
