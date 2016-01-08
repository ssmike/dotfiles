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
conky -c ~/.conkyrc | /bin/sh | dzen2 -geometry +0-0 -dock -e "onstart=lower" -ta l & 
sleep 1
stalonetray --grow-gravity NE --window-strut bottom -bg "#000000" --kludges=fix_window_pos,use_icons_hints,force_icons_size -i 19 --icon-gravity NE --geometry 7x1-0-0 --no-shrink --window-layer normal&
#top
#stalonetray --grow-gravity NE --geometry 1x1-0-0 -i 20 --window-strut bottom -bg "#000000" &
#echo "" > ~/.info
#conky -c ~/.conky > ~/.info
