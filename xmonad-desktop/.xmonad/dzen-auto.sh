#!/bin/bash
#conky | sh | /usr/bin/dzen2 -dock -x 610 -y 0 1
function take {
    while true 
    do
        cat $1
        echo ""
    done
}

killall conky
killall trayer-srg
#mkfifo ~/.info
#take ~/.info | dzen2 -y 1000 -dock & # -y 2000
conky -c ~/.xmonad/.conkyrc | /bin/sh | dzen2 -geometry '+0-0' -dock -e "onstart=lower" -ta l -xs `cat ~/.xmonad/primary_monitor` & 
sleep 1
trayer-srg --transparent true --edge bottom --monitor $((2 - `cat ~/.xmonad/primary_monitor`)) --align right --expand false --width 9 --alpha 255 --widthtype request --height 21 &
#top
#stalonetray --grow-gravity NE --geometry 1x1-0-0 -i 20 --window-strut bottom -bg "#000000" &
#echo "" > ~/.info
#conky -c ~/.conky > ~/.info
