#!/bin/bash
pgrep  compton && exit
#xhost +
compton -f -D 3 -b
nitrogen --restore &
dunst &
parcellite &
~/.xmonad/lightsOn.sh 120 &
xbindkeys &
#xscreensaver -no-splash &
#xautolock -locker "slock" -time 10 -detectsleep &
/usr/libexec/polkit-gnome-authentication-agent-1 &
#xinput set-prop 13 "Device Enabled" 0

function launch(){
python - "$@" <<EOF
import sys
from gi.repository import Gio
Gio.DesktopAppInfo.new_from_filename(sys.argv[1]).launch_uris(sys.argv[2:])
EOF
}

for file in ~/.config/autostart/*.desktop; do
    launch $file;
done
