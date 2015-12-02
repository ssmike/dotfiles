#!/bin/bash
killall remind
remind -z10 ~/.reminders | sed -u -e "/^$/d" | xargs -I '{}' -d'\n' notify-send 'напоминалка' '{}' &

