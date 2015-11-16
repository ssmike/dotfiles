#!/bin/bash
killall remind
remind -z10 ~/.reminders | xargs -I '{}' -d'\n' notify-send 'напоминалка' '{}' &
