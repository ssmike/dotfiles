#!/bin/bash
ifne () {
        read line || return 1
        (echo "$line"; cat) | eval "$@"
}
export DISPLAY=:0
remind ~/.reminders | sed "/^No reminders.$/d" | ifne notify-send '"reminders" "$(remind ~/.reminders | tail -n+2)" '
