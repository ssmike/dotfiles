#!/bin/bash
export DISPLAY=:0
remind -z10 ~/.reminders | xargs -I '{}' -d'\n' notify-send '{}'
