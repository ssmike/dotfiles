#!/bin/bash
/usr/bin/getmail --rcfile=sms --rcfile=aesc --rcfile=mike --rcfile=yandex
[ ! -z "`find /home/mike/mail/new -type f`" ] && notify-send "You have unread mail"
