#!/bin/sh
case "$2" in
    up | vpn-down)
        timedatectl set-timezone "$(curl --fail https://ipapi.co/timezone)"
    ;;
esac
