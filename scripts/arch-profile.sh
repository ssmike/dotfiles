#!/bin/bash
export PATH=/bin:/sbin:/usr/sbin:/usr/bin
DIR="/arch/proc/"

if [ "$(ls -A $DIR)" ]; then
    echo "arch chroot is already setted-up"
else
    mount --bind /dev/ /arch/dev
    mount --bind /proc /arch/proc
    mount --bind /sys /arch/sys
    mount --bind /dev/shm /arch/dev/shm/
    mount --bind / /arch/gentoo/
    cp /etc/resolv.conf /arch/etc/resolv.conf
fi

chroot /arch /bin/bash
