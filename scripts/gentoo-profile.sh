#!/bin/bash
export PATH=/bin:/sbin:/usr/sbin:/usr/bin
DIR="/gentoo/proc/"

if [ "$(ls -A $DIR)" ]; then
    echo "gentoo chroot is already setted-up"
else
    mount --bind /dev/ /gentoo/dev
    mount --bind /proc /gentoo/proc
    mount --bind /sys /gentoo/sys
    mount --bind /dev/shm /gentoo/dev/shm/
    mount --bind / /gentoo/arch
    cp /etc/resolv.conf /gentoo/etc/resolv.conf
fi

chroot /gentoo /bin/bash
