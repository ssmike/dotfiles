#!/bin/bash
export PATH=/bin:/sbin:/usr/sbin:/usr/bin
mount --bind /dev/ /gentoo/dev
mount --bind /proc /gentoo/proc
mount --bind /sys /gentoo/sys
mount --bind /dev/shm /gentoo/dev/shm/
cp /etc/resolv.conf /gentoo/etc/resolv.conf
chroot /gentoo /bin/bash
