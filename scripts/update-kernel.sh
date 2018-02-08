#!/bin/bash
cd /usr/src/linux
zcat /proc/config.gz > ./.config
make oldconfig
make -j4
make modules_install
make install
ver=`readlink /usr/src/linux | sed -e 's/linux-//'`
dracut -H -f --kver $ver
grub-mkconfig > /boot/grub/grub.cfg
