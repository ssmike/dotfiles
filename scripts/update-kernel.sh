#!/bin/bash
cd /usr/src/linux
zcat /proc/config.gz > ./.config
make oldconfig
make -j4
make modules_install
make install
dracut -H -k `readlink /usr/src/linux | sed -e 's/linux-/\/lib\/modules\//'` -f
grub-mkconfig > /boot/grub/grub.cfg
