#!/bin/bash
cd /usr/src/linux
if [ ! -f /usr/src/linux/.config.old2 ]; then
    zcat /proc/config.gz > ./.config
    make oldconfig
fi
make -j4
make modules_install
mount /boot
make install
ver=`readlink /usr/src/linux | sed -e 's/linux-//'`
dracut -H -f --kver $ver
emerge @module-rebuild
if which grub-mkconfig; then
    grub-mkconfig > /boot/grub/grub.cfg;
else
    mv /boot/vmlinuz-* /boot/EFI/gentoo
    mv /boot/initramfs-* /boot/EFI/gentoo
fi
umount /boot
