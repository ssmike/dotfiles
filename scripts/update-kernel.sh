#!/bin/bash
set -e

cd /usr/src/linux
if [ ! -f /usr/src/linux/.config ]; then
    zcat /proc/config.gz > ./.config
    make oldconfig
fi
make -j4
make modules_install
mount /boot
make install
ver=`readlink /usr/src/linux | sed -e 's/linux-//'`
dracut -H -f --kver $ver

if [[ "$ver" != "`uname -r`" ]]; then
    emerge @module-rebuild
else
    echo "kernel version didn't changed; don't rebuild modules"
fi

if which grub-mkconfig; then
    grub-mkconfig > /boot/grub/grub.cfg;
else
    mv /boot/vmlinuz-* /boot/EFI/gentoo
    mv /boot/initramfs-* /boot/EFI/gentoo
    mv /boot/config-* /boot/EFI/gentoo
    mv /boot/System.map-* /boot/EFI/gentoo
fi
umount /boot
