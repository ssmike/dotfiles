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

if which grub-mkconfig; then
    dracut -H -f --kver $ver
else
    dracut -H -f --kver $ver /boot/initramfs-$ver.img
fi

if [[ "$ver" != "`uname -r`" ]]; then
    emerge @module-rebuild
else
    echo "kernel version didn't changed; don't rebuild modules"
fi

echo "free space in /boot"
df -h /boot/
if which grub-mkconfig; then
    grub-mkconfig > /boot/grub/grub.cfg;
else
    mv /boot/vmlinuz-* /boot/EFI/gentoo || echo 'vmlinuz- not found'
    mv /boot/kernel-* /boot/EFI/gentoo || echo 'kernel- not found'
    mv /boot/initramfs-* /boot/EFI/gentoo || echo 'initramfs- not found'
    mv /boot/config-* /boot/EFI/gentoo || echo 'config- not found'
    mv /boot/System.map-* /boot/EFI/gentoo || echo 'system- not found'
fi
umount /boot
