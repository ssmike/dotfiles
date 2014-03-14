#!/bin/bash
cd /usr/src/linux
make -j4
make modules_install -j4
emerge @module-rebuild
mount -o remount,rw /boot
cp /usr/src/linux/arch/x86_64/boot/bzImage /boot/gentoo-kernel
rm /boot/initramfs*
dracut --host-only --force
mv /boot/initramfs* /boot/initramfs
mount -o remount,ro /boot
