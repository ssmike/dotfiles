#!/bin/bash
cd /usr/src/linux
make -j4
make modules_install -j4
emerge @module-rebuild
mount -o remount,rw /boot

#backup kernel & initramfs
echo "make backups[Y/n]"
notify-send "rebuilder" "kernel compilation finished"
read -r RESPONSE
if [ ! $RESPONSE = n ] ; then 
    cp /boot/gentoo-kernel /boot/backup-gentoo-kernel
    cp /boot/initramfs /boot/backup-initramfs
fi

#copy new kernel & initramfs
cp /usr/src/linux/arch/x86_64/boot/bzImage /boot/gentoo-kernel
rm /boot/initramfs*
dracut --host-only --force
mv /boot/initramfs* /boot/initramfs
mount -o remount,ro /boot
