#!/bin/bash
cd /usr/src/linux
cp .config ../config-$(uname -r)
make -j4
make modules_install -j4
#check if we are compiling different kernel
ACTUAL=$(uname -r)
NEW=$(cat /usr/src/linux/.config | grep "Kernel Configuration" | sed -e 's/.* \(.*\) Kernel Configuration/\1/g')
if [ ! $ACTUAL = $NEW ]; then 
    emerge @module-rebuild
fi

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
