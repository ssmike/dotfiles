#!/bin/bash

mount /btrfs-root

SNAME=root-$(date +%Y-%m-%d-%H)

cd /btrfs-root
if ! ls /btrfs-root/snapshots/ | grep $SNAME > /dev/null; then
    btrfs subvolume snapshot root snapshots/$SNAME
fi

SCOUNT=$(ls snapshots | wc -l)
MIN_SNAP=$(ls snapshots | head -n 1)

if (( $SCOUNT > 4 )); then
    btrfs subvolume delete snapshots/$MIN_SNAP
fi

cd /

umount /btrfs-root
