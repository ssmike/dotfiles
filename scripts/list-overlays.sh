#!/bin/bash
for i in /var/db/pkg/*/*; do
    if ! grep gentoo $i/repository >/dev/null; then
        echo -e "`basename $i`\t`cat $i/repository`";
    fi;
done
