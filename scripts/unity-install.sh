#!/bin/bash

CURRENT_PATH=`pwd`
echo "" > install-fails

for PACKAGE in `cat README | awk '/[0-9][0-9]:/ { print $2 }'`
do
        cd $PACKAGE
        for PACKAGE_FILE in `ls *.pkg.tar.xz`
        do
                echo "installing $PACKAGE_FILE"
                yaourt --noconfirm -U $PACKAGE_FILE || echo $PACKAGE_FILE >> $CURRENT_PATH/install-fails
        done
        cd $CURRENT_PATH
done
