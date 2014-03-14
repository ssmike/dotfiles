#/bin/bash

CURRENT_PATH=`pwd`
echo "" > build-fails

for PACKAGE in `cat README | awk '/[0-9][0-9]:/ { print $2 }'`
do
        echo ""
        echo "Building $PACKAGE"
        cd $PACKAGE
        makepkg || echo $PACKAGE >> $CURRENT_PATH/build-fails
        rm -rf src pkg
        cd $CURRENT_PATH
done
