#!/bin/bash
if [ ! -e ".git" ]; then
    echo "Should be run from repo's root";
    exit 1;
fi
blacklist=".git . .. make-symlinks.sh"
for file in `ls -a`; do
    if ! grep $file <<<$blacklist; then
        if [ -e ~/$file ] && [ ! -h ~/$file ]; then
            rm -r $file;
            cp -r ~/$file .;
        else
            rm ~/$file;
        fi
        ln -s $PWD/$file ~/$file;
    fi
done
