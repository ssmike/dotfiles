#!/bin/bash
if [ ! -e ".git" ]; then
    echo "Should be run from repo's root";
    exit 1;
fi
blacklist=".git . .. make-symlinks.sh scripts"
for file in `ls -a`; do
    if ! grep $file <<<$blacklist; then
        if [ -e ~/$file ] && [ ! -h ~/$file ]; then
            rm -rf $file;
            mv ~/$file .;
        else
            rm -rf ~/$file;
        fi
        ln -s $PWD/$file ~/$file;
    fi
done

mkdir ~/.config/nvim/
ln -s ~/.vimrc ~/.config/nvim/init.vim
mkdir -p ~/.config/kitty/
ln -s ~/.kitty ~/.config/kitty/kitty.conf
