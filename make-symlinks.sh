#!/bin/bash
if [ ! -e "./make-symlinks.sh" ]; then
    echo "Should be run from repo's root";
    exit 1;
fi
blacklist=".git . .. make-symlinks.sh scripts coc-settings.json"
for file in `ls -a`; do
    if ! grep $file <<<$blacklist >/dev/null; then
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
ln -s $PWD/coc-settings.json ~/.config/nvim/coc-settings.json
mkdir -p ~/.vim
ln -s $PWD/coc-settings.json ~/.vim/coc-settings.json

mkdir -p ~/.config/kitty/
ln -s ~/.kitty ~/.config/kitty/kitty.conf

mkdir -p ~/.config/
ln -s $PWD/.redshift ~/.config/redshift.conf

mkdir -p ~/.config/touchegg
ln -s $PWD/.touchegg ~/.config/touchegg/touchegg.conf

