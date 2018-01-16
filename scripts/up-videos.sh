#!/bin/zsh

for ext in avi mp4 mkv mpeg; do
    find -iname "*.$ext" -exec ln -s {} ~/Videos/ \;
done

for f in ~/Videos/*; do
    if [ ! -f $f ]; then
        rm $f
    fi
done
