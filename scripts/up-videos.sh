#!/bin/zsh

for f in ~/Downloads/**/*.{avi,mp4,mkv}; do
    ln -s $f ~/Videos/
done

for f in ~/Videos/*; do
    if [ ! -f $f ]; then
        rm $f
    fi
done
