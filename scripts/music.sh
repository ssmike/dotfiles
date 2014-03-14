#!/bin/bash

MUSIC_DIR=/home/mike/Music
MASK=*.mp3


for file in $MUSIC_DIR/$MASK
do
    if ! [ -d "$file" ]; then
        AUTHOR=$(id3info "$file" | grep TPE1 | sed  "s/.*: //g")
        mkdir -p "$MUSIC_DIR/$AUTHOR";
        mv "$file" "$MUSIC_DIR/$AUTHOR/";
    fi
done
