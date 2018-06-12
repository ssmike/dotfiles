#!/bin/zsh

SCANDIR="$1"
OUTDIR="$2"

for ext in avi mp4 mkv mpeg; do
    find "$SCANDIR" | grep ".$ext$" | xargs -I '{}' ln -s '{}' "$OUTDIR"
done

for f in "$OUTDIR"/*; do
    if [ ! -f $f ]; then
        rm $f
    fi
done
