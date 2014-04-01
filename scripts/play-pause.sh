#!/bin/bash

if mocp -i; then
    state=$(mocp -i | sed "s/.* \(.*\)/\1/g")
    if [ $state = STOP ]; then 
        mocp -p;
    else
        mocp -G;
    fi
else
    mocp -S;
    mocp -p;
fi
