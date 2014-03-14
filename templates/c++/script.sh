#!/bin/bash

while python ./gen.py && ./main;
do
    echo OK;
done
