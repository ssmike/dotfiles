#!/bin/bash

for i in $(cat /proc/cpuinfo | grep flags | sed "s/flags.*://g" | tail -n1) 
do
    grep "^$i " /usr/portage/profiles/use.desc
done
