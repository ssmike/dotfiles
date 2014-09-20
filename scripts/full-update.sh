#!/bin/bash
eix-sync && eix-update && emerge --newuse --update --deep @world --with-bdeps=y --backtrack=30 --ask && emerge --depclean && eclean distfiles
