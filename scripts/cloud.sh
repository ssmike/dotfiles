#!/bin/bash
cd /home/mike
wget "https://desktopcloud.cdnmail.ru/linux/i386/cloud"
chmod +x ./cloud
./cloud  -acceptLicense -email smstrash@mail.ru -folder /home/mike/Mailru -password morningstar
rm ./cloud
