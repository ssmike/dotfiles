#!/bin/python
# fetch packs by wget -r http://stkaddons.net

import os
from subprocess import call
import xml.etree.ElementTree as ET
from shutil import copyfile

ls = os.listdir('.')

preffix = '/tmp/trash'
supertuxpreffix = '/home/mike/.local/share/supertuxkart/addons'

def copyall(dir1, dir2):
    os.mkdir(dir2)
    for fl in os.listdir(dir1):
        copyfile(os.path.join(dir1, fl), os.path.join('dir1'))

def addonmove():
    if os.path.isfile(os.path.join(preffix, 'track.xml')): 
        tree = ET.parse(os.path.join(preffix, 'track.xml'))
        root = tree.getroot()
        name = root.attrib['name']
        os.mkdir(os.path.join(supertuxpreffix, name))
        copyall(preffix, os.path.join(supertuxpreffix, 'tracks', name))
    elif os.path.isfile(os.path.join(preffix, 'kart.xml')): 
        tree = ET.parse(os.path.join(preffix, 'kart.xml'))
        root = tree.getroot()
        name = root.attrib['name']
        os.mkdir(os.path.join(supertuxpreffix, name))
        copyall(preffix, os.path.join(supertuxpreffix, 'karts', name))
        #kart directory
    else: pass

for s in ls:
    if 'zip' in s:
        call(["rm", "-r", preffix])
        call(["mkdir", preffix])
        call(["unzip", s, "-d", preffix])
        addonmove()
