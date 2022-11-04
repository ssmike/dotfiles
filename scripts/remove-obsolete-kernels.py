#!/bin/env python3
import os
import os.path
import argparse
import subprocess
import logging

logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
_log = logging.getLogger(__name__)


def shell(cmd, utf8=False, ensure_success=True):
    _log.info('call %s check=%o', cmd, ensure_success)
    completed = subprocess.run(cmd, capture_output=True, check=ensure_success)
    if utf8:
        return completed.stdout.decode('utf-8')
    else:
        return completed.stdout


def parse_version(s):
    result = []
    segment = ''
    for c in s:
        if not c.isdigit():
            result.append(int(segment))
            segment = ''
            if c != '.':
                break
        else:
            segment += c

    return result


parser = argparse.ArgumentParser()
parser.add_argument('--dry-run', action='store_true')
parser.add_argument('--no-mount', action='store_true')
args = parser.parse_args()

base_version = parse_version(shell(['uname', '-r'], utf8=True))
_log.info('base version %s', base_version)

mounted_boot = False

if not args.no_mount:
    shell(['mount', '/boot'], ensure_success=False)
    mounted_boot = True

dirs = ['/boot', '/boot/EFI/gentoo']
files = []

for directory in dirs:
    if os.path.exists(directory):
        for file in os.listdir(directory):
            fullname = os.path.join(directory, file)
            if os.path.isfile(fullname):
                files.append((file, fullname))

allowed_prefixes = [
    'vmlinuz-',
    'config-',
    'initramfs-',
    'System.map-',
]

collected = []
for fname, fullname in files:
    for prefix in allowed_prefixes:
        if '-gentoo' not in fname:
            continue
        if not fname.startswith(prefix):
            continue
        ver = parse_version(fname[len(prefix):])
        if ver < base_version:
            _log.info('delete %s', fullname)
            collected.append(fullname)
        else:
            _log.info('reject %s', fullname)

if not args.dry_run:
    for file in collected:
        os.unlink(file)

if not args.no_mount and mounted_boot:
    shell(['umount', '/boot'], ensure_success=False)
