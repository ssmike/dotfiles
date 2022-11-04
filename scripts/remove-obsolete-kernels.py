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
    digits_finished = False
    for c in s:
        if not c.isdigit() and not digits_finished:
            result.append(int(segment))
            segment = ''
            if c != '.':
                digits_finished = True
        else:
            if digits_finished and c == '.':
                break
            segment += c
    return (result, segment)


parser = argparse.ArgumentParser()
parser.add_argument('--dry-run', action='store_true')
parser.add_argument('--no-mount', action='store_true')
args = parser.parse_args()

base_version, base_stream = parse_version(shell(['uname', '-r'], utf8=True))
_log.info('base version %s, %s', base_version, base_stream)

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
        ver, stream = parse_version(fname[len(prefix):])
        if ver < base_version and stream == base_stream:
            _log.info('delete %s version %s %s', fullname, ver, stream)
            collected.append(fullname)
        else:
            _log.info('reject %s version %s %s', fullname, ver, stream)

if not args.dry_run:
    for file in collected:
        os.unlink(file)

if not args.no_mount and mounted_boot:
    shell(['umount', '/boot'], ensure_success=False)
