#!/bin/env python3
import os
import os.path
import argparse
import subprocess
import logging
import shutil

logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.DEBUG)
_log = logging.getLogger(__name__)


def shell(cmd, utf8=False, ensure_success=True):
    _log.debug('call %s check=%o', cmd, ensure_success)
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
    return (result, segment.strip())


class MountedBoot:
    def __init__(self, enabled):
        self.enabled = enabled

    def __enter__(self):
        if self.enabled:
            shell(['mount', '/boot'], ensure_success=False)

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.enabled:
            shell(['umount', '/boot'], ensure_success=False)
        if exc_val:
            raise


def delete_kernel_srcs(args):
    collected_srcs = []
    for file in os.listdir(args.base_src_path):
        joined = os.path.join(args.base_src_path, file)

        if not os.path.isdir(joined):
            continue
        if os.path.islink(joined):
            _log.debug('skip symlink %s', joined)
            continue

        if os.path.isfile(os.path.join(joined, 'Makefile')):
            _log.debug('skip valid kernel src %s', joined)
        else:
            _log.info('going to delete %s', joined)
            collected_srcs.append(joined)

    if not collected_srcs:
        return

    confirm = input('delete files [y/n] ')
    if confirm == 'y':
        for file in collected_srcs:
            shutil.rmtree(file)


def remove_bootable_kernels(args):
    kernel_ver = args.base_bootable_kernel
    if kernel_ver is None:
        kernel_ver = shell(['uname', '-r'], utf8=True)

    base_version, base_stream = parse_version(kernel_ver)
    _log.info('base version %s, %s', base_version, base_stream)

    with MountedBoot(not args.no_mount):
        dirs = args.bootable_dirs
        _log.debug('assume kernel dirs %s', dirs)
        files = []

        for directory in dirs:
            if os.path.exists(directory):
                for file in os.listdir(directory):
                    fullname = os.path.join(directory, file)
                    if os.path.isfile(fullname):
                        files.append((file, fullname))

        allowed_prefixes = [
            'kernel-',
            'vmlinuz-',
            'config-',
            'initramfs-',
            'System.map-',
        ]

        collected = []
        for fname, fullname in files:
            for prefix in allowed_prefixes:
                # if '-gentoo' not in fname:
                #     continue
                if not fname.startswith(prefix):
                    continue
                ver, stream = parse_version(fname[len(prefix):])
                if ver < base_version and stream == base_stream:
                    _log.info('delete %s version %s %s', fullname, ver, stream)
                    collected.append(fullname)
                else:
                    _log.info('reject %s version %s %s', fullname, ver, stream)

        if collected:
            confirm = input('delete files [y/n] ')
            if confirm == 'y':
                for file in collected:
                    os.unlink(file)


parser = argparse.ArgumentParser()
# parser.add_argument('--dry-run', action='store_true')
parser.add_argument('--no-mount', action='store_true')
parser.add_argument('--base-src-path', default='/usr/src/')
parser.add_argument('--base-bootable-kernel', default=None)
parser.add_argument('--bootable-dirs', action='append', default=['/boot', '/boot/EFI/gentoo'])

parser.add_argument('--no-remove-kernel-srcs', default=False, action='store_true')
parser.add_argument('--no-remove-kernels', default=False, action='store_true')
args = parser.parse_args()

if not args.no_remove_kernel_srcs:
    delete_kernel_srcs(args)

if not args.no_remove_kernels:
    remove_bootable_kernels(args)
