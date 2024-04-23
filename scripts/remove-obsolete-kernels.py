#!/bin/env python3
import os
import os.path
import argparse
import subprocess
import logging
import shutil
import re


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
    segment = segment.strip()
    match = re.match('^(.*)-r([0-9]*)$', segment)
    if match:
        result.append(int(match[2]))
        _log.debug('found -r patch number %s', match[2])
        segment = match[1]
    return (tuple(result), segment)


base_version, base_stream = None, None
keep_versions = set()


def ensure_actual_version(args):
    global base_version, base_stream

    if base_version is not None:
        return

    kernel_ver = args.base_bootable_kernel
    if kernel_ver is None:
        kernel_ver = shell(['uname', '-r'], utf8=True)

    for version in args.keep_versions:
        print(parse_version(version))
        keep_versions.add(parse_version(version))

    base_version, base_stream = parse_version(kernel_ver)
    _log.info('base version %s, %s', base_version, base_stream)


def version_to_delete(version, fname):
    global keep_versions, base_version, base_stream
    ver, stream = parse_version(version)
    if ver < base_version and stream == base_stream and (ver, stream) not in keep_versions:
        _log.info('delete %s version %s %s', fname, ver, stream)
        return True
    else:
        _log.info('keep %s version %s %s', fname, ver, stream)
        return False


def confirm_deletion(collected, rmtree=True):
    if collected:
        confirm = input('delete files [y/n] ')
        if confirm == 'y':
            for file in collected:
                if rmtree:
                    shutil.rmtree(file)
                else:
                    os.unlink(file)


def remove_kernel_modules(args):
    ensure_actual_version(args)
    collected = []
    for dr in args.kernel_modules_base:
        for name in os.listdir(dr):
            fname = os.path.join(dr, name)
            if version_to_delete(name, fname):
                collected.append(fname)
    confirm_deletion(collected)


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

    confirm_deletion(collected_srcs)


def remove_bootable_kernels(args):
    ensure_actual_version(args)
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
                if version_to_delete(fname[len(prefix):], fname):
                    collected.append(fullname)

        confirm_deletion(collected, rmtree=False)


parser = argparse.ArgumentParser()
# parser.add_argument('--dry-run', action='store_true')
parser.add_argument('--no-mount', action='store_true')
parser.add_argument('--base-src-path', default='/usr/src/')
parser.add_argument('--base-bootable-kernel', default=None)
parser.add_argument('--bootable-dirs', action='append', default=['/boot', '/boot/EFI/gentoo'])
parser.add_argument('--kernel-modules-base', action='append', default=['/lib/modules'])

parser.add_argument('--no-remove-kernel-srcs', default=False, action='store_true')
parser.add_argument('--no-remove-kernels', default=False, action='store_true')
parser.add_argument('--no-remove-modules', default=False, action='store_true')

parser.add_argument('--keep-versions', action='append', default=[])
args = parser.parse_args()

if not args.no_remove_kernel_srcs:
    delete_kernel_srcs(args)

if not args.no_remove_kernels:
    remove_bootable_kernels(args)

if not args.no_remove_modules:
    remove_kernel_modules(args)
