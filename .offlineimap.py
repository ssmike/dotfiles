#! /usr/bin/env python2
from subprocess import check_output
import sys
import os
import json
import binascii
import codecs
import gnomekeyring as gkey
import getpass

def set_credentials(repo, user, pw):
    KEYRING_NAME = "offlineimap"
    attrs = { "repo": repo, "user": user }
    keyring = gkey.get_default_keyring_sync()
    gkey.item_create_sync(keyring, gkey.ITEM_NETWORK_PASSWORD,
        KEYRING_NAME, attrs, pw, True)

def get_credentials(repo):
    keyring = gkey.get_default_keyring_sync()
    attrs = {"repo": repo}
    items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
    return (items[0].attributes["user"], items[0].secret)

def get_username(repo):
    return get_credentials(repo)[0]
def get_password(repo):
    return get_credentials(repo)[1]

def get_pass(user):
    content = json.loads(check_output("gpg -dq ~/.offlineimap.gpg", shell=True).strip("\n"))
    return content[user]

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print "Usage: %s <repository> <username>" \
            % (os.path.basename(sys.argv[0]))
        sys.exit(0)
    repo, username = sys.argv[1:]
    password = getpass.getpass("Enter password for user '%s': " % username)
    password_confirmation = getpass.getpass("Confirm password: ")
    if password != password_confirmation:
        print "Error: password confirmation does not match"
        sys.exit(1)
    set_credentials(repo, username, password)
