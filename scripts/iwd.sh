#!/bin/bash
systemctl stop NetworkManager
systemctl stop dhcpcd
systemctl start iwd
