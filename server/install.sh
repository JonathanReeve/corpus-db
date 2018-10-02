#!/run/current-system/sw/bin/bash

# A script for automating the NixOS install process to Vultr
# This assumes you've already deployed a new server using
# the latest NixOS ISO (grab the URL from the NixOS website).

# Run this script by opening the console, then running as root:
# nix-env -iA nixos.git
# git clone https://github.com/JonathanReeve/corpus-db
# cd server
# ./install.sh

# Untested! Beware
parted -a optimal /dev/vda mkpart primary btrfs 0% 100% mklabel root
mount /dev/disk/by-label/root /mnt
cp configuration.nix hardware-configuration.nix /mnt/etc/nixos/
nixos-install
