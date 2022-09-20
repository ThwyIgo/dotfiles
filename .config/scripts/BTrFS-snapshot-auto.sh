#!/usr/bin/env bash
# Created by Thwy. Inspired by https://github.com/linuxdabbler/personal-dot-files/blob/master/scripts/snaphome
# DON'T MAKE 2 SNAPSHOTS OF THE SAME SUBVOLUME IN LESS THAN 1MIN. You'll break the script
# Set the variables "rootsv rootPath homesv homePath snapsv snapPath" before running the script

if [ $(whoami) != 'root' ]; then
    printf "This script must be run as root (BTrFS limitation, sorry)\n" && exit 1
fi

# Number of snapshots allowed
sn=4
# Colors
RED='\033[0;31m'
NC='\033[0m'
yellow='\033[1;33m'
# Vars
today="$(date +"%Y-%m-%d-%R")"
rootsv="@" # Name of the root subvol (uncomment below to get it automatically)
#rootsv="$(btrfs subvolume list / | awk '/level 5/ {print $NF}'| head -n1)"
rootPath=/
homesv="@home" # Name of the home subvol (uncomment below to get it automatically)
#homesv="$(btrfs subvolume list / | awk '/level 5/ && /home/ {print $NF}')"
homePath=/home
snapsv="@snapshots" # Name of the snapshots subvol (uncomment below to get it automatically)
#snapsv="$(btrfs subvolume list / | awk '/level 5/ && /snap/ {print $NF}')"
snapPath=/.snapshots

#Actual code
numSnapsRoot="$(btrfs subvolume list / | grep ${snapsv} | grep root | wc -l)"
numSnapsHome="$(btrfs subvolume list / | grep ${snapsv} | grep home | wc -l)"
oldSnapRoot=${snapPath}$(sudo btrfs subvolume list / | grep ${snapsv} | grep root | awk '{print $NF}' | head -n1 | grep -Eo "\/.*") # Name of the oldest root snapshot
oldSnapHome=${snapPath}$(sudo btrfs subvolume list / | grep ${snapsv} | grep home | awk '{print $NF}' | head -n1 | grep -Eo "\/.*") # Name of the oldest home snapshot

btrfs subvolume snapshot $rootPath ${snapPath}/root-$today
btrfs subvolume snapshot $homePath ${snapPath}/home-$today

if [ $numSnapsRoot -ge $sn  ]; then
    printf "${yellow}${oldSnapRoot} foi deletado!${NC}\n"
    btrfs subvolume delete $oldSnapRoot
fi
if [ $numSnapsHome -ge $sn  ]; then
    printf "${yellow}${oldSnapHome} foi deletado!${NC}\n"
    btrfs subvolume delete $oldSnapHome
fi
