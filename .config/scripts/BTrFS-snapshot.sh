#!/usr/bin/env bash
# Created by Thwy. Inspired by https://github.com/linuxdabbler/personal-dot-files/blob/master/scripts/snaphome
# DON'T MAKE 2 SNAPSHOTS OF THE SAME SUBVOLUME IN LESS THAN 1MIN. You'll break the script
# Set the variables "rootsv rootPath homesv homePath snapsv snapPath" before running the script

if [ $(whoami) != 'root' ]; then
    printf "This script must be run as root (BTrFS limitation, sorry)\n" && exit 1
fi

# Number of snapshots allowed
sn=5
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

# Functions
function checkIfNumber {
    local NUM=$1
    if [[ $NUM =~ ^[0-9]+$ ]]; then
	return 0
    else
	return 1
    fi
}
function selectOptions {
    # Função retorna número da opção em $OPTION e o texto da opção em $S_OPTION
    local OPTIONS=($@)
    local i=0
    while [ $i -lt ${#OPTIONS[@]} ]; do
	echo "$((( ${i} + 1 ))). ${OPTIONS[$i]}"
	i=$((( $i+1 )))
    done
    read -p "Número da opção > " OPTION
    checkIfNumber $OPTION
    if [ $? = 0 ]; then
	if [ $OPTION -le $i ] && [ $OPTION -ge 1 ]; then
	    S_OPTION=${OPTIONS[$((( $OPTION - 1 )))]}
	    #Retornar "-1" indicando sucesso
	    i="-1"
	fi
    fi
    if [ $? = 1 ] || [ $i != "-1" ]; then
	echo -e "\n${RED}Insira uma opção válida${NC}"
	selectOptions $@
    fi
}

#Actual code
echo "Select a subvolume to snapshot"
selectOptions "/root" "/home"
case $OPTION in
    1)
	selsv=$rootsv
	snapname=root
	path=$rootPath
	;;
    2)
	selsv=$homesv
	snapname=home
	path=$homePath
	;;
    *)
	echo "invalid option"
	exit
	;;
esac
numSnaps="$(btrfs subvolume list / | grep ${snapsv} | grep ${snapname} | wc -l)"
oldSnap=${snapPath}$(sudo btrfs subvolume list / | grep ${snapsv} | grep ${snapname} | awk '{print $NF}' | head -n1 | grep -Eo "\/.*") # Print path to the old snapshot

btrfs subvolume snapshot $path ${snapPath}/${snapname}-$today

if [ $numSnaps -ge $sn  ]; then
    printf "${yellow}Would you like to remove the oldest snapshot? (${oldSnap})${nc}\n"
    read -p "Type \"yes\" for yes or anything else for no > " reallyrm
    if [ "$reallyrm" = "yes" ]; then
	btrfs subvolume delete $oldSnap
	exit
    fi
fi
