#!/bin/bash 
clear
cond=$1
dir="dist-newstyle"
if [[ -d $dir ]]; then
    if [ "$cond" = "remove" ]; then
        rm -rf $dir
        echo "Current date and time: $(date)"
        echo "Removed old build at /"$dir
    fi
fi
cabal run