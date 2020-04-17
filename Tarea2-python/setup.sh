#!/bin/sh

replaceDir="$(pwd | sed "s#/#\\\/#g")\/venv"
targetDir=$(pwd)/venv/bin

for f in activate activate.csh activate.fish
do
    sed -i "s/insert_dir/$replaceDir/g" $targetDir/$f
done

replaceDir="${replaceDir}\/bin\/python3"

for f in easy_install easy_install-3.7 pip pip3 pip3.7
do
    sed -i "s/insert_dir/$replaceDir/g" $targetDir/$f
done
