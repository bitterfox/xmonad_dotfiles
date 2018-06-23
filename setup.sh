#!/bin/bash

DOT_FILES=(.xmobarrc .xmonad .wishes.xml .comptonrc)

DISCARD_FILES= # 現状なし

# 不要なファイルをバックアップして捨てる
for discard in ${DISCARD_FILES[@]}
do
   if [ -e $HOME/$discard ]; then
       mv -f $HOME/$discard $HOME/${discard}.old.`date +%y%m%d`
   fi
done

# シンボリックリンクをはる
for file in ${DOT_FILES[@]}
do
    rm -rf $HOME/$file
    ln -s `pwd`/$file $HOME/$file
done

