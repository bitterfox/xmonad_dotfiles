#!/bin/bash

DOT_FILES=(.xmobarrc .xmonad .wishes.xml .comptonrc .Xresources .config/dunst/dunstrc)

DISCARD_FILES= # 現状なし

# 不要なファイルをバックアップして捨てる
for discard in ${DISCARD_FILES[@]}
do
   if [ -e $HOME/$discard ]; then
       mv -f $HOME/$discard $HOME/${discard}.old.`date +%y%m%d`
   fi
done

# シンボリックリンクをはる
mkdir -p .config/dunst
for file in ${DOT_FILES[@]}
do
    rm -rf $HOME/$file
    ln -s `pwd`/$file $HOME/$file
done

cat >> $HOME/.profile <<EOF
if [ -f "$$HOME/.xmonad/xmonad.state" ] ; then
    rm "$$HOME/.xmonad/xmonad.state"
fi
EOF
