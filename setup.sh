#!/bin/bash

DOT_FILES=(.xmonad .xmobar .xmobarrc .wishes.xml .comptonrc .Xresources .config/dunst/dunstrc config.py)

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
    mkdir -p `dirname $HOME/$file`
    rm -rf $HOME/$file
    ln -s `pwd`/$file $HOME/$file
done

cat >> $HOME/.profile <<EOF
if [ -f "\$HOME/.xmonad/xmonad.state" ] ; then
    rm "\$HOME/.xmonad/xmonad.state"
fi
EOF

sudo cp `pwd`/xorg.conf/* /usr/share/X11/xorg.conf.d/

sudo cp `pwd`/.config/libinput-gestures.conf /etc/

sudo cp `pwd`/polkit/xmonad.policy /usr/share/polkit-1/actions/
sudo sed -i -e "s#\$HOME#$HOME#g" /usr/share/polkit-1/actions/xmonad.policy
