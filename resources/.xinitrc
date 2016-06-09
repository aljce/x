#!/bin/sh
# etc .xinitrc n/a 04/22/2016 ix <arcetera@openmailbox.org>

setroot -t ~/.wall.png
xrdb < ~/.Xdefaults
xset +fp $HOME/.fonts fp rehash
xmodmap ~/.xmodmaprc
urxvtd -q -o -f
xbanish &
sxhkd -c ~/.sxhkdrc &
xidle -timeout 600 -ne -program /usr/local/bin/slock &
compton &
rrun &
#exec cwm
#exec dwm
exec xmonad
