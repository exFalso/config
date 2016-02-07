#!/bin/sh

# autocutsel -fork &
# autocutsel -selection PRIMARY -fork &

setxkbmap gb
xsetroot -cursor_name left_ptr
amixer -c 0 sset "Auto-Mute Mode" Disabled
pulseaudio --start &
modprobe nvidia
xmonad
