#!/bin/sh

xsetroot -cursor_name left_ptr
amixer -c 0 sset "Auto-Mute Mode" Disabled
pulseaudio --start &
twmnd &
modprobe nvidia
xmonad
