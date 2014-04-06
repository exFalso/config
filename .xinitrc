#!/bin/sh

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

setxkbmap gb
xsetroot -cursor_name left_ptr
xmonad
