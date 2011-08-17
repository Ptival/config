#!/bin/sh

# Numlock ON
numlockx &

# Set Caps Lock to Left Control
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

# Cursor is an arrow
xsetroot -cursor_name left_ptr

exec xmonad
