# Use my aliases
. ~/.common.rc.sh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '
