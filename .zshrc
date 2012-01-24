# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch
unsetopt beep notify
bindkey -e
# End of lines configured by zsh-newuser-install

. ~/.zsh_completion

autoload omz
zstyle :omz:style theme arch-blue
plugins=(archlinux sprunge git)
omz init

alias cls="echo -ne '\033c'"
alias ls="ls -h --color"
alias ll="ls -l"
alias lla="ll -a"

bindkey '\e[7~' beginning-of-line
bindkey '\e[8~' end-of-line
