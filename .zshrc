. ~/.common.rc.sh
. ~/.zsh_completion

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch
unsetopt beep notify
bindkey -e
# End of lines configured by zsh-newuser-install

bindkey '\e[7~' beginning-of-line
bindkey '\e[8~' end-of-line

ZSH=$HOME/.oh-my-zsh
#ZSH_THEME="alanpeabody"
#ZSH_THEME="agnoster"
ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_MODE="awesome-patched"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(longstatus time)
POWERLEVEL9K_DIR_BACKGROUND="red"
POWERLEVEL9K_LONGSTATUS_BACKGROUND="blue"
POWERLEVEL9K_TIME_BACKGROUND="white"
POWERLEVEL9K_TIME_FORMAT="%D{%d/%m/%y %H:%M:%S}"
plugins=(git)
source $ZSH/oh-my-zsh.sh

