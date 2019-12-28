export TERM="xterm-256color"

. ~/.zsh_completion

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch
unsetopt beep notify
bindkey -e
# End of lines configured by zsh-newuser-install
typeset -g -A key

bindkey -e

# Linux keybindings
bindkey '^[[3~'   delete-char
bindkey '^[[1;3D' backward-word
bindkey '^[[1;3C' forward-word
bindkey '^[[1~'   beginning-of-line
bindkey '^[[4~'   end-of-line
bindkey '\e[7~' beginning-of-line
bindkey '\e[8~' end-of-line

# MacOS keybindings
bindkey '^[^[[A' beginning-of-line
bindkey '^[^[[B' end-of-line
bindkey '^[^[[C' forward-word
bindkey '^[^[[D' backward-word

#source $HOME/awesome-terminal-fonts/build/devicons-regular.sh
#source $HOME/awesome-terminal-fonts/build/fontawesome-regular.sh
#source $HOME/awesome-terminal-fonts/build/octicons-regular.sh
#source $HOME/awesome-terminal-fonts/build/pomicons-regular.sh

ZSH=$HOME/.oh-my-zsh
#ZSH_THEME="alanpeabody"
#ZSH_THEME="agnoster"
POWERLEVEL9K_MODE="awesome-fontconfig"
ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon dir vcs custom_nixshell)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status time)
DIRCOLOR="007"
POWERLEVEL9K_DIR_HOME_BACKGROUND=$DIRCOLOR
POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND=$DIRCOLOR
POWERLEVEL9K_DIR_DEFAULT_BACKGROUND=$DIRCOLOR
POWERLEVEL9K_CUSTOM_NIXSHELL="echo ${name}"
POWERLEVEL9K_CUSTOM_NIXSHELL_BACKGROUND="006"
POWERLEVEL9K_TIME_BACKGROUND="010"
POWERLEVEL9K_TIME_FORMAT="%D{%H:%M:%S}"
plugins=(git nix-shell)
source $ZSH/oh-my-zsh.sh

# Do not move next line further up, needs to be late
. ~/.common.rc.sh

# Set up direnv
eval "$(direnv hook zsh)"
