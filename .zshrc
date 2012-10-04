# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch
unsetopt beep notify
bindkey -e
# End of lines configured by zsh-newuser-install

. ~/.zsh_completion

#autoload omz
#zstyle :omz:style theme arch-blue
#plugins=(archlinux sprunge git)
#omz init

alias cls="echo -ne '\033c'"
alias ls="ls -h --color"
alias ll="ls -l"
alias lla="ll -a"
alias gbv="git branch -a -vv"

bindkey '\e[7~' beginning-of-line
bindkey '\e[8~' end-of-line

ZSH=$HOME/.oh-my-zsh

#ZSH_THEME="lambda"
ZSH_THEME="alanpeabody"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export KRAKEN=/home/vrobert/kraken
#export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl

SSH_AGENT_FILE=~/.ssh-agent
touch $SSH_AGENT_FILE      # there is probably a cleverer way...
chmod 700 $SSH_AGENT_FILE  # just to not eval bad things :)
if [ -n "`ps -e|grep ssh-agent`" ];
then
  eval `cat $SSH_AGENT_FILE`
else
  AGENT=`ssh-agent`
  eval $AGENT
  ssh-add
  echo $AGENT > $SSH_AGENT_FILE
fi
