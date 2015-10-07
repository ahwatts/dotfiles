# -*- encoding: utf-8; mode: sh; --*-

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWUPSTREAM=auto

export EDITOR=emacsclient

export LC_COLLATE=C

export PATH=$HOME/.local/bin:$HOME/bin:/usr/local/sbin:$PATH

# Add a local version of .bash_profile so that stuff that shouldn't be
# checked in has a place to go.
if [ -f ~/.bash_profile_local ]; then
    . ~/.bash_profile_local
fi
