# -*- mode: sh; encoding: utf-8; -*-

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
    export PS1='\[\033[1;32m\]\u@\h\[\033[0m\] \[\033[1;34m\]\W\[\033[0m\] \$ '
else
    export PS1='\u@\h \W $(__ahw_git_ps1 "[%s] ")\$ '
fi

# Add a local version of .bashrc so that stuff that shouldn't be
# checked in has a place to go.
if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi
