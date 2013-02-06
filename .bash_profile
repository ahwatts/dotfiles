# .bash_profile

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

export PATH=$PATH:$HOME/.local/bin:$HOME/bin

# Add a local version of .bash_profile so that stuff that shouldn't be
# checked in has a place to go.
if [ -f ~/.bash_profile_local ]; then
	. ~/.bash_profile_local
fi

if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
    export PS1='\[\033[1;32m\]\u@\h\[\033[0m\] \[\033[1;34m\]\W\[\033[0m\] $(rvm-prompt "[\[\033[1;36m\]" i v p g "\[\033[0m\]] " 2> /dev/null)$(__ahw_git_ps1 "[\[\033[1;35m\]%s\[\033[0m\]] ")\$ '
else
    export PS1='\u@\h \W $(rvm-prompt "[" i v p g "] ")$(__ahw_git_ps1 "[%s] ")\$ '
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
