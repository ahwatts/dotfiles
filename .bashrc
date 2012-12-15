# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

alias head_files='git show --pretty="format:" --name-only HEAD | grep "\w" | tr '\''\n'\'' '\'','\'''
alias head_diff='cap production diff_file FILE=`head_files`'
alias head_push='cap production push_file FILE=`head_files` DETAILS="`git log --pretty=oneline -1`"'
# alias head_commit_push_reaper='git add -A && git commit -m "tidy" &&
# head_push && cap production reaper SLICE=all'

function __ahw_git_ps1 {
    gitdir="$(__gitdir)"
    if [[ ! -z "$gitdir" && "$(realpath $gitdir)" != "${HOME}/.git" ]]; then 
       printf "%s" "$(__git_ps1 $@) "
    fi
}

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
