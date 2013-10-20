# .bashrc
# -*- mode: bash; indent-tabs-mode: t -*-

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# The git completion has moved.
if [ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]; then
    . /usr/share/git-core/contrib/completion/git-prompt.sh
fi

# User specific aliases and functions

alias head_files='git show --pretty="format:" --name-only HEAD | grep "\w" | tr '\''\n'\'' '\'','\'''
alias head_diff='cap production diff_file FILE=`head_files`'
alias head_push='cap production push_file FILE=`head_files` DETAILS="`git log --pretty=oneline -1`"'
alias head_commit_push_reaper='git add -A && git commit -m "tidy" && head_push && cap production reaper SLICE=all'

shopt -s globstar

function __ahw_git_ps1 {
	gitdir="$(__gitdir 2> /dev/null)"

	if [[ ! -z "$gitdir" ]]; then
		realpath_cmd="$(which realpath 2> /dev/null)"
		real_gitdir="$gitdir"
		if [[ ! -z "$realpath_cmd" ]]; then
			real_gitdir="$($realpath_cmd $gitdir)"
		fi

		if [[ "$real_gitdir" != "${HOME}/.git" ]]; then
			printf "%s" "$(__git_ps1 $@) "
		fi
	fi
}

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
