# -*- mode: sh; -*-

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
alias head_push='cap production push_file FILE=`head_files` DETAILS="`git log --abbrev-commit --pretty=oneline -1`"'
alias head_commit_push_reaper='git add -A && git commit -m "tidy" && head_push && cap production reaper SLICE=all'

alias au_files='git ls-files -v | grep '\''^[a-z]'\'' | awk '\''{ print $2; }'\'''
alias mod_files='git status -s | grep '\''^ *M'\'' | awk '\''{ print $2; }'\'''
alias mod_files_comma='echo `mod_files` | tr '\'' '\'' '\'','\'''
alias stash_au_files='git update-index --no-assume-unchanged `au_files` && git stash'
alias unstash_au_files='git stash pop && git update-index --assume-unchanged `mod_files`'
alias dockviz='docker run --rm -e DOCKER_HOST nate/dockviz'

function es_indices {
    curl -s "${@}/_cat/indices?v" | (read h; printf "%s\n" "$h"; sort -bk 2)
}

function es_aliases {
    curl -s "${@}/_cat/aliases?v" | (read h; printf "%s\n" "$h"; sort -bk 2)
}

if [[ "${BASH_VERSION}" > "3.9999" ]]; then
    shopt -s globstar
fi

function __ahw_git_ps1 {
    gitdir="$(__gitdir 2> /dev/null)"

    if [[ ! -z "$gitdir" ]]; then
        if [[ "$gitdir" == ".git" ]]; then
            gitdir="${PWD}/${gitdir}"
        fi

        if [[ "$gitdir" != "${HOME}/.git" ]]; then
            printf "%s" "$(__git_ps1 $@) "
        fi
    fi
}

if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
    export PS1='\[\033[1;32m\]\u@\h\[\033[0m\] \[\033[1;34m\]\W\[\033[0m\] $(__ahw_git_ps1 "[\[\033[1;35m\]%s\[\033[0m\]] ")\$ '
else
    export PS1='\u@\h \W $(__ahw_git_ps1 "[%s] ")\$ '
fi
