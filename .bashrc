# -*- mode: sh; encoding: utf-8; -*-

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific aliases and functions
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
    has_gitdir="$(type __gitdir 2> /dev/null)"

    if [[ ! -z "$has_gitdir" ]]; then
        gitdir="$(__gitdir 2> /dev/null)"

        if [[ ! -z "$gitdir" ]]; then
            if [[ "$gitdir" == ".git" ]]; then
                gitdir="${PWD}/${gitdir}"
            fi

            if [[ "$gitdir" != "${HOME}/.git" ]]; then
                printf "%s" "$(__git_ps1 $@) "
            fi
        fi
    fi
}

if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
    export PS1='\[\033[1;32m\]\u@\h\[\033[0m\] \[\033[1;34m\]\W\[\033[0m\] $(__ahw_git_ps1 "[\[\033[1;35m\]%s\[\033[0m\]] ")\$ '
else
    export PS1='\u@\h \W $(__ahw_git_ps1 "[%s] ")\$ '
fi

# Add a local version of .bashrc so that stuff that shouldn't be
# checked in has a place to go.
if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi
