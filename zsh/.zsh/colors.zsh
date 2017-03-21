autoload colors; colors

# The variables are wrapped in \%\{\%\}. This should be the case for every
# variable that does not contain space.
for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
  eval PR_$COLOR='%{$fg_no_bold[${(L)COLOR}]%}'
  eval PR_BOLD_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done

eval RESET='$reset_color'
export PR_RED PR_GREEN PR_YELLOW PR_BLUE PR_WHITE PR_BLACK
export PR_BOLD_RED PR_BOLD_GREEN PR_BOLD_YELLOW PR_BOLD_BLUE 
export PR_BOLD_WHITE PR_BOLD_BLACK

# Clear LSCOLORS
unset LSCOLORS

# Main change, you can see directories on a dark background
#expor tLSCOLORS=gxfxcxdxbxegedabagacad

export CLICOLOR=1

# Enable color in grep
alias grep='grep --color=auto'
export GREP_COLOR='3;33'

if [[ $IS_LINUX -eq 1 ]]; then
    # This was taken from Xubuntu's .bashrc.
    # Enable color support of ls and also add handy aliases.
    if [ -x /usr/bin/dircolors ]; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        alias ls='ls --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    fi
fi

if [[ $IS_MAC -eq 1 ]]; then
    export LSCOLORS=Gxfxcxdxbxegedabagacad
    export LS_COLORS=exfxcxdxbxegedabagacad
fi
