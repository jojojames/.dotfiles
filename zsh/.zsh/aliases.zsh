# -------------------------------------------------------------------
# use nocorrect alias to prevent auto correct from "fixing" these
# -------------------------------------------------------------------
#alias foobar='nocorrect foobar'
# -------------------------------------------------------------------
# directory movement
# -------------------------------------------------------------------
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
# -------------------------------------------------------------------
# directory information
# -------------------------------------------------------------------

alias l='ls -al'
alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias lh='ls -d .*' # show hidden files/directories only

if [[ $IS_MAC -eq 1 ]]; then
    # Colorize output, add file type indicator, and put sizes in human readable format
    alias ls='ls -GFh'
    # directories sorted by size
    alias 'dus=du -sckx * | sort -nr'
fi

# -------------------------------------------------------------------
# Mac only
# -------------------------------------------------------------------
if [[ $IS_MAC -eq 1 ]]; then
    # rebuild Launch Services to remove duplicate entries on Open With menu
    alias rebuildopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.fram ework/Support/lsregister -kill -r -domain local -domain system -domain user'

    emacs() {
        # https://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx/26565655#26565655
        EMACSPATH=/Applications/Emacs.app/Contents/MacOS

        # Check if an emacs server is available
        # (by checking to see if it will evaluate a lisp statement)
        if ! (${EMACSPATH}/bin/emacsclient --eval "t"  2> /dev/null > /dev/null )
        then
            # There is no server available so,
            # Start Emacs.app detached from the terminal
            # and change Emac's directory to PWD
            # ------------------------------------------
            # Why use parens?
            # https://apple.stackexchange.com/questions/133087/how-to-prevent-the-application-launched-in-terminal-from-undesirably-exiting
            (nohup ${EMACSPATH}/Emacs --chdir "${PWD}" "${@}" 2>&1 > /dev/null &)
        else
            # The emacs server is available so use emacsclient
            if [ "$#" -eq  "0" ]
            then
                # There are no arguments, so
                # tell emacs to open a new window
                # ${EMACSPATH}/bin/emacsclient --eval "(list-directory \"${PWD}\")"
                ${EMACSPATH}/bin/emacsclient --eval "(dired \"${PWD}\")"
            else
                # There are arguments, so
                # tell emacs to open them
                ${EMACSPATH}/bin/emacsclient --no-wait "${@}"
            fi

            # Bring emacs to the foreground
            ${EMACSPATH}/bin/emacsclient --eval "(x-focus-frame nil)"
        fi
    }
fi

alias e=emacs

# -------------------------------------------------------------------
# me
# -------------------------------------------------------------------
# -2 for color, || to attach to a current session or create a new session
alias tm='tmux -2 attach || tmux -2 new'
# force tmux to use 256 colors
alias tmux='TERM=screen-256color-bce tmux'

if [[ $IS_MAC -eq 1 ]]; then
    # vim with no plugins
    alias vi='vim -u NONE'
fi

alias jformat='python -m json.tool'
# -------------------------------------------------------------------
# remote machines
# -------------------------------------------------------------------
alias seedbox='ssh jojojames@genesis.whatbox.ca'
alias syno='ssh -p 4000 james@jojobebe.noip.me'
# -------------------------------------------------------------------
# Git
# -------------------------------------------------------------------
alias ga='git add'
alias gp='git push'
alias gl='git log'
alias gpl="git log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gs='git status'
alias gd='git diff'
alias gm='git commit -m'
alias gma='git commit -am'
alias gb='git branch'
alias gc='git checkout'
alias gcb='git checkout -b'
alias gra='git remote add'
alias grr='git remote rm'
alias gpu='git pull'
alias gcl='git clone'
alias gta='git tag -a -m'
alias gf='git reflog'
alias gv='git log --pretty=format:'%s' | cut -d " " -f 1 | sort | uniq -c | sort -nr'

# leverage aliases from ~/.gitconfig
alias gh='git hist'
alias gt='git today'

# curiosities
# gsh shows the number of commits for the current repos for all developers
alias gsh="git shortlog | grep -E '^[ ]+\w+' | wc -l"

# gu shows a list of all developers and the number of commits they've made
alias gu="git shortlog | grep -E '^[^ ]'"

# -------------------------------------------------------------------
# WH
# -------------------------------------------------------------------
alias redis_start='${EM_REDIS_HOME}/redis-server.sh start'

# -------------------------------------------------------------------
# TB
# -------------------------------------------------------------------
function adbreset() {
    adb kill-server
    adb start-server
    adb devices
}
