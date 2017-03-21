#########################################
# gpg security
#########################################
# combo of these two links
# https://www.emacswiki.org/emacs/Gmail,_Gnus_and_GPG_on_a_Mac
# https://blog.chendry.org/2015/03/13/starting-gpg-agent-in-osx.html
[ -f ~/.gpg-agent-info ] && source ~/.gpg-agent-info
if [ -S "${GPG_AGENT_INFO%%:*}" ]; then
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
else
    if type "gpg-agent" > /dev/null; then
        eval $( gpg-agent --daemon --write-env-file ~/.gpg-agent-info )
    fi
fi

GPG_TTY=$(tty)
export GPG_TTY
