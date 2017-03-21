# To see the key combo you want to use just do:
# cat > /dev/null
# And press it

# Examples
# bindkey "^K"      kill-whole-line                      # ctrl-k
# bindkey "^R"      history-incremental-search-backward  # ctrl-r
# bindkey "^A"      beginning-of-line                    # ctrl-a  
# bindkey "^E"      end-of-line                          # ctrl-e
# bindkey "[B"      history-search-forward               # down arrow
# bindkey "[A"      history-search-backward              # up arrow
# bindkey "^D"      delete-char                          # ctrl-d
# bindkey "^F"      forward-char                         # ctrl-f
# bindkey "^B"      backward-char                        # ctrl-b
# bindkey -v   # default to standard vi bindings, regardless of editor string

###################################
# me
###################################
bindkey -e # use emacs bindings
###################################
# oh-my-zsh
###################################

# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init() {
        echoti smkx
    }
    function zle-line-finish() {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

bindkey ' ' magic-space                               # [Space] - do history expansion
