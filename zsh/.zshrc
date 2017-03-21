#########################################
# me
#########################################
# http://zanshin.net/2013/02/02/zsh-configuration-from-the-ground-up/
source ~/.zsh/bindkeys.zsh
source ~/.zsh/checks.zsh
source ~/.zsh/colors.zsh
source ~/.zsh/setopts.zsh
source ~/.zsh/exports.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/completion.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/functions.zsh
source ~/.zsh/history.zsh

if [[ $IS_MAC -eq 1 ]]; then
    # Assume Linux handles gpg on its own well enough.
    source ~/.zsh/gpg.zsh
fi
