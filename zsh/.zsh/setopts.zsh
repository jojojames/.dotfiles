# ===== Basics
# uncomment to not beep on error
# setopt no_beep

# Allow comments even in interactive shells
setopt interactive_comments

# ===== Changing Directories
# If you type foo, and it isn't a command, and it is a directory in your cdpath, go there
setopt auto_cd
# if argument to cd is the name of a parameter whose value is a valid directory, it will become the current directory
setopt cdablevarS
# don't push multiple copies of the same directory onto the directory stack
setopt pushd_ignore_dups

# ===== Expansion and Globbing
# treat #, ~, and ^ as part of patterns for filename generation
setopt extended_glob

# ===== History
# Allow multiple terminal sessions to all append to one zsh command history
setopt append_history
# save timestamp of command and duration
setopt extended_history
# Add commands as they are typed, don't wait until shell exit
setopt inc_append_history
# when trimming history, lose oldest duplicates first
setopt hist_expire_dups_first
# Do not write events to history that are duplicates of previous events
setopt hist_ignore_dups
# remove command line from history list when first character on the line is a space
setopt hist_ignore_space
# When searching history don't display results already cycled through twice
setopt hist_find_no_dups
# Remove extra blanks from each command line being added to history
setopt hist_reduce_blanks
# don't execute, just expand history
setopt hist_verify
# imports new commands and appends typed commands to history
#setopt share_history

# ===== Completion 
# When completing from the middle of a word, move the cursor to the end of the word    
setopt always_to_end
# show completion menu on successive tab press. needs unsetop menu_complete to work
setopt auto_menu
# any parameter that is set to the absolute name of a directory immediately becomes a name for that directory
setopt auto_name_dirs
# Allow completion from within a word/phrase
setopt complete_in_word

# do not autoselect the first completion entry
unsetopt menu_complete

# ===== Correction
# spelling correction for commands
setopt correct
# spelling correction for arguments
# setopt correctall

# ===== Prompt
# Enable parameter expansion, command substitution, and arithmetic expansion in the prompt
setopt prompt_subst
# only show the rprompt on the current prompt
setopt transient_rprompt

# ===== Scripts and Functions
# perform implicit tees or cats when multiple redirections are attempted
setopt multios
