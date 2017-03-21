# Mac OS X uses path_helper and /etc/paths.d to preload PATH, clear it out first
# if [ -x /usr/libexec/path_helper ]; then
#     PATH=''
#     eval `/usr/libexec/path_helper -s`
# fi

# UTF-8 in Emacs
# https://superuser.com/questions/538357/utf-8-characters-in-emacs-only-in-term-ansi-term-multi-term-do-not-appear
export LANG='en_US.UTF-8'
export LC_ALL="en_US.UTF-8"
