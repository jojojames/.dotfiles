# Setup terminal
export TERM=xterm-256color

# This resolves issues install the mysql, postgres, and other gems with native non universal binary extensions
export ARCHFLAGS='-arch x86_64'

export LESS='--ignore-case --raw-control-chars'
export PAGER='less'

# CTAGS Sorting in VIM/Emacs is better behaved with this in place
export LC_COLLATE=C 
# https://stackoverflow.com/questions/11287564/getting-sed-error-illegal-byte-sequence-in-bash
export LC_ALL=C

# Virtual Environment Stuff
if [[ -a /usr/libexec/java_home ]]; then
    #JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_71.jdk/Contents/Home/jre
    #export JAVA_HOME=`/usr/libexec/java_home -v1.6`
    #export JAVA_HOME=`/usr/libexec/java_home -v1.8`
    export JAVA_HOME=`/usr/libexec/java_home -v1.7`
fi

export EDITOR=/usr/local/bin/mvim
export XCODE_BUILD=$HOME/Library/Developer/Xcode/DerivedData

# PATHS
USER_BIN=$HOME/.bin
LOCAL_BIN=/usr/local/bin
SBIN_BIN=/usr/local/sbin
HEROKU=/usr/local/heroku/bin
OPT_BIN=/opt/local/bin
OPT_SBIN=/opt/local/sbin
XAMP=/Applications/xampp/xamppfiles/bin
CABAL=$HOME/.cabal/bin
REDIS=/usr/local/redis-2.8.7/bin
RVM_BIN=$HOME/.rvm/bin
CASK_ELISP_BIN=$HOME/.cask/bin
RTAGS=$HOME/.bin/rtags/bin

LLVM_BIN=/opt/local/libexec/llvm-3.9/bin
# /usr/local/opt/llvm/bin
export LIBCLANG_LIBDIR=/opt/local/libexec/llvm-3.9/lib

export OMNISHARP=$HOME/Developer/omnisharp-server/OmniSharp/bin/Debug/
export GOBIN=/usr/local/go/bin

##############
# BEGIN: WH RELATED
##############

# Is $ANDROID_HOME a path variable other Android tools expect?
export WH_ANDROID_HOME=/Applications/adt-bundle-mac-x86_64-20140702/sdk
WH_ANDROID_TOOLS=$WH_ANDROID_HOME/tools
WH_ANDROID_PLATFORM_TOOLS=$WH_ANDROID_HOME/platform-tools

export EMPROJECTROOT=$HOME/Developer/emroot
export EM_SOFTWARE=/usr/local
export EM_REPO=${EMPROJECTROOT}/EveryoneMessages

export EM_REDIS_HOME=${EMPROJECTROOT}/redis
export EM_REDIS_DATA=${EMPROJECTROOT}/redis/data

# GRADLE
export GRADLE_221=/usr/local/gradle-2.2.1
export GRADLE_HOME=${GRADLE_221}

# GRAILS
export GRAILS_211=/usr/local/grails-2.1.1
export GRAILS_221=/usr/local/grails-2.2.1
export GRAILS_HOME=${GRAILS_221}

CASSANDRA=/usr/local/Cassandra/apache-cassandra-2.0.7/bin

export JAVA_OPTS="-Xms1g -Xmx2g -XX:MaxPermSize=2g"
export WHPROJECTROOT=$HOME/Developer/whoshere

export MONOTOUCH_BIN=/Developer/MonoTouch/usr/bin
export MONODROID_BIN=/Developer/MonoAndroid/usr/bin

##############
# END: WH RELATED
##############

##############
# BEGIN: TB RELATED
##############

export TB_ANDROID_HOME=/Users/jnguyen/Library/Developer/Xamarin/android-sdk-macosx
TB_ANDROID_TOOLS=$TB_ANDROID_HOME/tools
TB_ANDROID_PLATFORM_TOOLS=$TB_ANDROID_HOME/platform-tools

##############
# END: TB RELATED
##############

# Add to $PATH variable.
MY_PATHS=$RTAGS:$USER_BIN:$RVM_BIN:$CASK_ELISP_BIN:$LOCAL_BIN:$CABAL
LANGUAGE_PATHS=$MONOTOUCH_BIN:$MONODROID_BIN
LANGUAGE_PATHS=$WH_ANDROID_TOOLS:$WH_ANDROID_PLATFORM_TOOLS:$LANGUAGE_PATHS
LANGUAGE_PATHS=$TB_ANDROID_TOOLS:$TB_ANDROID_PLATFORM_TOOLS:$LANGUAGE_PATHS
TOOL_PATHS=$LLVM_BIN:$GRAILS_HOME/bin:$GRADLE_HOME:$XAMP:$CASSANDRA:$REDIS
SYSTEM_PATHS=$OPT_BIN:$OPT_SBIN:$SBIN_BIN:$HEROKU

export PATH=$MY_PATHS:$LANGUAGE_PATHS:$TOOL_PATHS:$SYSTEM_PATHS:$PATH

if [[ $IS_LINUX -eq 1 ]]; then
    LOVE_PATH=$HOME/Code/love/src
    export PATH=$LOVE_PATH:$PATH
fi

# don't create additional files during tar
COPYFILE_DISABLE=true

# disable automatic renaming
export DISABLE_AUTO_TITLE="true"

# Homebrew Analytics Opt Out
# https://github.com/Homebrew/brew/blob/master/share/doc/homebrew/Analytics.md
export HOMEBREW_NO_ANALYTICS=1
