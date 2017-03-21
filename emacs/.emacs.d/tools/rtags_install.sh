#/bin/bash

# Update macports.
sudo port selfupdate

# Install clang from macports.
sudo port install clang-3.9
# james at jamesretina in ~ port installed requested
# The following ports are currently installed:
#   clang-3.9 @3.9-r266579_0+analyzer+assertions+debug
#   clang-3.9 @3.9-r271348_0+analyzer+assertions+debug (active)

# Set up rtags
# https://github.com/Andersbakken/rtags
cd $HOME/.bin
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
cmake -DLIBCLANG_LLVM_CONFIG_EXECUTABLE=/opt/local/libexec/llvm-3.9/bin/llvm-config -DLIBCLANG_LIBDIR=/opt/local/libexec/llvm-3.9/lib -DLIBCLANG_CXXFLAGS="-I/opt/local/libexec/llvm-3.9/include -pipe -Os -std=c++11 -stdlib=libc++  -fPIC -fvisibility-inlines-hidden -Wall -W -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wmissing-field-initializers -pedantic -Wno-long-long -Wcovered-switch-default -Wnon-virtual-dtor -Wdelete-non-virtual-dtor -Werror=date-time -std=c++11 -g  -fno-exceptions -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS" -DLIBCLANG_LIBRARIES=/opt/local/libexec/llvm-3.9/lib/libclang.dylib .
make
