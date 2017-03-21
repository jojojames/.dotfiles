#/bin/bash
install_name_tool -change @rpath/libclang.dylib /opt/local/libexec/llvm-3.9/lib/libclang.dylib ~/.emacs.d/irony/bin/irony-server
