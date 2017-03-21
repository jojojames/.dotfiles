#!/bin/bash
HEADERS_PATH=$(xcode-select -print-path)/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/System/Library/Frameworks
find ${HEADERS_PATH} -name "*.h" | xargs ruby $HOME/.emacs.d/fork/motion-mode/bin/make_dict.rb
cp $(pwd)/motion-mode ~/.emacs.d/ac-dict/
echo "copied $(pwd)/motion-mode to ~/.emacs.d/ac-dict"
