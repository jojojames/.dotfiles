#!/bin/bash

find /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/System/Library/Frameworks -name "*.h" | xargs ./yasobjc.rb -o ~/.emacs.d/snippets/objc-mode
