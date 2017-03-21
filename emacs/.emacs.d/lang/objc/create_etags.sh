find . -name '*.[hm]' | xargs etags
find /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/System/Library/Frameworks -name '*.[h]' | xargs etags -a
