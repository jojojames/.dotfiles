#!/bin/bash

if type "mbsync" > /dev/null; then
    echo "isync installed"
else
    echo "isync not installed. installing isync."
    brew install isync
fi

if type "msmtp" > /dev/null; then
    echo "msmtp installed"
else
    echo "msmtp not installed. installing msmtp."
    brew install msmtp
fi

if type "mu" > /dev/null; then
    echo "mu installed"
else
    brew install mu --with-emacs
    echo "mu not installed. installing mu."
    echo "If mu is not installed correctly, run this command.
    $ EMACS=/Applications/Emacs.app/Contents/MacOS/bin/emacs brew install mu --with-emacs --HEAD"
fi

if type "w3m" > /dev/null; then
    echo "w3m installed"
else
    brew install w3m
    echo "w3m not installed. installing w3m."
fi

if type "openssl" > /dev/null; then
    echo "openssl installed"
else
    echo "openssl not installed. installing openssl"
    brew install openssl
fi

echo "linking openssl --force"
brew unlink openssl && brew link openssl --force

if type "gpg" > /dev/null; then
    echo "gpg installed"
else
    echo "gpg not installed. installing gpg"
    brew install gpg
fi

if type "gpg2" > /dev/null; then
    echo "gpg2 installed"
else
    echo "gpg2 not installed. installing gpg2"
    brew install gpg2
fi

if type "pinentry-mac" > /dev/null; then
    echo "pinentry-mac installed"
else
    echo "pinentry-mac not installed. installing pinentry-mac"
    brew install pinentry-mac
fi

if type "gnutls-cli" > /dev/null; then
    echo "gnutls installed"
else
    echo "gnutls not installed. installing gnutls"
    brew install gnutls
fi

if [ ! -d $HOME/Mail ];
then
    echo "creating directory $HOME/Mail"
    mkdir $HOME/Mail
fi

if [ ! -d $HOME/Mail/gmail ];
then
    echo "creating directory $HOME/Mail"
    mkdir $HOME/Mail/gmail
fi

if [ ! -d $HOME/Mail/whoshere ];
then
    echo "creating directory $HOME/whoshere"
    mkdir $HOME/Mail/whoshere
fi

if [ ! -d $HOME/Mail/fastmail ];
then
    echo "creating directory $HOME/fastmail"
    mkdir $HOME/Mail/fastmail
fi

if [ ! -d $HOME/Mail/tableau ];
then
    echo "creating directory $HOME/tableau"
    mkdir $HOME/Mail/tableau
fi

# check for file
if [ -f $HOME/.authinfo.gpg ];
then
    echo "moving $HOME/.authinfo.gpg to $HOME/.authinfo.gpg_backup"
    mv $HOME/.authinfo.gpg $HOME/.authinfo.gpg_backup
fi

# check for symlink
if [ -L $HOME/.authinfo.gpg ];
then
    echo "removing symlink $HOME/.authinfo.gpg"
    rm $HOME/.authinfo.gpg
fi

echo "linking $HOME/.emacs.d/.email.gpg to $HOME/.authinfo.gpg "
ln -s $HOME/.emacs.d/mail/.email.gpg $HOME/.authinfo.gpg

if [ -f $HOME/.gnupg/gpg-agent.conf ];
then
    echo "moving $HOME/.gnupg/gpg-agent.conf to $HOME/.gnupg/gpg-agent.conf_backup"
    mv $HOME/.gnupg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf_backup 
fi

if [ -L $HOME/.gnupg/gpg-agent.conf ];
then
    echo "removing symlink $HOME/.gnupg/gpg-agent.conf"
    rm $HOME/.gnupg/gpg-agent.conf
fi

echo "linking $HOME/.emacs.d/.gpg-agent.conf to $HOME/.gnupg/gpg-agent.conf "
mkdir -p $HOME/.gnupg; ln -s $HOME/.emacs.d/mail/.gpg-agent.conf $HOME/.gnupg/gpg-agent.conf

if [ -f $HOME/.mbsyncrc ];
then
    echo "moving $HOME/.mbsyncrc to $HOME/.mbsyncrc_backup"
    mv $HOME/.mbsyncrc $HOME/.mbsyncrc_backup
fi

if [ -L $HOME/.mbsyncrc ];
then
    echo "removing symlink $HOME/.mbsyncrc"
    rm $HOME/.mbsyncrc
fi

echo "linking $HOME/.emacs.d/.mbsyncrc to $HOME/.mbsyncrc "
ln -s $HOME/.emacs.d/mail/.mbsyncrc $HOME/.mbsyncrc

if [ -f $HOME/.msmtprc ];
then
    echo "moving $HOME/.msmtprc to $HOME/.mbsyncrc_backup"
    mv $HOME/.msmtprc $HOME/.msmtprc_backup
fi

if [ -L $HOME/.msmtprc ];
then
    echo "removing symlink $HOME/.msmtprc"
    rm $HOME/.msmtprc
fi

echo "linking $HOME/.emacs.d/mail/.msmtprc to $HOME/.msmtprc"
ln -s $HOME/.emacs.d/mail/.msmtprc $HOME/.msmtprc

echo "run $ mbsync gmail"
echo "run $ mu index --maildir=~/Mail"
echo "restart computer to check the launch agent in ~/Library/LaunchAgents or run another terminal to get the gpg-agent running."
