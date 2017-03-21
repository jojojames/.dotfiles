#/bin/sh
# Remove this script when this is resolved.
# For now, can set up anaconda-mode using this script.
# https://github.com/proofit404/anaconda-mode/issues/225

cd $HOME/Code
git clone https://github.com/davidhalter/jedi
cd jedi
git checkout v0.10.0
python setup.py sdist
cd dist
PYTHONPATH=$HOME/.emacs.d/anaconda-mode/0.1.7 easy_install -d $HOME/.emacs.d/anaconda-mode/0.1.7 -S $HOME/.emacs.d/anaconda-mode/0.1.7 -a -Z jedi-0.10.0.tar.gz
