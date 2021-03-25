# To get the latest emacs (ubuntu)
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot

# To install doom
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

# Other Dependencies
doom.d located in HOME directory
org located at ~/org
dotfiles located at ~/dotfiles (used for some private config files, in addition to private.el)

# Additional configuration
You may need to add some variables to private.el or generate your own private config files for some modules.
