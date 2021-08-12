# To get the latest emacs (ubuntu)
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot

# To install doom
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

# Other Dependencies
doom.d located in HOME directory
org located at ~/org
dotfiles located at ~/dotfiles (used for some private config files, in addition to private.el)

# Windows
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#on-windows

Download and install Git from https://git-scm.com/download/win
Download and extract Emacs, ripgrep and fd where you want them, but in different folders:

    Emacs 27.2 from http://ftp.wayne.edu/gnu/emacs/windows/emacs-27/
    Ripgrep from https://github.com/BurntSushi/ripgrep/releases
    (optional) fd from https://github.com/sharkdp/fd/releases

Add the three folders from step 2 to your PATH
    1. Go to Control panel -> User Accounts -> Change my environment variables.
    2. Click “New”, type HOME and set your C:\Users\USERNAME and OK.
    3. Select “Path”, click “edit”, prepend C:\path\to\the\emacs\bin: to it and click OK.
    4. Select “Path”, click “edit”, prepend C:\path\to\the\ripgrep: to it and click OK.
    5. Select “Path”, click “edit”, prepend C:\path\to\the\fd: to it and click OK.
    6. Click Ok.


N.B. org-roam requires sqlite3. https://sqlite.org/download.html
(org-roam config is not portable yet, comment it out in init.el!)

# Additional configuration
You may need to add some variables to private.el or generate your own private config files for some modules.
