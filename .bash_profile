# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi


# set python virtualenvwrapper vars
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/repos
if [ -f "/usr/local/bin/virtualenvwrapper.sh" ]; then
   . "/usr/local/bin/virtualenvwrapper.sh"
fi

# set golang root and path
export GOROOT=/usr/local/go
export GOPATH=$HOME/repos/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export PATH=$HOME/.cabal/bin:$PATH
