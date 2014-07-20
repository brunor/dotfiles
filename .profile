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

# have the terminal display git branch in color
function git-branch-name {
  git symbolic-ref HEAD 2>/dev/null | cut -d"/" -f 3
}
function git-branch-prompt {
  local branch=`git-branch-name`
  if [ $branch ]; then printf " [%s]" $branch; fi
}
#PS1="\u@\h \[\033[0;36m\]\W\[\033[0m\]\[\033[0;32m\]\$(git-branch-prompt)\[\033[0m\] \$ "
PS1="\u@\h \[\033[0;36m\]\W\[\033[0m\]\[\033[0;32m\]\[\033[0m\] \$ "

# set python virtualenvwrapper vars
export WORKON_HOME=~/.virtualenvs
if [ -f "/usr/local/bin/virtualenvwrapper.sh" ]; then
. /usr/local/bin/virtualenvwrapper.sh
fi

# set golang root and path
export GOROOT=/usr/local/go
export GOPATH=$HOME/repos/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export PATH=$HOME/.cabal/bin:$PATH



##-------------------------------##
#     fillbucket                 #
# creates new repo in bitbucket  #
# $1 = username                  #
# $2 = password                  #
# $3 = reponame                  #
# Makes a non git directory to a new private bitbucket repository with all files added to initial commit
# Usage = fillbucket <username> <password> <reponame>
# function fillbucket () {
#     if [ -z "$1" ]; then
#         echo "username:"
#         read NAME
#     else
#         NAME=$1
#     fi

#     if [ -z "$2" ]; then
#         echo "password:"
#         read PASSWORD
#     else
#         PASSWORD=$1
#     fi

#     if [ -z "$3" ]; then
#         echo "reponame:"
#         read REPONAME
#     else
#         REPONAME=$1
#     fi


#     #git init
#     #git add -A .
#     #git commit -m "Initial commit"
#     curl --user $NAME:$PASSWORD https://api.bitbucket.org/1.0/repositories/ --data name=$REPONAME --data is_private='true'
#     git remote add origin https://$NAME@bitbucket.org/$NAME/$REPONAME.git
#     git push -u origin --all
#     git push -u origin --tags
# }
