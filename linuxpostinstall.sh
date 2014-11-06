#!/bin/bash
# -*- Mode: sh; coding: utf-8; indent-tabs-mode: nil; tab-width: 4 -*-
#
# Authors:
#   Sam Hewitt <hewittsamuel@gmail.com>
#
# Description:
#   A post-installation bash script for Ubuntu (14.04)
#
# Legal Stuff:
#
# This script is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; version 3.
#
# This script is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <https://www.gnu.org/licenses/gpl-3.0.txt>

echo ''
echo '#-------------------------------------------#'
echo '#     Ubuntu 14.04 Post-Install Script      #'
echo '#-------------------------------------------#'

#----- FUNCTIONS -----#

# SYSTEM UPGRADE
function sysupgrade {
# Perform system upgrade
echo ''
read -p 'Proceed with system upgrade? (Y)es, (N)o : ' REPLY
case $REPLY in
# Positive action
[Yy]* )
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Dist-Upgrade
    echo 'Performing system upgrade...'
    sudo apt-get dist-upgrade -y
    echo 'Done.'
    main
    ;;
# Negative action
[Nn]* )
    clear && main
    ;;
# Error
* )
    clear && echo 'Sorry, try again.'
    sysupgrade
    ;;
esac
}

# INSTALL APPLICATIONS
function favorites {
# Install Favourite Applications
echo ''
echo 'Installing selected favorite applications...'
echo ''
echo 'Current package list:
nautilus-dropbox
vlc
mplayer
unity-tweak-tool
ubuntu-wallpapers*
flashplugin-downloader
xchat
ttf-mscorefonts-installer
xclip
zenity
indicator-multiload'
echo ''
read -p 'Proceed? (Y)es, (N)o : ' REPLY
case $REPLY in
# Positive action
[Yy]* )
    echo 'Requires root privileges:'
    # Feel free to change to whatever suits your preferences.
    sudo apt-get install -y --no-install-recommends nautilus-dropbox vlc mplayer unity-tweak-tool ubuntu-wallpapers* flashplugin-downloader xchat ttf-mscorefonts-installer xclip zenity indicator-multiload
    echo 'Done.'
    main
    ;;
# Negative action
[Nn]* )
    clear && main
    ;;
# Error
* )
    clear && echo 'Sorry, try again.'
    favorites
    ;;
esac
}

# INSTALL LAPTOP BATTERY TOOLS
function laptop {
echo ''
echo 'Installing selected laptop battery tools...'
echo ''
echo 'Current package list:
smartmontools
tlp
tlp-rdw
ethtool
tp-smapi-dkms
acpi-call-tools'
echo ''
read -p 'Proceed? (Y)es, (N)o : ' REPLY
case $REPLY in
# Positive action
[Yy]* )
    echo 'Requires root privileges:'
    # Add repository
    echo 'Adding LPT repository to sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:linrunner/tlp
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Install package(s)
    echo 'Installing LPT tools...'
    echo 'Requires root privileges:'
    sudo apt-get install -y smartmontools tlp tlp-rdw ethtool tp-smapi-dkms acpi-call-tools
    echo 'Done.'
    main
    ;;
# Negative action
[Nn]* )
    clear && main
    ;;
# Error
* )
    clear && echo 'Sorry, try again.'
    laptop
    ;;
esac
}

# INSTALL SYSTEM TOOLS
function system {
echo ''
echo '1. Install favorite system utilities?'
echo '2. Install favorite network utilities?'
echo '3. Install favorite security utilities?'
echo 'r. Return.'
echo ''
read -p 'What would you like to do? (Enter your choice) : ' REPLY
case $REPLY in
# Install Favorite System utilities
1)
    echo 'Installing favorite system utilities...'
    echo ''
    echo 'Current package list:
    openssh-server
    ppa-purge
    ssh
    virt-manager
    zsync
    terminator
    lsb-core
    gparted
    lm-sensors
    compizconfig-settings-manager
    hardinfo
    libnotify-bin
    tree
    ccze
    unace unrar zip unzip p7zip-full p7zip-rar sharutils rar uudeview mpack arj cabextract
    file-roller
    bleachbit'
    echo ''
    read -p 'Proceed? (Y)es, (N)o : ' REPLY
    case $REPLY in
    # Positive action
    [Yy]* )
        echo 'Requires root privileges:'
        # Feel free to change to whatever suits your preferences.
        sudo apt-get install -y openssh-server ppa-purge ssh virt-manager zsync terminator lsb-core gparted lm-sensors compizconfig-settings-manager hardinfo libnotify-bin most tree ccze unace unrar zip unzip p7zip-full p7zip-rar sharutils rar uudeview mpack arj cabextract file-roller bleachbit
        echo 'Done.'
        system
        ;;
    # Negative action
    [Nn]* )
        clear && system
        ;;
    # Error
    * )
        echo 'Sorry, try again.'
        system
        ;;
    esac
    ;;
# Install Network Utils
2)
    echo 'Installing favorite network utilities...'
    echo ''
    echo 'Current package list:
    iftop
    ifstat
    iptraf
    wireshark
    tshark
    arp-scan
    htop
    netspeed
    nmap
    netpipe-tcp'
    echo ''
    read -p 'Proceed? (Y)es, (N)o : ' REPLY
    case $REPLY in
    # Positive action
    [Yy]* )
        echo 'Requires root privileges:'
        # Feel free to change to whatever suits your preferences.
        sudo apt-get install -y --no-install-recommends iftop ifstat iptraf wireshark tshark arp-scan htop netspeed nmap netpipe-tcp
        echo 'Done.'
        clear && system
        ;;
    # Negative action
    [Nn]* )
        clear && system
        ;;
    # Error
    * )
        clear && echo 'Sorry, try again.'
        system
        ;;
    esac
    ;;
# Install Security Utils
3)
    echo 'Installing favorite security utilities...'
    echo ''
    echo 'Current package list:
    gufw
    rkhunter
    chkrootkit
    fail2ban'
    echo ''
    read -p 'Proceed? (Y)es, (N)o : ' REPLY
    case $REPLY in
    # Positive action
    [Yy]* )
        echo 'Requires root privileges:'
        # Feel free to change to whatever suits your preferences.
        sudo apt-get install -y gufw rkhunter chkrootkit fail2ban
        echo 'Done.'
        clear && system
        ;;
    # Negative action
    [Nn]* )
        clear && system
        ;;
    # Error
    * )
        clear && echo 'Sorry, try again.'
        system
        ;;
    esac
    ;;
# Return
[Rr]*)
    clear && main;;
# Invalid choice
* )
    clear && echo 'Not an option, try again.' && system;;
esac
}




# INSTALL UBUNTU RESTRICTED EXTRAS
function codecinstall {
echo ''
read -p 'Install Ubuntu Restricted Extras? (Y)es, (N)o : ' REPLY
case $REPLY in
# Positive action
[Yy]* )
    echo 'Installing...'
    echo 'Requires root privileges:'
    sudo apt-get install -y ubuntu-restricted-extras
    echo 'Done.'
    main
    ;;
# Negative action
[Nn]* )
    clear && main;;
# Error
* )
    clear && echo 'Sorry, try again.' && codecinstall;;
esac
}

# INSTALL DEVELOPMENT TOOLS
function development {
echo ''
echo '1. Install development tools?'
echo '2. Install Ubuntu SDK?'
echo '3. Install Ubuntu Phablet Tools?'
echo 'r. Return'
echo ''
read -p 'What would you like to do? (Enter your choice) : ' REPLY
case $REPLY in
# Install Development Tools
1)
    echo 'Installing development tools...'
    echo ''
    echo 'Current package list:
    build-essential
    emacs24
    subversion
    git
    git-core
    ipython
    python-docutils
    python-pip
    python-dev
    python-virtualenv
    python-software-properties
    ctags
    chromium-browser
    haskell-platform
    nodejs'
    echo ''
    read -p 'Proceed? (Y)es, (N)o : ' REPLY
    case $REPLY in
    # Positive action
    [Yy]* )
        echo 'Requires root privileges:'
        # Feel free to change to whatever suits your preferences.
        sudo apt-get install -y build-essential emacs24 subversion git git-core ipython python-docutils python-pip python-dev python-virtualenv python-software-properties ctags haskell-platform chromium-browser nodejs
        # Set up git config
        git config --global user.name "Bruno Rasic"
        git config --global user.email brunorasic@outlook.com
        git config --global core.editor emacs
        git config --global alias.co checkout
        git config --global alias.br branch
        git config --global alias.ci commit
        git config --global alias.st status
        git config --global alias.unstage 'reset HEAD --'
        git config --global alias.last 'log -1 HEAD'
        #sudo wget -P /etc/bash_completion.d/ https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
        echo 'Done.'
        development
        ;;
    # Negative action
    [Nn]* )
        clear && development
        ;;
    # Error
    * )
        echo 'Sorry, try again.'
        development
        ;;
    esac
    ;;
# Install Ubuntu SDK
2)
    # Add repository
    echo 'Adding Ubuntu SDK Team PPA to software sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:ubuntu-sdk-team/ppa
    # Update repository information
    echo 'Updating repository information...'
    sudo apt-get update
    # Install package(s)
    echo 'Installing Ubuntu SDK...'
    sudo apt-get install -y ubuntu-sdk
    echo 'Done.'
    development
    ;;
# Install Ubuntu Phablet Tools
3)
    echo 'Installing Phablet Tools...'
    sudo apt-get install -y phablet-tools
    echo 'Done.'
    development
    ;;
# Return
[Rr]*)
    clear && main;;
# Invalid choice
* )
    clear && echo 'Not an option, try again.' && development;;
esac
}

# INSTALL DESIGN TOOLS
function design {
echo ''
echo 'Installing design tools...'
echo ''
echo 'Current package list:
fontforge
fontforge-extras
gimp
gimp-plugin-registry
gimp-data
gimp-data-extras
icontool
imagemagick
inkscape'
echo ''
read -p 'Proceed? (Y)es, (N)o : ' REPLY
case $REPLY in
# Positive action
[Yy]* )
    echo 'Requires root privileges:'
    # Feel free to change to whatever suits your preferences.
    sudo apt-get install -y fontforge fontforge-extras gimp gimp-plugin-registry gimp-data gimp-data-extras icontool imagemagick inkscape
    echo 'Done.'
    main
    ;;
# Negative action
[Nn]* )
    clear && main;;
# Error
* )
    clear && echo 'Sorry, try again.' && design
    ;;
esac
}

# THIRD PARTY THEMES
function themes {
echo 'What would you like to install? '
echo ''
echo '1. Moka Icon Theme'
echo '2. Faba Icon Theme'
echo '3. Moka GTK Theme'
echo '4. Moka GNOME Shell Theme'
echo '5. Orchis GTK Theme'
echo 'r. Return'
echo ''
read -p 'Enter your choice: ' REPLY
case $REPLY in
# Moka Icon Theme
1)
    # Add repository
    echo 'Adding Moka Icon Theme repository to sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:moka/moka-icon-theme
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Install package(s)
    echo 'Installing Moka icon themes...'
    echo 'Requires root privileges:'
    sudo apt-get install -y moka-icon-theme
    echo 'Done.'
    # Set Theme
    read -p "Do you want to set Moka as desktop theme? (Y)es, (N)o : " INPUT
    case $INPUT in
        [Yy]* )
            echo "Setting Moka as desktop Icon theme..."
            gsettings set org.gnome.desktop.interface icon-theme "Moka"
            echo "Done."
            themes
            ;;
        [Nn]* ) echo 'Done.'; themes;;
        * ) echo; echo "Uh oh, invalid response. Continuing without changes."; themes;;
    esac
    themes
    ;;
# Faba Icon Theme
2)
    # Add repository
    echo 'Adding Faba Icon Theme repository to sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:moka/faba-icon-theme
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Install package(s)
    echo 'Installing Faba icon themes...'
    echo 'Requires root privileges:'
    sudo apt-get install -y faba-icon-theme faba-mono-icons faba-icon-theme-symbolic
    echo 'Done.'
    themes
    ;;
# Moka GTK Theme
3)
    # Add repository
    echo 'Adding Moka GTK Theme repository to sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:moka/moka-gtk-theme
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Install package(s)
    echo 'Installing Moka GTK theme...'
    echo 'Requires root privileges:'
    sudo apt-get install -y moka-gtk-theme
    echo 'Done.'
    # Set Theme
    read -p "Do you want to set Moka as desktop theme? (Y)es, (N)o : " INPUT
    case $INPUT in
        [Yy]* )
            echo "Setting Moka as GTK window theme..."
            gsettings set org.gnome.desktop.wm.preferences theme "Moka"
            echo "Done."
            echo "Setting Moka as desktop GTK theme..."
            gsettings set org.gnome.desktop.interface gtk-theme "Moka"
            echo "Done."
            themes
            ;;
        [Nn]* ) echo 'Done.'; themes;;
        * ) echo; echo "Uh oh, invalid response. Continuing without changes."; themes;;
    esac
    themes
    ;;
# Moka GNOME Shell Theme
4)
    # Add repository
    echo 'Adding Moka GNOME Shell Theme repository to sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:moka/moka-gnome-shell-theme
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Install package(s)
    echo 'Installing Moka GNOME Shell theme...'
    echo 'Requires root privileges:'
    sudo apt-get install -y moka-gnome-shell-theme
    echo 'Done.'
    themes
    ;;
# Orchis GTK Theme
5)
    # Add repository
    echo 'Adding Orchis GTK Theme repository to sources...'
    echo 'Requires root privileges:'
    sudo add-apt-repository -y ppa:moka/orchis-gtk-theme
    # Update repository information
    echo 'Updating repository information...'
    echo 'Requires root privileges:'
    sudo apt-get update
    # Install package(s)
    echo 'Installing Orchis GTK theme...'
    echo 'Requires root privileges:'
    sudo apt-get install -y orchis-gtk-theme
    echo 'Done.'
    # Set Theme
    read -p "Do you want to set Orchis as desktop theme? (Y)es, (N)o : " INPUT
    case $INPUT in
        [Yy]* )
            echo "Setting Orchis as GTK window theme..."
            gsettings set org.gnome.desktop.wm.preferences theme "Orchis"
            echo "Done."
            echo "Setting Orchis as desktop GTK theme..."
            gsettings set org.gnome.desktop.interface gtk-theme "Orchis"
            echo "Done."
            themes
            ;;
        [Nn]* ) echo 'Done.'; themes;;
        * ) echo; echo "Uh oh, invalid response. Continuing without changes."; themes;;
    esac
    themes
    ;;
# Return
[Rr]*)
    clear && customize;;
# Invalid choice
* )
    clear && echo 'Not an option, try again.' && themes;;
esac
}

# CUSTOMIZATION
function customize {
echo ''
echo '1. Configure system?'
echo '2. Install Third-Party themes?'
echo 'r. Return'
echo ''
read -p 'What would you like to do? (Enter your choice) : ' REPLY
case $REPLY in
    1) clear && config;; # System Configuration
    2) clear && themes;; # Install Third-Party Themes
    [Rr]*) clear && main;; # Return
    * ) clear && echo 'Not an option, try again.' && customize;; # Invalid choice
esac
}

# THIRD PARTY APPLICATIONS
function thirdparty {
echo 'What would you like to install? '
echo ''
echo '1. Google Chrome?'
echo '2. Google Talk Plugin?'
echo '3. Google Music Manager?'
echo '4. Steam?'
echo '5. Sublime Text 3 (build 3059)?'
echo '6. Spotify client'
echo '7. Oracle Java8'
echo 'r. Return'
echo ''
read -p 'Enter your choice: ' REPLY
case $REPLY in
# Google Chrome
1)
    echo 'Downloading Google Chrome...'
    # Download Debian file that matches system architecture
    if [ $(uname -i) = 'i386' ]; then
        wget https://dl.google.com/linux/direct/google-chrome-stable_current_i386.deb
    elif [ $(uname -i) = 'x86_64' ]; then
        wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
    fi
    # Install package(s)
    echo 'Installing Google Chrome...'
    echo 'Requires root privileges:'b
    sudo dpkg -i google-chrome*.deb
    sudo apt-get install -fy
    # Cleanup and finish
    rm google-chrome*.deb
    cd
    echo 'Done.'
    thirdparty
    ;;
# Google Talk Plugin
2)
    echo 'Downloading Google Talk Plugin...'
    # Download Debian file that matches system architecture
    if [ $(uname -i) = 'i386' ]; then
        wget https://dl.google.com/linux/direct/google-talkplugin_current_i386.deb
    elif [ $(uname -i) = 'x86_64' ]; then
        wget https://dl.google.com/linux/direct/google-talkplugin_current_amd64.deb
    fi
    # Install package(s)
    echo 'Installing Google Talk Plugin...'
    echo 'Requires root privileges:'
    sudo dpkg -i google-talkplugin_current*.deb
    sudo apt-get install -fy
    # Cleanup and finish
    rm google-talkplugin_current*.deb
    cd
    echo 'Done.'
    thirdparty
    ;;
# Google Music Manager
3)
    echo 'Downloading Google Music Manager...'
    # Download Debian file that matches system architecture
    if [ $(uname -i) = 'i386' ]; then
        wget https://dl.google.com/linux/direct/google-musicmanager-beta_current_i386.deb
    elif [ $(uname -i) = 'x86_64' ]; then
        wget https://dl.google.com/linux/direct/google-musicmanager-beta_current_amd64.deb
    fi
    # Install package(s)
    echo 'Installing Google Music Manager...'
    echo 'Requires root privileges:'
    sudo dpkg -i google-musicmanager-*.deb
    sudo apt-get install -fy
    # Cleanup and finish
    rm google-musicmanager*.deb
    cd
    echo 'Done.'
    thirdparty
    ;;
# Steam
4)
    echo 'Downloading Steam...'
    cd $HOME/Downloads
    # Download Debian file that matches system architecture
    if [ $(uname -i) = 'i386' ]; then
        wget http://repo.steampowered.com/steam/archive/precise/steam_latest.deb
    elif [ $(uname -i) = 'x86_64' ]; then
        wget http://repo.steampowered.com/steam/archive/precise/steam_latest.deb
    fi
    # Install package(s)
    echo 'Installing Steam...'
    echo 'Requires root privileges:'
    sudo dpkg -i steam*.deb
    sudo apt-get install -fy
    # Cleanup and finish
    rm steam*.deb
    cd
    echo 'Done.'
    thirdparty
    ;;
# Sublime Text 3 (build 3059)
5)
    echo 'Downloading Sublime Text 3 (build 3059)...'
    # Download Debian file that matches system architecture
    if [ $(uname -i) = 'i386' ]; then
        wget http://c758482.r82.cf2.rackcdn.com/sublime-text_build-3059_i386.deb
    elif [ $(uname -i) = 'x86_64' ]; then
        wget http://c758482.r82.cf2.rackcdn.com/sublime-text_build-3059_amd64.deb
    fi
    # Install package(s)
    echo 'Installing Sublime Text 3 (build 3059)...'
    echo 'Requires root privileges:'
    sudo dpkg -i sublime-text_build-3059*.deb
    sudo apt-get install -fy
    # Create symbolic link
    echo 'Creating symbolic link...'
    echo 'Requires root privileges:'
    sudo ln -sf /opt/sublime_text/sublime_text /usr/bin/sublime
    echo 'Done.'
    # Cleanup and finish
    rm sublime-text_build-3059*.deb
    cd
    echo 'Done.'
    thirdparty
    ;;
# Spotify
6)
    # Add repository
    echo 'Adding Spotify repository to sources...'
    echo 'Creating apt list file...'
    touch spotify.list
    echo "deb http://repository.spotify.com stable non-free" >> spotify.list
    echo 'Moving spotify.list to /etc/apt/sources.list.d/'
    echo 'Requires root privileges:'
    sudo mv -f spotify.list /etc/apt/sources.list.d/
    echo 'Done.'
    # Update repository information
    echo 'Adding repository key and updating repository information...'
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 94558F59
    sudo apt-get update
    # Install package(s)
    echo 'Installing Spotify client...'
    sudo apt-get install -y spotify-client
    echo 'Done.'
    thirdparty
    ;;
# Oracle Java8
7)
    # Add repository
    echo 'Adding ppa:webupd8team/java repository to sources...'
    sudo add-apt-repository -y ppa:webupd8team/java
    # Update repository information
    sudo apt-get update
    # Install package(s)
    echo 'Installing Oracle Java 8 ...'
    sudo apt-get install -y oracle-java8-installer
    echo 'Done.'
    thirdparty
    ;;
# Return
[Rr]*)
    clear && main;;
# Invalid choice
* )
    clear && echo 'Not an option, try again.' && thirdparty;;
esac
}

# CONFIG
function config {
echo ''
echo '1. Set preferred application-specific settings?'
echo '2. Show all startup applications?'
echo 'r. Return'
echo ''
read -p 'What would you like to do? (Enter your choice) : ' REPLY
case $REPLY in
# GSettings
1)
    # Font Sizes
    echo 'Setting font preferences...'
    gsettings set org.gnome.desktop.interface text-scaling-factor '1.0'
    gsettings set org.gnome.desktop.interface document-font-name 'Ubuntu 10.5'
    gsettings set org.gnome.desktop.interface font-name 'Ubuntu 10.5'
    gsettings set org.gnome.desktop.interface monospace-font-name 'Ubuntu Mono 11'
    gsettings set org.gnome.nautilus.desktop font 'Ubuntu 10.5'
    gsettings set org.gnome.desktop.wm.preferences titlebar-font 'Ubuntu Bold 10.5'
    gsettings set org.gnome.settings-daemon.plugins.xsettings antialiasing 'rgba'
    gsettings set org.gnome.settings-daemon.plugins.xsettings hinting 'slight'
    # Unity Settings
    # echo 'Setting Unity preferences...'
    # echo 'Remove Unwanted Lenses from Unity Dash...
    # unity-lens-music
    # unity-lens-photos
    # unity-lens-gwibber
    # unity-lens-shopping
    # unity-lens-video'
    # echo 'Requires root privileges:'
    # sudo apt-get autoremove unity-lens-music unity-lens-photos unity-lens-gwibber unity-lens-shopping unity-lens-video
    # gsettings set com.canonical.Unity.ApplicationsLens display-available-apps false
    # gsettings set com.canonical.unity-greeter draw-user-backgrounds true
    # gsettings set com.canonical.indicator.power icon-policy 'charge'
    # gsettings set com.canonical.Unity.Lenses remote-content-search 'none'
    # gsettings set com.canonical.Unity.Lenses disabled-scopes "['more_suggestions-amazon.scope', 'more_suggestions-u1ms.scope', 'more_suggestions-populartracks.scope', 'music-musicstore.scope', 'more_suggestions-ebay.scope', 'more_suggestions-ubuntushop.scope', 'more_suggestions-skimlinks.scope']"
    # gsettings set com.canonical.indicator.session show-real-name-on-panel true
    # # Nautilus Preferences
    # echo 'Setting Nautilus preferences...'
    # gsettings set org.gnome.nautilus.preferences sort-directories-first true
    # # Rhythmbox Preferences
    # echo 'Setting Rhythmbox preferences...'
    # gsettings set org.gnome.rhythmbox.rhythmdb monitor-library true
    # gsettings set org.gnome.rhythmbox.sources browser-views 'artists-albums'
    # Done
    echo "Done."
    config
    ;;
# Startup Applications
2)
    echo 'Changing display of startup applications.'
    echo 'Requires root privileges:'
    cd /etc/xdg/autostart/ && sudo sed --in-place 's/NoDisplay=true/NoDisplay=false/g' *.desktop
    cd
    echo 'Done.'
    config
    ;;
# Return
[Rr]*)
    clear && main;;
# Invalid choice
* )
    clear && echo 'Not an option, try again.' && config;;
esac
}

# CLEANUP SYSTEM
function cleanup {
echo ''
echo '1. Remove unused pre-installed packages?'
echo '2. Remove old kernel(s)?'
echo '3. Remove orphaned packages?'
echo '4. Remove leftover configuration files?'
echo '5. Clean package cache?'
echo '6. Remove Unwanted Lenses from Unity Dash?'
echo '7. Remove all Java related packages (Sun, Oracle, OpenJDK, IcedTea plugins, GIJ)?'
echo 'r. Return?'
echo ''
read -p 'What would you like to do? (Enter your choice) : ' REPLY
case $REPLY in
# Remove Unused Pre-installed Packages
1)
    echo 'Removing selected pre-installed applications...'
    echo 'Requires root privileges:'
    sudo apt-get purge landscape-client-ui-install ubuntuone-control-panel* overlay*
    echo 'Done.'
    cleanup
    ;;
# Remove Old Kernel
2)
    echo 'Removing old Kernel(s)...'
    echo 'Requires root privileges:'
    sudo dpkg -l 'linux-*' | sed '/^ii/!d;/'"$(uname -r | sed "s/\(.*\)-\([^0-9]\+\)/\1/")"'/d;s/^[^ ]* [^ ]* \([^ ]*\).*/\1/;/[0-9]/!d' | grep -v linux-libc-dev | xargs sudo apt-get -y purge
    echo 'Done.'
    cleanup
    ;;
# Remove Orphaned Packages
3)
    echo 'Removing orphaned packages...'
    echo 'Requires root privileges:'
    sudo apt-get autoremove -y
    echo 'Done.'
    cleanup
    ;;
# Remove residual config files?
4)
    echo 'Removing leftover configuration files...'
    echo 'Requires root privileges:'
    sudo dpkg --purge $(COLUMNS=200 dpkg -l | grep '^rc' | tr -s ' ' | cut -d ' ' -f 2)
    echo 'Done.'
    cleanup
    ;;
# Clean Package Cache
5)
    echo 'Cleaning package cache...'
    echo 'Requires root privileges:'
    sudo apt-get clean
    echo 'Done.'
    cleanup
    ;;
# Remove Unwanted Lenses from Unity Dash
6)
    echo 'Remove Unwanted Lenses from Unity Dash...
    unity-lens-music
    unity-lens-photos
    unity-lens-gwibber
    unity-lens-shopping
    unity-lens-video'
    echo 'Requires root privileges:'
    sudo apt-get autoremove unity-lens-music unity-lens-photos unity-lens-gwibber unity-lens-shopping unity-lens-video
    echo 'Done.'
    cleanup
    ;;
# Clean Package Cache
7)
    echo 'Removing all Java related packages (Sun, Oracle, OpenJDK, IcedTea plugins, GIJ)...'
    echo 'Requires root privileges:'
    sudo apt-get update
    apt-cache search java | awk '{print($1)}' | grep -E -e '^(ia32-)?(sun|oracle)-java' -e '^openjdk-' -e '^icedtea' -e '^(default|gcj)-j(re|dk)' -e '^gcj-(.*)-j(re|dk)' -e 'java-common' | xargs sudo apt-get -y remove
    sudo apt-get -y autoremove
    # Purge config files
    dpkg -l | grep ^rc | awk '{print($2)}' | xargs sudo apt-get -y purge
    # Remove Java config and cache directory
    sudo bash -c 'ls -d /home/*/.java' | xargs sudo rm -rf
    # Remove manually installed JVMs:
    sudo rm -rf /usr/lib/jvm/*
    # Remove Java entries, if there is still any, from the alternatives
    for g in ControlPanel java java_vm javaws jcontrol jexec keytool mozilla-javaplugin.so orbd pack200 policytool rmid rmiregistry servertool tnameserv unpack200 appletviewer apt extcheck HtmlConverter idlj jar jarsigner javac javadoc javah javap jconsole jdb jhat jinfo jmap jps jrunscript jsadebugd jstack jstat jstatd native2ascii rmic schemagen serialver wsgen wsimport xjc xulrunner-1.9-javaplugin.so; do sudo update-alternatives --remove-all $g; done
    echo 'Done.'
    cleanup
    ;;
# Return
[Rr]*)
    clear && main;;
# Invalid choice
* )
    clear && echo 'Not an option, try again.' && cleanup;;
esac
}

# Quit
function quit {
read -p "Are you sure you want to quit? (Y)es, (N)o " REPLY
case $REPLY in
    [Yy]* ) exit 99;;
    [Nn]* ) clear && main;;
    * ) clear && echo 'Sorry, try again.' && quit;;
esac
}

#----- MAIN FUNCTION -----#
function main {
echo ''
echo '1. Perform system update & upgrade?'
echo '2. Install favorite applications?'
echo '3. Install favorite system utilities?'
echo '4. Install development tools?'
echo '5. Install design tools?'
echo '6. Install extra GNOME components?'
echo '7. Install Ubuntu Restricted Extras?'
echo '8. Install third-party applications?'
echo '9. Install laptop battery improvement tools?'
echo '10. Customize system?'
echo '11. Cleanup the system?'
echo 'q. Quit?'
echo ''
read -p 'What would you like to do? (Enter your choice) : ' REPLY
case $REPLY in
    1) sysupgrade;; # System Upgrade
    2) clear && favorites;; # Install Favorite Applications
    3) clear && system;; # Install Favorite Tools
    4) clear && development;; # Install Dev Tools
    5) clear && design;; # Install Design Tools
    #6) clear && gnome;; # Install GNOME components
    7) clear && codecinstall;; # Install Ubuntu Restricted Extras
    8) clear && thirdparty;; # Install Third-Party Applications
    9) clear && laptop;; # Customize system
    10) clear && customize;; # Customize system
    11) clear && cleanup;; # Cleanup System
    [Qq]* ) echo '' && quit;; # Quit
    * ) clear && echo 'Not an option, try again.' && main;;
esac
}

#----- RUN MAIN FUNCTION -----#
main

#END OF SCRIPT
