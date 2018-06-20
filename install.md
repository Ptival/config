* To set up NixOS

In /etc/nixos, symlink configuration.nix to /home/ptival/config/configuration.nix

In /home/ptival/config, symlink:
hardware-configuration.nix -> hardware-configuration/{machine}.nix
      machine-specifig.nix ->       machine-specific/{machine}.nix

No need to make up extra symlinks in /etc/nixos, since local paths are resolved
after following the symlink.

Also, it's nice to have a local copy of nixpkgs, and then:

sudo nixos-rebuild switch -I nixpkgs=/home/ptival/nixpkgs

* To reinstall oh-my-zsh

git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

* To reinstall powerlevel9k

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

* To reinstall awesome-terminal-fonts

git clone https://github.com/gabrielelana/awesome-terminal-fonts.git
cd awesome-terminal-fonts
gco patching-strategy
mkdir -p ~/.local/share/fonts
cp patched/*.ttf ~/.local/share/fonts
fc-cache -rv
