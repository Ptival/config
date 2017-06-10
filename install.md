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
