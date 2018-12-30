install: link-config

link-config: ~/.Xresources.d/xterm.conf

~/.Xresources.d/xterm.conf: ~/.dotfiles/xterm/Xresources.conf
	@echo "Linking xterm config"
	@ln -sT --interactive ~/.dotfiles/xterm/Xresources.conf ~/.Xresources.d/xterm.conf
