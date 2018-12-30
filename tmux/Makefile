install: link-config

link-config: ~/.tmux.conf

~/.tmux.conf: ~/.dotfiles/tmux/tmux.conf
	@echo "Linking tmux config"
	@ln -sT --interactive ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf
