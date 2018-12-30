install: link-config disable-motd

link-config: ~/.config/fish

~/.config/fish: ~/.dotfiles/fish
	@echo "Linking fish config directory"
	@ln -sT --interactive ~/.dotfiles/fish ~/.config/fish

disable-motd:
	@echo "Disabling fish motd"
	@fish -c "set -U fish_greeting"
