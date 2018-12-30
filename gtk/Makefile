install: link-config

link-config: ~/.gtkrc-2.0 ~/.config/gtk-3.0/settings.ini

~/.gtkrc-2.0: ~/.dotfiles/gtk/gtkrc-2.0 
	@echo "Linking gtk2 config"
	@ln -sT --interactive ~/.dotfiles/gtk/gtkrc-2.0 ~/.gtkrc-2.0

~/.config/gtk-3.0/settings.ini: ~/.dotfiles/gtk/gtk3-settings.ini
	@echo "Linking gtk3 config"
	@ln -sT --interactive ~/.dotfiles/gtk/gtk3-settings.ini ~/.config/gtk-3.0/settings.ini
