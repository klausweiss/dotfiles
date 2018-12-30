install: link-config install-plugins

link-config: ~/.emacs.d

~/.emacs.d: ~/.dotfiles/emacs
	@echo "Linking emacs config directory"
	@ln -sT --interactive ~/.dotfiles/emacs ~/.emacs.d

install-plugins:
	@echo "Installing emacs plugins"
	@emacs --batch -l ~/.emacs.d/init.el
