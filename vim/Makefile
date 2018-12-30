install: link-config install-plugins

link-config: ~/.vimrc ~/.vim

~/.vimrc: ~/.dotfiles/vim/vimrc
	@echo "Linking vim config file"
	@ln -sT --interactive ~/.dotfiles/vim/vimrc ~/.vimrc

~/.vim: ~/.dotfiles/vim/vim
	@echo "Linking vim config directory"
	@ln -sT --interactive ~/.dotfiles/vim/vim ~/.vim

install-plugins:
	@echo "Installing vim plugins"
	@vim +PlugInstall +qall
