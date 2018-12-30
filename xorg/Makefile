install: link-config link-wallpaper create-userdirs reload-xresources

link-config: ~/.config/user-dirs.dirs ~/.xinitrc ~/.Xresources ~/.Xresources.d/

link-wallpaper: ~/.config/wallpaper

~/.config/user-dirs.dirs: ~/.dotfiles/xorg/user-dirs
	@echo "Linking user-dirs"
	@ln -sT --interactive ~/.dotfiles/xorg/user-dirs ~/.config/user-dirs.dirs

~/.config/wallpaper: ~/.dotfiles/xorg/wallpaper.png 
	@echo "Linking wallpaper"
	@ln -sT --interactive ~/.dotfiles/xorg/wallpaper.png ~/.config/wallpaper

~/.xinitrc: ~/.dotfiles/xorg/xinitrc 
	@echo "Linking .xinitrc"
	@ln -sT --interactive ~/.dotfiles/xorg/xinitrc ~/.xinitrc

~/.Xresources: ~/.dotfiles/xorg/Xresources
	@echo "Linking Xresources"
	@ln -sT --interactive ~/.dotfiles/xorg/Xresource ~/.Xresources

create-userdirs: ~/doc/ ~/src/ ~/tmp/
	@echo "Creating userdirs"
	@./create-user-dirs

~/.Xresources.d:
	@echo "Creating Xresources config dir"
	@mkdir -p ~/.Xresources.d/

reload-xresources: ~/.Xresources.d ~/.Xresources
	@echo "Loading Xresources config"
	@xrdb -I"$HOME" -load ~/.Xresources
