install: install-packages allow-s2ram

install-packages:
	@echo "Installing packages"
	@sudo apt install -y \
		awesome \
		emacs25 \
		fish \
		redshift \
		rofi \
		slock \
		synapse \
		tmux \
		uswsusp \
		vim \
		xterm

allow-s2ram:
	@echo "Allowing all users to use s2ram"
	@sudo chmod +s /usr/sbin/s2ram
