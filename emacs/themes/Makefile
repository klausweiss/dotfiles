SHELL := /bin/bash

AUTOTHEMER_URL := https://raw.githubusercontent.com/jasonm23/autothemer/master/autothemer.el
GRUVBOX_REPO := https://github.com/greduan/emacs-theme-gruvbox.git

.PHONY: clean \
	compile \
	update-autothemer \
	update-gruvbox \

clean:
	find -name '*.elc' -exec rm {} +

compile:
	emacs --batch \
	        -L . \
	        -L ../lib \
	        -L gruvbox \
		--execute '(byte-recompile-directory "." 0)'

update: \
	update-autothemer \
	update-gruvbox \


update-autothemer:
	wget -N $(AUTOTHEMER_URL)

update-gruvbox:
	$(eval DIR = $(shell mktemp --directory --suffix gruvbox))
	git clone $(GRUVBOX_REPO) $(DIR) --depth 1
	rm -rf gruvbox/
	mkdir gruvbox/
	cp -r $(DIR)/{*.el,README*} gruvbox/
	rm -rf $(DIR)

