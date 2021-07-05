SHELL := /bin/bash
PLUGINS_DIR := pack/default/start


define get-all-prefix-targets
	cat $(MAKEFILE_LIST) | grep "^$(1)-.*:" | sed "s/:.*//g" | tr "\n" " "
endef
all-update-targets := $(shell $(call get-all-prefix-targets,"update"))
all-compile-targets := $(shell $(call get-all-prefix-targets,"compile"))

define get-all-lib-dirs
	$(eval MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
	find $(MAKEFILE_DIR) -type d | sed "s/^\//-L \//g" | tr "\n" " "
endef


.PHONY: clean \
	$(all-compile-targets) \
	update \
	$(all-update-targets) \

update: $(all-update-targets)

define clone-repo
	rm -rf $(2)
	git clone $(1) $(2) --depth 1 --recurse-submodules
endef

define copy-if-any-srcs
	if test -n "$(strip $(1))"; then \
		cp -r $(1) $(2)/; \
	fi
endef

define copy-existing-srcs
	rsync --ignore-missing-args -r $(1) $(2)/
endef

define clear-junk
	find $(1) -name ".git*" -type d -exec rm -rf {} +
endef

define download-from-repo
	$(eval DEST_DIR = $(PLUGINS_DIR)/$(notdir $(basename $(1))))
	$(call clone-repo, $(1), $(DEST_DIR))
	$(call clear-junk, $(DEST_DIR))
endef


update-barbar: \
	update-nvim-web-devicons \

	$(call download-from-repo, https://github.com/romgrk/barbar.nvim)

update-compe:
	$(call download-from-repo, https://github.com/hrsh7th/nvim-compe)

update-gruvbox-material:
	$(call download-from-repo, https://github.com/sainnhe/gruvbox-material)

update-neogit: \
	update-plenary \

	$(call download-from-repo, https://github.com/TimUntersberger/neogit)

update-nvim-lsp-config:
	$(call download-from-repo, https://github.com/neovim/nvim-lspconfig)

update-nvim-web-devicons:
	$(call download-from-repo, https://github.com/kyazdani42/nvim-web-devicons)

update-plenary:
	$(call download-from-repo, https://github.com/nvim-lua/plenary.nvim)

update-popup: \
	update-plenary \

	$(call download-from-repo, https://github.com/nvim-lua/popup.nvim)

update-telescope: \
	update-popup \
	update-plenary \

	$(call download-from-repo, https://github.com/nvim-telescope/telescope.nvim)

update-undotree:
	$(call download-from-repo, https://github.com/mbbill/undotree)

update-vim-airline:
	$(call download-from-repo, https://github.com/vim-airline/vim-airline)

update-vim-gitgutter:
	$(call download-from-repo, https://github.com/airblade/vim-gitgutter)

update-vim-polyglot:
	$(call download-from-repo, https://github.com/sheerun/vim-polyglot)

