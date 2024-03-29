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
	$(eval REPO = $(1))
	$(eval DIR = $(2))
	$(eval BRANCH = $(3))
	$(eval BRANCH_OPTION =
	ifneq ($(strip $(3)),)
	override BRANCH_OPTION = --branch $(BRANCH)
	endif)
	rm -rf $(DIR)
	git clone $(REPO) $(DIR) --depth 1 --recurse-submodules $(BRANCH_OPTION)
	$(eval override BRANCH_OPTION = )
endef

define clone-subdirectory
	$(eval REPO = $(1))
	$(eval SUBDIR = $(2))
	$(eval DIR = $(3))
	$(eval BRANCH = $(4))
	$(eval BRANCH_OPTION =
	ifneq ($(strip $(4)),)
	override BRANCH_OPTION = --branch $(BRANCH)
	endif)
	rm -rf $(DIR)
	git clone $(REPO) $(DIR) --depth 1 $(BRANCH_OPTION) --sparse --filter=blob:none
	cd $(DIR); \
	git sparse-checkout set $(SUBDIR)
	# commands above create a $(SUBDIR) directory in $(DIR)
	cp -r $(DIR)/$(SUBDIR)/* $(DIR)
	rm -rf $(DIR)/$(SUBDIR)
	$(eval override BRANCH_OPTION = )
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
	$(eval REPO = $(1))
	$(eval BRANCH = $(2))
	$(eval DEST_DIR = $(PLUGINS_DIR)/$(notdir $(basename $(REPO))))
	$(call clone-repo, $(REPO), $(DEST_DIR), $(BRANCH))
	$(call clear-junk, $(DEST_DIR))
endef

define download-from-git-subdirectory
	$(eval REPO = $(1))
	$(eval SUBDIR = $(2))
	$(eval DEST_DIR = $(PLUGINS_DIR)/$(notdir $(basename $(REPO))))
	$(call clone-subdirectory, $(REPO), $(SUBDIR), $(DEST_DIR))
	$(call clear-junk, $(DEST_DIR))
endef


update-autosession:
	$(call download-from-repo, https://github.com/rmagatti/auto-session)

update-barbar: \
	update-nvim-web-devicons \

	$(call download-from-repo, https://github.com/romgrk/barbar.nvim)

update-Comment:
	$(call download-from-repo, https://github.com/numToStr/Comment.nvim)

update-cmp:
	$(call download-from-repo, https://github.com/hrsh7th/nvim-cmp)
	$(call download-from-repo, https://github.com/hrsh7th/cmp-nvim-lsp)
	$(call download-from-repo, https://github.com/hrsh7th/cmp-buffer)
	$(call download-from-repo, https://github.com/hrsh7th/cmp-path)
	$(call download-from-repo, https://github.com/hrsh7th/cmp-cmdline)
	$(call download-from-repo, https://github.com/saadparwaiz1/cmp_luasnip)

update-flutter-tools: \
	update-plenary \

	$(call download-from-repo, https://github.com/akinsho/flutter-tools.nvim)

update-gitsigns: \
	update-plenary \

	$(call download-from-repo, https://github.com/lewis6991/gitsigns.nvim)

update-gruvbox-material:
	$(call download-from-repo, https://github.com/sainnhe/gruvbox-material)
	sh after-update/tweak-gruvbox-material.sh

update-hop:
	$(call download-from-repo, https://github.com/phaazon/hop.nvim)

update-koka:
	$(call download-from-git-subdirectory, https://github.com/koka-lang/koka, support/vim)

update-lazygit:
	$(call download-from-repo, https://github.com/kdheepak/lazygit.nvim)

update-lsp_signature:
	$(call download-from-repo, https://github.com/ray-x/lsp_signature.nvim)

update-lualine:
	$(call download-from-repo, https://github.com/hoob3rt/lualine.nvim)

update-luasnip:
	$(call download-from-repo, https://github.com/L3MON4D3/LuaSnip)

update-mason:
	$(call download-from-repo, https://github.com/williamboman/mason.nvim)

update-mason-lspconfig: \
	update-mason \
	update-nvim-lspconfig \

	$(call download-from-repo, https://github.com/williamboman/mason-lspconfig.nvim)

update-sonokai:
	$(call download-from-repo, https://github.com/sainnhe/sonokai)

update-neogit: \
	update-plenary \

	$(call download-from-repo, https://github.com/TimUntersberger/neogit)

update-nvim-autopairs:
	$(call download-from-repo, https://github.com/windwp/nvim-autopairs)

update-nvim-lspconfig:
	$(call download-from-repo, https://github.com/neovim/nvim-lspconfig)

update-nvim-tree: \
	update-nvim-web-devicons \

	$(call download-from-repo, https://github.com/kyazdani42/nvim-tree.lua)

update-nvim-treesitter:
	$(call download-from-repo, https://github.com/nvim-treesitter/nvim-treesitter)

update-nvim-web-devicons:
	$(call download-from-repo, https://github.com/kyazdani42/nvim-web-devicons)

update-plenary:
	$(call download-from-repo, https://github.com/nvim-lua/plenary.nvim)

update-popup: \
	update-plenary \

	$(call download-from-repo, https://github.com/nvim-lua/popup.nvim)

update-readline.vim:
	$(call download-from-repo, https://github.com/ryvnf/readline.vim)

update-session-lens:
	$(call download-from-repo, https://github.com/rmagatti/session-lens)

update-snippets-flutter:
	$(call download-from-repo, https://github.com/Nash0x7E2/awesome-flutter-snippets)

update-snippets-friendly-snippets:
	$(call download-from-repo, https://github.com/rafamadriz/friendly-snippets)

update-symbols-outline:
	$(call download-from-repo, https://github.com/simrat39/symbols-outline.nvim)

update-telescope: \
	update-popup \
	update-plenary \

	$(call download-from-repo, https://github.com/nvim-telescope/telescope.nvim)

update-telescope-ui-select: \
	update-telescope \

	$(call download-from-repo, https://github.com/nvim-telescope/telescope-ui-select.nvim)

update-undotree:
	$(call download-from-repo, https://github.com/mbbill/undotree)

update-trouble: \
	update-nvim-web-devicons \

	$(call download-from-repo, https://github.com/folke/trouble.nvim)

update-vim-polyglot:
	$(call download-from-repo, https://github.com/sheerun/vim-polyglot)

update-vim-rooter:
	$(call download-from-repo, https://github.com/airblade/vim-rooter)

update-vim-table-mode:
	$(call download-from-repo, https://github.com/dhruvasagar/vim-table-mode)

