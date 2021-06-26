SHELL := /bin/bash

ALL_THE_ICONS_REPO := https://github.com/domtronn/all-the-icons.el
CMAKE_MODE_URL := https://raw.githubusercontent.com/Kitware/CMake/master/Auxiliary/cmake-mode.el
COMPANY_REPO := https://github.com/company-mode/company-mode.git
DASHBOARD_REPO := https://github.com/emacs-dashboard/emacs-dashboard.git
	PAGE_BREAK_LINES_URL := https://raw.githubusercontent.com/purcell/page-break-lines/master/page-break-lines.el
DOCKERFILE_MODE_URL := https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el
EXEC_PATH_FROM_SHELL_URL := https://raw.githubusercontent.com/purcell/exec-path-from-shell/master/exec-path-from-shell.el
EXPAND_REGION_REPO := https://github.com/magnars/expand-region.el.git
FLYCHECK_REPO := https://github.com/flycheck/flycheck.git
FORMAT_ALL_THE_CODE_URL := https://raw.githubusercontent.com/lassik/emacs-format-all-the-code/master/format-all.el
	INHERITENV_URL := https://raw.githubusercontent.com/purcell/inheritenv/main/inheritenv.el
	LANGUAGE_ID_URL := https://raw.githubusercontent.com/lassik/emacs-language-id/master/language-id.el
HASKELL_MODE_REPO := https://github.com/haskell/haskell-mode.git
JUMP_TREE_MODE_REPO := https://github.com/yangwen0228/jump-tree
LSP_MODE_REPO := https://github.com/emacs-lsp/lsp-mode.git
	F_URL := https://raw.githubusercontent.com/rejeep/f.el/master/f.el
	HT_URL := https://raw.githubusercontent.com/Wilfred/ht.el/master/ht.el
	LV_URL := https://raw.githubusercontent.com/abo-abo/hydra/master/lv.el
	MARKDOWN_URL := https://raw.githubusercontent.com/jrblevin/markdown-mode/master/markdown-mode.el
	S_URL := https://raw.githubusercontent.com/magnars/s.el/master/s.el
	SPINNER_URL := https://raw.githubusercontent.com/Malabarba/spinner.el/master/spinner.el
LSP_UI_REPO := https://github.com/emacs-lsp/lsp-ui.git
LSP_HASKELL_URL := https://raw.githubusercontent.com/emacs-lsp/lsp-haskell/master/lsp-haskell.el
LSP_IVY_URL := https://raw.githubusercontent.com/emacs-lsp/lsp-ivy/master/lsp-ivy.el
LUA_MODE_URL := https://raw.githubusercontent.com/immerrr/lua-mode/master/lua-mode.el
	DASH_FUNCTIONAL_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash-functional.el
MAGIT_REPO := https://github.com/magit/magit.git
	TRANSIENT_URL := https://raw.githubusercontent.com/magit/transient/master/lisp/transient.el
	WITH_EDITOR_URL := https://raw.githubusercontent.com/magit/with-editor/master/with-editor.el
	DASH_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
MINI_MODELINE_URL := https://raw.githubusercontent.com/kiennq/emacs-mini-modeline/master/mini-modeline.el
PERSPECTIVE_URL := https://raw.githubusercontent.com/nex3/perspective-el/master/perspective.el
PERSPECTIVE_PROJECTILE_URL := https://raw.githubusercontent.com/bbatsov/persp-projectile/master/persp-projectile.el
PROJECTILE_URL := https://raw.githubusercontent.com/bbatsov/projectile/master/projectile.el
RUSTIC_REPO := https://github.com/brotzeit/rustic.git
	XTERM_COLOR_URL := https://raw.githubusercontent.com/atomontage/xterm-color/master/xterm-color.el
SIMPLECLIP_URL := https://raw.githubusercontent.com/rolandwalker/simpleclip/master/simpleclip.el
SMART_SHIFT_URL := https://raw.githubusercontent.com/klausweiss/smart-shift/master/smart-shift.el
SWIPER_REPO := https://github.com/abo-abo/swiper.git
TREEMACS_REPO := https://github.com/Alexander-Miller/treemacs.git
	PFUTURE_URL := https://raw.githubusercontent.com/Alexander-Miller/pfuture/master/pfuture.el
UNDO_TREE_URL := http://www.dr-qubit.org/undo-tree/undo-tree.el
	QUEUE_URL := https://www.dr-qubit.org/predictive/queue.el
VISUAL_REGEXP_URL := https://raw.githubusercontent.com/benma/visual-regexp.el/master/visual-regexp.el
WHICH_KEY_URL := https://raw.githubusercontent.com/justbur/emacs-which-key/master/which-key.el
WINUM_URL := https://raw.githubusercontent.com/deb0ch/emacs-winum/master/winum.el
YAFOLDING_URL := https://raw.githubusercontent.com/zenozeng/yafolding.el/master/yafolding.el
YAML_URL := https://raw.githubusercontent.com/yaml/yaml-mode/master/yaml-mode.el
YASNIPPET_REPO := https://github.com/joaotavora/yasnippet.git
YASNIPPET_SNIPPETS_REPO := https://github.com/AndreaCrotti/yasnippet-snippets.git


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
	compile \
	$(all-compile-targets) \
	update \
	$(all-update-targets) \

clean:
	find -name '*.elc' -exec rm {} +

compile: $(all-compile-targets)
	emacs --batch \
		-L . \
		$(shell $(call get-all-lib-dirs)) \
		--execute '(byte-recompile-directory "." 0)'

update: $(all-update-targets)

define download-from-url
	wget -N $(1)
endef

define clone-repo
	$(eval SUFFIX = $(notdir $(basename $(1))))
	$(eval DIR = $(shell mktemp --directory --suffix $(SUFFIX)))
	git clone $(1) $(DIR) --depth 1
endef

define copy-if-any-srcs
	if test -n "$(strip $(1))"; then \
		cp -r $(1) $(2)/; \
	fi
endef

define copy-existing-srcs
	rsync --ignore-missing-args -r $(1) $(2)/
endef

define update-lib-files
	$(eval LIB_DIR = $(1))
	rm -rf $(LIB_DIR)/
	mkdir $(LIB_DIR)

	$(eval FILES_TO_COPY = $(2))
	$(eval ABS_FILES = $(foreach F,$(FILES_TO_COPY),$(DIR)/$(F)))
	$(call copy-if-any-srcs, $(ABS_FILES), $(LIB_DIR))

	$(eval STANDARD_FILES_TO_COPY = *.el README*)
	$(eval ABS_STANDARD_FILES = $(foreach F,$(STANDARD_FILES_TO_COPY),$(strip $(DIR)/$(F))))
	$(call copy-existing-srcs, $(ABS_STANDARD_FILES), $(LIB_DIR))
endef

define clean-repo-dir
	rm -rf $(DIR)
endef

define download-from-repo
	$(eval DEST_DIR = $(notdir $(basename $(1))))
	$(call clone-repo, $(1))
	$(call update-lib-files, $(DEST_DIR), $(2))
	$(call clean-repo-dir)
endef


update-all-the-icons:
	$(call download-from-repo, $(ALL_THE_ICONS_REPO), data/)

update-company:
	$(call download-from-repo, $(COMPANY_REPO))

update-cmake-mode:
	$(call download-from-url, $(CMAKE_MODE_URL))

update-dash:
	$(call download-from-url, $(DASH_URL))
	$(call download-from-url, $(DASH_FUNCTIONAL_URL))

update-dashboard: update-page-break-lines
	$(call download-from-repo, $(DASHBOARD_REPO), banners/)

update-dockerfile-mode:
	$(call download-from-url, $(DOCKERFILE_MODE_URL))

update-exec-path-from-shell:
	$(call download-from-url, $(EXEC_PATH_FROM_SHELL_URL))

update-expand-region:
	$(call download-from-repo, $(EXPAND_REGION_REPO))

update-f:
	$(call download-from-url, $(F_URL))

update-format-all: \
	update-inheritenv \
	update-language-id \

	$(call download-from-url, $(FORMAT_ALL_THE_CODE_URL))

update-flycheck:
	$(call download-from-repo, $(FLYCHECK_REPO))

update-haskell-mode:
	$(call clone-repo, $(HASKELL_MODE_REPO))
	make -C $(DIR)
	$(call update-lib-files, haskell-mode)
	$(call clean-repo-dir)

update-ht:
	$(call download-from-url, $(HT_URL))

update-inheritenv:
	$(call download-from-url, $(INHERITENV_URL))

update-jump-tree-mode:
	$(call download-from-repo, $(JUMP_TREE_MODE_REPO))

update-language-id:
	$(call download-from-url, $(LANGUAGE_ID_URL))

update-lsp-mode: \
	update-dash \
	update-f \
	update-ht \
	update-lv \
	update-markdown \
	update-s \
	update-spinner \

	$(call download-from-repo, $(LSP_MODE_REPO), scripts/* clients/)

update-lsp-haskell: \
	update-lsp-mode \
	update-haskell-mode \

	$(call download-from-url, $(LSP_HASKELL_URL))

update-lsp-ivy: \
	update-lsp-mode \
	update-dash \
	update-swiper \

	$(call download-from-url, $(LSP_IVY_URL))

update-lsp-ui: \
	update-dash \
	update-lsp-mode \
	update-markdown \

	$(call download-from-repo, $(LSP_UI_REPO), resources/)

update-lua-mode:
	$(call download-from-url, $(LUA_MODE_URL))

update-lv:
	$(call download-from-url, $(LV_URL))

update-magit: \
	update-with-editor \
	update-transient \
	update-dash \

	$(call clone-repo, $(MAGIT_REPO))
	make -C $(DIR) versionlib
	$(call update-lib-files, magit, lisp/*)
	$(call clean-repo-dir)

update-markdown:
	$(call download-from-url, $(MARKDOWN_URL))

update-mini-modeline:
	$(call download-from-url, $(MINI_MODELINE_URL))

update-page-break-lines:
	$(call download-from-url, $(PAGE_BREAK_LINES_URL))

update-perspective:
	$(call download-from-url, $(PERSPECTIVE_URL))

update-persp-projectile: \
	update-perspective \
	update-projectile \

	$(call download-from-url, $(PERSPECTIVE_PROJECTILE_URL))

update-pfuture:
	$(call download-from-url, $(PFUTURE_URL))

update-projectile:
	$(call download-from-url, $(PROJECTILE_URL))

update-queue:
	$(call download-from-url, $(QUEUE_URL))

update-rustic: update-xterm-color
	$(call download-from-repo, $(RUSTIC_REPO))

update-s:
	$(call download-from-url, $(S_URL))

update-spinner:
	$(call download-from-url, $(SPINNER_URL))

update-simpleclip:
	$(call download-from-url, $(SIMPLECLIP_URL))

update-smart-shift:
	$(call download-from-url, $(SMART_SHIFT_URL))

update-swiper:
	$(call download-from-repo, $(SWIPER_REPO))

update-transient:
	$(call download-from-url, $(TRANSIENT_URL))

update-treemacs: \
	update-dash \
	update-f \
	update-ht \
	update-pfuture \
	update-s \

	$(call download-from-repo, $(TREEMACS_REPO), icons/ src/)

update-undo-tree: \
	update-queue \

	$(call download-from-url, $(UNDO_TREE_URL))

update-visual-regexp:
	$(call download-from-url, $(VISUAL_REGEXP_URL))

update-which-key:
	$(call download-from-url, $(WHICH_KEY_URL))

update-winum:
	$(call download-from-url, $(WINUM_URL))

update-with-editor:
	$(call download-from-url, $(WITH_EDITOR_URL))

update-xterm-color:
	$(call download-from-url, $(XTERM_COLOR_URL))

update-yafolding-mode:
	$(call download-from-url, $(YAFOLDING_URL))

update-yaml-mode:
	$(call download-from-url, $(YAML_URL))

update-yasnippet:
	$(call download-from-repo, $(YASNIPPET_REPO))

update-yasnippet-snippets:
	$(call download-from-repo, $(YASNIPPET_SNIPPETS_REPO), snippets)
