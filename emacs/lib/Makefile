SHELL := /bin/bash

ALL_THE_ICONS_REPO := https://github.com/domtronn/all-the-icons.el
	MEMOIZE_URL := https://raw.githubusercontent.com/skeeto/emacs-memoize/master/memoize.el
AWESOME_TAB_URL := https://raw.githubusercontent.com/manateelazycat/awesome-tab/master/awesome-tab.el
COMPANY_REPO := https://github.com/company-mode/company-mode.git
COMPANY_BOX_REPO := https://github.com/sebastiencs/company-box.git
	DASH_FUNCTIONAL_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash-functional.el
CUA_LITE_URL := https://raw.githubusercontent.com/emacsattic/cua-lite/master/cua-lite.el
DASHBOARD_REPO := https://github.com/emacs-dashboard/emacs-dashboard.git
	PAGE_BREAK_LINES_URL := https://raw.githubusercontent.com/purcell/page-break-lines/master/page-break-lines.el
EXPAND_REGION_REPO := https://github.com/magnars/expand-region.el.git
IVY_POSFRAME_URL := https://raw.githubusercontent.com/tumashu/ivy-posframe/master/ivy-posframe.el
	POSFRAME_URL := https://raw.githubusercontent.com/tumashu/posframe/master/posframe.el
LSP_MODE_REPO := https://github.com/emacs-lsp/lsp-mode.git
	F_URL := https://raw.githubusercontent.com/rejeep/f.el/master/f.el
	HT_URL := https://raw.githubusercontent.com/Wilfred/ht.el/master/ht.el
	LV_URL := https://raw.githubusercontent.com/abo-abo/hydra/master/lv.el
	MARKDOWN_URL := https://raw.githubusercontent.com/jrblevin/markdown-mode/master/markdown-mode.el
	S_URL := https://raw.githubusercontent.com/magnars/s.el/master/s.el
	SPINNER_URL := https://raw.githubusercontent.com/Malabarba/spinner.el/master/spinner.el
LSP_UI_REPO := https://github.com/emacs-lsp/lsp-ui.git
MAGIT_REPO := https://github.com/magit/magit.git
	TRANSIENT_URL := https://raw.githubusercontent.com/magit/transient/master/lisp/transient.el
	WITH_EDITOR_URL := https://raw.githubusercontent.com/magit/with-editor/master/with-editor.el
	DASH_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
PERSPECTIVE_URL := https://raw.githubusercontent.com/nex3/perspective-el/master/perspective.el
PERSPECTIVE_PROJECTILE_URL := https://raw.githubusercontent.com/bbatsov/persp-projectile/master/persp-projectile.el
PROJECTILE_URL := https://raw.githubusercontent.com/bbatsov/projectile/master/projectile.el
SIMPLECLIP_URL := https://raw.githubusercontent.com/rolandwalker/simpleclip/master/simpleclip.el
SMART_SHIFT_URL := https://raw.githubusercontent.com/klausweiss/smart-shift/master/smart-shift.el
SWIPER_REPO := https://github.com/abo-abo/swiper.git
UNDO_TREE_URL := http://www.dr-qubit.org/undo-tree/undo-tree.el
VISUAL_REGEXP_URL := https://raw.githubusercontent.com/benma/visual-regexp.el/master/visual-regexp.el
WHICH_KEY_URL := https://raw.githubusercontent.com/justbur/emacs-which-key/master/which-key.el
define get-all-update-targets
	cat $(MAKEFILE_LIST) | grep '^update-.*:' | sed 's/:.*//g' | tr '\n' ' '
endef

all-update-targets := $(shell $(call get-all-update-targets))


.PHONY: clean \
	compile \
	update \
	$(all-update-targets) \

clean:
	find -name '*.elc' -exec rm {} +

compile:
	emacs --batch \
		-L . \
		-L company \
		-L company-box \
		-L dashboard \
		-L expand-region \
		-L lsp-mode \
		-L lsp-ui \
		-L magit \
		-L swiper \
		--execute '(byte-recompile-directory "." 0)'

update: $(all-update-targets)

define download-from-url
	wget -N $(1)
endef

define clone-repo
	$(eval DIR = $(shell mktemp --directory --suffix $(2)))
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
	$(call clone-repo, $(1), $(2))
	$(call update-lib-files, $(2), $(3))
	$(call clean-repo-dir)
endef


update-all-the-icons: update-memoize
	$(call download-from-repo, $(ALL_THE_ICONS_REPO), all-the-icons, data/)

update-awesome-tab:
	$(call download-from-url, $(AWESOME_TAB_URL))

update-company:
	$(call download-from-repo, $(COMPANY_REPO), company)

update-company-box: update-dash
	$(call download-from-repo, $(COMPANY_BOX_REPO), company-box, images/)

update-cua-lite:
	$(call download-from-url, $(CUA_LITE_URL))

update-dash:
	$(call download-from-url, $(DASH_URL))
	$(call download-from-url, $(DASH_FUNCTIONAL_URL))

update-dashboard: update-page-break-lines
	$(call download-from-repo, $(DASHBOARD_REPO), dashboard, banners/)

update-expand-region:
	$(call download-from-repo, $(EXPAND_REGION_REPO), expand-region)

update-f:
	$(call download-from-url, $(F_URL))

update-ht:
	$(call download-from-url, $(HT_URL))

update-ivy-posframe: update-posframe
	$(call download-from-url, $(IVY_POSFRAME_URL))

update-lsp-mode: \
	update-dash \
	update-f \
	update-ht \
	update-lv \
	update-markdown \
	update-s \
	update-spinner \

	$(call download-from-repo, $(LSP_MODE_REPO), lsp-mode, scripts/*)

update-lsp-ui: \
	update-dash \
	update-lsp-mode \
	update-markdown \

	$(call download-from-repo, $(LSP_UI_REPO), lsp-ui)

update-lv:
	$(call download-from-url, $(LV_URL))

update-magit: \
	update-with-editor \
	update-transient \
	update-dash \

	$(call clone-repo, $(MAGIT_REPO), magit)
	make -C $(DIR) versionlib
	$(call update-lib-files, magit, lisp/*)
	$(call clean-repo-dir)

update-markdown:
	$(call download-from-url, $(MARKDOWN_URL))

update-memoize:
	$(call download-from-url, $(MEMOIZE_URL))

update-page-break-lines:
	$(call download-from-url, $(PAGE_BREAK_LINES_URL))

update-perspective:
	$(call download-from-url, $(PERSPECTIVE_URL))

update-persp-projectile: \
	update-perspective \
	update-projectile \

	$(call download-from-url, $(PERSPECTIVE_PROJECTILE_URL))

update-posframe:
	$(call download-from-url, $(POSFRAME_URL))

update-projectile:
	$(call download-from-url, $(PROJECTILE_URL))

update-s:
	$(call download-from-url, $(S_URL))

update-spinner:
	$(call download-from-url, $(SPINNER_URL))

update-simpleclip:
	$(call download-from-url, $(SIMPLECLIP_URL))

update-smart-shift:
	$(call download-from-url, $(SMART_SHIFT_URL))

update-transient:
	$(call download-from-url, $(TRANSIENT_URL))

update-swiper:
	$(call download-from-repo, $(SWIPER_REPO), swiper)

update-undo-tree:
	$(call download-from-url, $(UNDO_TREE_URL))

update-visual-regexp:
	$(call download-from-url, $(VISUAL_REGEXP_URL))

update-which-key:
	$(call download-from-url, $(WHICH_KEY_URL))

update-with-editor:
	$(call download-from-url, $(WITH_EDITOR_URL))

