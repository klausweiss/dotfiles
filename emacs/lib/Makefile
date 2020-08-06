SHELL := /bin/bash

AWESOME_TAB_URL := https://raw.githubusercontent.com/manateelazycat/awesome-tab/master/awesome-tab.el
COMPANY_REPO := https://github.com/company-mode/company-mode.git
COMPANY_BOX_REPO := https://github.com/sebastiencs/company-box.git
	DASH_FUNCTIONAL_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash-functional.el
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
SMART_SHIFT_URL := https://raw.githubusercontent.com/klausweiss/smart-shift/master/smart-shift.el
SWIPER_REPO := https://github.com/abo-abo/swiper.git
UNDO_TREE_URL := http://www.dr-qubit.org/undo-tree/undo-tree.el
VISUAL_REGEXP_URL := https://raw.githubusercontent.com/benma/visual-regexp.el/master/visual-regexp.el
WHICH_KEY_URL := https://raw.githubusercontent.com/justbur/emacs-which-key/master/which-key.el


.PHONY: clean \
	compile \
	update \
	update-awesome-tab \
	update-company \
	update-company-box \
	update-dash \
	update-dashboard \
	update-expand-region \
	update-f \
	update-ht \
	update-ivy-posframe \
	update-lsp-mode \
	update-lsp-ui \
	update-lv \
	update-magit \
	update-markdown \
	update-page-break-lines \
	update-perspective \
	update-persp-projectile \
	update-posframe \
	update-projectile \
	update-s \
	update-smart-shift \
	update-spinner \
	update-swiper \
	update-transient \
	update-undo-tree \
	update-visual-regexp \
	update-which-key \
	update-with-editor \

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

update: update-awesome-tab \
	update-company \
	update-company-box \
	update-dashboard \
	update-expand-region \
	update-ivy-posframe \
	update-lsp-mode \
	update-lsp-ui \
	update-magit \
	update-perspective \
	update-persp-projectile \
	update-posframe \
	update-projectile \
	update-smart-shift \
	update-swiper \
	update-undo-tree \
	update-visual-regexp \
	update-which-key \

define download-from-url
	wget -N $(1)
endef

define clone-repo
	$(eval DIR = $(shell mktemp --directory --suffix $(2)))
	git clone $(1) $(DIR) --depth 1
endef

define update-lib-files
	rm -rf $(1)/
	mkdir $(1)
	cp -r $(DIR)/{*.el,README*} $(1)/
	rm -rf $(DIR)
endef

define download-from-repo
	$(call clone-repo, $(1), $(2))
	$(call update-lib-files, $(2))
endef


update-awesome-tab:
	$(call download-from-url, $(AWESOME_TAB_URL))

update-company:
	$(call download-from-repo, $(COMPANY_REPO), company)

update-company-box: update-dash
	$(call clone-repo, $(COMPANY_BOX_REPO), company-box)
	rm -rf company-box/
	mkdir company-box
	cp -r $(DIR)/{*.el,README*,images} company-box/
	rm -rf $(DIR)

update-dash:
	$(call download-from-url, $(DASH_URL))
	$(call download-from-url, $(DASH_FUNCTIONAL_URL))

update-dashboard: update-page-break-lines
	$(call clone-repo, $(DASHBOARD_REPO), dashboard)
	rm -rf dashboard/
	mkdir dashboard
	cp -r $(DIR)/{*.el,README*,banners} dashboard/
	rm -rf $(DIR)

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

	$(call clone-repo, $(LSP_MODE_REPO), lsp-mode)
	rm -rf lsp-mode/
	mkdir lsp-mode
	cp -r $(DIR)/{*.el,README*,scripts/*} lsp-mode/
	rm -rf $(DIR)

update-lsp-ui: \
	update-dash \
	update-lsp-mode \
	update-markdown \

	$(call download-from-repo, $(LSP_UI_REPO), lsp-ui)

update-lv:
	$(call download-from-url, $(LV_URL))

update-magit: update-with-editor \
	update-transient \
	update-dash \

	$(call clone-repo, $(MAGIT_REPO), magit)
	make -C $(DIR) versionlib
	rm -rf magit/
	cp -r $(DIR)/lisp magit
	rm -rf $(DIR)

update-markdown:
	$(call download-from-url, $(MARKDOWN_URL))

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

