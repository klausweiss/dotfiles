;; package rpositories
(require 'package) 
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")) 
(setq package-enable-at-startup nil)
(package-initialize)

;; use-package 
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
      (package-install 'use-package)) 
(eval-when-compile
    (require 'use-package))
(setq use-package-always-ensure t)

;; packages
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  )

(setq evilkeys (list
		"SPC" 'helm-M-x
		;; buffers
		"bd" 'kill-this-buffer
		"bl" 'helm-buffers-list
		;; files
		"ff"  'helm-find-files
		"fed" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
		"feR" '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
		;; magit
		"gs" 'magit
		;; projectile
		"pp" 'helm-projectile-switch-project))

(use-package evil-leader
  :config
  (setq evil-leader/leader "SPC")
  (global-evil-leader-mode)
  (apply 'evil-leader/set-key evilkeys))

(use-package evil
  :config
  (evil-mode t)
  )

(use-package evil-magit)

(use-package helm
  :bind (("M-x"     . 'helm-M-x)
	 ("C-x C-f" . 'helm-find-files)
	 :map helm-map
	 ("<tab>"   . 'helm-execute-persistent-action)
	 )
  :config
  (helm-mode t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-etags-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-session-fuzzy-match t)
  ) 

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package projectile) 
(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package which-key
  :config
  (which-key-mode t)
  )

(use-package yasnippet)

;; languages
;; Python

;; themes
(use-package jbeans-theme)
(use-package gruvbox-theme)
(use-package birds-of-paradise-plus-theme)

;; UI
(global-linum-mode t)
(load-theme 'gruvbox t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; keyboard
;; ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
