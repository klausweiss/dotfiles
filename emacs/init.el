;; package repositories
(require 'package) 
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; use-package 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

;; packages
(load "~/.emacs.d/lib/load-directory.el")
(load-directory "~/.emacs.d/lib/" :recursive 't)
(load-directory "~/.emacs.d/packages/" :recursive 't)

;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'no-error)
