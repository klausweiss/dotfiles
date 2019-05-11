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

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  )

(use-package helm-company
  :after (:all helm company)
  :bind
  (:map company-mode-map
	("C-<tab>" . helm-company)
	:map company-active-map
	("C-<tab>" . helm-company)))
