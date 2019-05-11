(use-package haskell-mode
  :pin melpa
  :hook
  ('haskell-mode . #'interactive-haskell-mode)
  :bind (:map haskell-mode-map
	      ("C-c C-h" . #'haskell-hoogle-lookup-from-local))
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-tags-on-save t)
  (haskell-stylish-on-save t)
  :hook
  (haskell-mode . (lambda ()
		    (set (make-local-variable 'company-backends)
			 (append '((company-capf company-dabbrev-code))
				 company-backends)))))

(eval-after-load "interactive-haskell-mode"
    '(define-key interactive-haskell-mode-map (kbd "C-c C-h") #'haskell-hoogle-lookup-from-local))
