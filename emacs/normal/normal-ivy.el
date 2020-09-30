(require 'normal-autoload)

(require 'ivy)

(with-eval-after-load 'ivy
  (setq enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) "
	ivy-re-builders-alist '((t . ivy--regex-plus))
	)

  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (ivy-mode 1)
  )

(provide 'normal-ivy)
