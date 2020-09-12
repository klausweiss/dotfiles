(require 'normal-autoload)

(require 'ivy)

(with-eval-after-load 'ivy
  (require 'ivy-posframe)
  (setq enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) "
	ivy-re-builders-alist '((t . ivy--regex-plus))
	)

  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (ivy-mode 1)
  )

(with-eval-after-load 'ivy-posframe
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
	ivy-posframe-parameters '((internal-border-width . 1))
	)
  (ivy-posframe-mode 1)
  )

(provide 'normal-ivy)
