(require 'easymenu)
(require 'normal-autoload)
(require 'normal-buffer-interaction)
(require 'normal-emacs-interaction)
(require 'normal-interface)
(require 'normal-project-integration)
(require 'normal-seq)
(require 'normal-text-manipulation)
(require 'normal-window-movement)

(autoload-all "../lib/swiper/counsel"
	      #'counsel-M-x
	      #'counsel-yank-pop
	      )
(autoload-all "../lib/expand-region/expand-region"
	      #'er/expand-region
	      #'er/contract-region
	      )
(autoload-all "../lib/lsp-mode/lsp"
	      #'lsp-rename
	      #'lsp-execute-code-action
	      )
(autoload-all "../lib/lsp-ui/lsp-ui"
	      #'lsp-ui-peek-find-references
	      #'lsp-ui-peek-find-definitions
	      #'lsp-ui-peek-find-implementations
	      )
(autoload-all "../lib/magit/magit"
	      #'magit-status
	      )
(autoload-all "../lib/projectile"
	      #'projectile-add-known-project
	      #'projectile-dired
	      #'projectile-remove-known-project
	      #'projectile-switch-project
	      #'projectile-switch-to-buffer
	      )
(autoload-all "../lib/perspective"
	      #'persp-switch
	      #'persp-switch-last
	      )
(autoload-all "../lib/smart-shift"
	      #'smart-shift-up
	      #'smart-shift-down
	      #'smart-shift-left
	      )
(autoload-all "../lib/undo-tree"
	      #'undo-tree-redo
	      #'undo-tree-undo
	      #'undo-tree-visualize
	      )

(defvar normal-global-map
  (let ((map (make-sparse-keymap))
	(menu-bar (seq-find-by-first-val 'menu-bar global-map))
	(tool-bar (seq-find-by-first-val 'tool-bar global-map)))
    (define-key map (kbd "C-S-o") #'find-file)
    (define-key map (kbd "C-o") #'projectile-find-file-if-in-project)
    (define-key map (kbd "C-s") #'save-buffer)
    (define-key map (kbd "C-S-s") #'write-file)
    (define-key map (kbd "C-z") #'undo-tree-undo)
    (define-key map (kbd "C-v") #'clipboard-yank)
    (define-key map (kbd "C-S-v") #'counsel-yank-pop)
    (define-key map (kbd "C-y") #'undo-tree-redo)
    (define-key map (kbd "C-S-y") #'undo-tree-visualize)
    (define-key map (kbd "C-f") #'swiper-isearch-with-current-region)
    (define-key map (kbd "C-r") #'vr/replace-in-buffer)
    (define-key map (kbd "C-a") #'mark-whole-buffer)
    (define-key map (kbd "C-w") #'kill-buffer-and-window)
    (define-key map (kbd "C-d") #'duplicate-current-line)
    (define-key map (kbd "C-k") #'kill-whole-line)
    (define-key map (kbd "C-<backspace>") #'backward-kill-word)
    (define-key map (kbd "<C-delete>") #'kill-word)
    (define-key map (kbd "C-<return>") #'newline-above)
    (define-key map (kbd "S-<return>") #'newline-below)
    (define-key map (kbd "C-S-a") #'counsel-M-x)
    (define-key map (kbd "C-`") #'eval-expression)
    (define-key map (kbd "C-<tab>") #'awesome-tab-forward-tab)
    (define-key map (kbd "<C-iso-lefttab>") #'awesome-tab-backward-tab)
    (define-key map (kbd "C-/") #'comment-dwim-line)
    (define-key map (kbd "C-e") #'er/expand-region)
    (define-key map (kbd "C-S-e") #'er/contract-region)
    (define-key map (kbd "C-S-<up>") #'smart-shift-up)
    (define-key map (kbd "C-S-<down>") #'smart-shift-down)
    (define-key map (kbd "C-S-g") #'magit-status)
    (define-key map (kbd "C-p C-p") #'projectile-switch-project)
    (define-key map (kbd "C-p C-a") #'projectile-add-known-project)
    (define-key map (kbd "C-p C-d") #'projectile-dired)
    (define-key map (kbd "C-p x") #'projectile-remove-known-project)
    (define-key map (kbd "C-p C-s") #'persp-switch)
    (define-key map (kbd "C-p TAB") #'persp-switch-last)
    (define-key map (kbd "C-p C-b") #'projectile-switch-to-buffer)
    (define-key map (kbd "C-p b") #'persp-counsel-switch-buffer-force)
    (define-key map (kbd "C-' t") #'toggle-theme)
    (define-key map (kbd "<f2>") #'flymake-goto-next-error)
    (define-key map (kbd "S-<f2>") #'flymake-goto-prev-error)
    (define-key map (kbd "C-b") #'lsp-ui-peek-find-references)
    (define-key map (kbd "C-S-b") #'lsp-ui-peek-find-definitions)
    (define-key map (kbd "C-M-b") #'lsp-ui-peek-find-implementation)
    (define-key map (kbd "S-<f6>") #'lsp-rename)
    (define-key map (kbd "M-RET") #'lsp-execute-code-action)
    (define-key map (kbd "C-M-s") #'switch-to-or-split-window-right)
    (define-key map (kbd "C-M-n") #'switch-to-or-split-window-down)
    (define-key map (kbd "C-M-t") #'switch-to-or-split-window-left)
    (define-key map (kbd "C-M-p") #'switch-to-or-split-window-up)

    (define-key map (kbd "C-? ?") #'help-for-help)
    (define-key map (kbd "C-? a") #'apropos-command)
    (define-key map (kbd "C-? b") #'describe-bindings)
    (define-key map (kbd "C-? c") #'describe-key-briefly)
    (define-key map (kbd "C-? C") #'describe-coding-system)
    (define-key map (kbd "C-? d") #'apropos-documentation)
    (define-key map (kbd "C-? e") #'view-echo-area-messages)
    (define-key map (kbd "C-? f") #'describe-function)
    (define-key map (kbd "C-? F") #'Info-goto-emacs-command-node)
    (define-key map (kbd "C-? g") #'describe-gnu-project)
    (define-key map (kbd "C-? h") #'view-hello-file)
    (define-key map (kbd "C-? i") #'info)
    (define-key map (kbd "C-? I") #'describe-input-method)
    (define-key map (kbd "C-? k") #'describe-key)
    (define-key map (kbd "C-? K") #'Info-goto-emacs-key-command-node)
    (define-key map (kbd "C-? l") #'find-library)
    (define-key map (kbd "C-? L") #'describe-language-environment)
    (define-key map (kbd "C-? m") #'describe-mode)
    (define-key map (kbd "C-? n") #'view-emacs-news)
    (define-key map (kbd "C-? o") #'describe-symbol)
    (define-key map (kbd "C-? p") #'finder-by-keyword)
    (define-key map (kbd "C-? P") #'describe-package)
    (define-key map (kbd "C-? r") #'info-emacs-manual)
    (define-key map (kbd "C-? s") #'describe-syntax)
    (define-key map (kbd "C-? S") #'info-lookup-symbol)
    (define-key map (kbd "C-? t") #'help-with-tutorial)
    (define-key map (kbd "C-? v") #'describe-variable)
    (define-key map (kbd "C-? w") #'where-is)
    (define-key map (kbd "C-? .") #'display-local-help)
    (define-key map (kbd "C-? C-a") #'about-emacs)
    (define-key map (kbd "C-? C-c") #'describe-copying)
    (define-key map (kbd "C-? C-d") #'view-emacs-debugging)
    (define-key map (kbd "C-? C-e") #'view-external-packages)
    (define-key map (kbd "C-? C-f") #'view-emacs-FAQ)
    (define-key map (kbd "C-? C-m") #'view-order-manuals)
    (define-key map (kbd "C-? C-n") #'view-emacs-news)
    (define-key map (kbd "C-? C-o") #'describe-distribution)
    (define-key map (kbd "C-? C-p") #'view-emacs-problems)
    (define-key map (kbd "C-? C-t") #'view-emacs-todo)
    (define-key map (kbd "C-? C-w") #'describe-no-warranty)

    (define-key map (kbd "C-0") #'reset-font-size)
    (define-key map (kbd "<C-mouse-4>") #'increase-font-size)
    (define-key map (kbd "C-=") #'increase-font-size)
    (define-key map (kbd "<C-mouse-5>") #'decrease-font-size)
    (define-key map (kbd "C--") #'decrease-font-size)

    (define-key map [mouse-1] #'mouse-set-point)
    (define-key map [mouse-2] #'mouse-yank-primary)
    (define-key map [mouse-3] #'mouse-save-then-kill)
    (define-key map [mouse-4] #'mwheel-scroll)
    (define-key map [mouse-5] #'mwheel-scroll)
    (define-key map [down-mouse-1] #'mouse-drag-region)
    (define-key map [drag-mouse-1] #'mouse-set-region)
    (define-key map [vertical-scroll-bar mouse-1] #'scroll-bar-toolkit-scroll)
    (define-key map [horizontal-scroll-bar mouse-1] #'scroll-bar-toolkit-horizontal-scroll)

    (define-key map [escape] #'keyboard-quit)
    (define-key map (kbd "TAB") #'shift-right/complete/insert-tab)
    (define-key map (kbd "<backtab>") #'smart-shift-left)
    (define-key map (kbd "<left>") #'left-char)
    (define-key map (kbd "<right>") #'right-char)
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<home>") #'beginning-of-line)
    (define-key map (kbd "C-<home>") #'beginning-of-buffer)
    (define-key map (kbd "C-<end>") #'end-of-buffer)
    (define-key map (kbd "<end>") #'end-of-line)
    (define-key map (kbd "<next>") #'scroll-up-command)
    (define-key map (kbd "<prior>") #'scroll-down-command)
    (define-key map (kbd "C-<right>") #'right-word)
    (define-key map (kbd "C-<left>") #'left-word)
    (define-key map (kbd "C-<up>") #'backward-paragraph)
    (define-key map (kbd "C-<down>") #'forward-paragraph)
    (define-key map (kbd "<deletechar>") #'delete-char)
    (define-key map (kbd "DEL") #'backward-delete-char)
    (define-key map (kbd "RET") #'newline)
    (substitute-key-definition
     #'self-insert-command #'self-insert-command
     map global-map)
    (append map (list menu-bar tool-bar))
    )
  )

(use-global-map normal-global-map)
(cua-mode t)

;; https://stackoverflow.com/a/10166400/3103257
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'normal-keymap)
