;; moving between windows
(global-set-key (kbd "C-S-i") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-k") 'windmove-down)
(global-set-key (kbd "C-S-j") 'windmove-left)

;; manipulating windows
(global-set-key (kbd "C-+") 'split-window-below)
(global-set-key (kbd "C-|") 'split-window-right)
(global-set-key (kbd "C-)") 'delete-window)
