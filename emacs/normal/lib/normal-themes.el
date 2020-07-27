(defvar dark-theme 'wombat)
(defvar light-theme 'tango)
(defvar current-theme light-theme)

(defun load-dark-theme ()
  (interactive)
  (disable-theme light-theme)
  (setq current-theme dark-theme)
  (load-theme dark-theme t))

(defun load-light-theme ()
  (interactive)
  (disable-theme dark-theme)
  (setq current-theme light-theme)
  (load-theme light-theme t))

(defun toggle-theme ()
  (interactive)
  (if (eq current-theme light-theme)
      (load-dark-theme)
    (load-light-theme)))

(provide 'normal-themes)
