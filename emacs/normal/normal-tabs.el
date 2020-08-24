(require 'awesome-tab)
(require 'normal-autoload)

(autoload-all "lib/normal-tabs-lib"
	      #'normal-awesome-tab-buffer-groups
	      )

(with-eval-after-load 'awesome-tab
  (setq awesome-tab-height 120)
  (awesome-tab-mode t)

  (setq awesome-tab-buffer-groups-function #'normal-awesome-tab-buffer-groups)
  )

(provide 'normal-tabs)
