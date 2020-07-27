(require 'normal-config-file)
(require 'normal-keymap)

(easy-menu-add-item
 normal-global-map
 '(menu-bar file)
 ["Reload configuration" reload-config]
 "Quit"
 )

(provide 'normal-menu)
