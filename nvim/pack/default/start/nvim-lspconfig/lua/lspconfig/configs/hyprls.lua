local util = require 'lspconfig.util'

return {
  default_config = {
    cmd = { 'hyprls', '--stdio' },
    filetypes = { 'hyprlang' },
    root_dir = util.find_git_ancestor,
    single_file_support = true,
  },
  docs = {
    description = [[
https://github.com/hyprland-community/hyprls

`hyprls` can be installed via `go`:
```sh
go install github.com/ewen-lbh/hyprls/cmd/hyprls@latest
```

]],
  },
}
