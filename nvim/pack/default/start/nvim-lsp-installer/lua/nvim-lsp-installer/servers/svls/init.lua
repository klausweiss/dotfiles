local server = require "nvim-lsp-installer.server"
local cargo = require "nvim-lsp-installer.installers.cargo"

return function(name, root_dir)
    return server.Server:new {
        name = name,
        root_dir = root_dir,
        languages = { "systemverilog" },
        homepage = "https://github.com/dalance/svls",
        installer = cargo.crate "svls",
        default_options = {
            cmd_env = cargo.env(root_dir),
        },
    }
end
