local a = require "mason-core.async"
local Optional = require "mason-core.optional"
local notify = require "mason-core.notify"
local _ = require "mason-core.functional"

---@async
---@param user_args string[]: The arguments, as provided by the user.
local function parse_packages_from_user_args(user_args)
    local registry = require "mason-registry"
    local Package = require "mason-core.package"
    local server_mapping = require "mason-lspconfig.mappings.server"
    local language_mapping = require "mason.mappings.language"

    return _.filter_map(function(server_specifier)
        local server_name, version = Package.Parse(server_specifier)
        -- 1. first see if the provided arg is an actual lspconfig server name
        return Optional
            .of_nilable(server_mapping.lspconfig_to_package[server_name])
            -- 2. if not, check if it's a language specifier (e.g., "typescript" or "java")
            :or_(function()
                return Optional.of_nilable(language_mapping[server_name]):map(function(package_names)
                    local package_names = _.filter(function(package_name)
                        return server_mapping.package_to_lspconfig[package_name] ~= nil
                    end, package_names)

                    if #package_names == 0 then
                        return nil
                    end

                    return a.promisify(vim.ui.select)(package_names, {
                        prompt = ("Please select which server you want to install for language %q:"):format(
                            server_name
                        ),
                        format_item = function(package_name)
                            local server_name = server_mapping.package_to_lspconfig[package_name]
                            if registry.is_installed(package_name) then
                                return ("%s (installed)"):format(server_name)
                            else
                                return server_name
                            end
                        end,
                    })
                end)
            end)
            :map(function(package_name)
                return { package = package_name, version = version }
            end)
            :if_not_present(function()
                notify(("Could not find LSP server %q."):format(server_name), vim.log.levels.ERROR)
            end)
    end, user_args)
end

---@async
local function parse_packages_from_heuristics()
    local server_mapping = require "mason-lspconfig.mappings.server"
    local registry = require "mason-registry"

    -- Prompt user which server they want to install (based on the current filetype)
    local current_ft = vim.api.nvim_buf_get_option(vim.api.nvim_get_current_buf(), "filetype")
    local filetype_mapping = require "mason-lspconfig.mappings.filetype"
    return Optional.of_nilable(filetype_mapping[current_ft])
        :map(function(server_names)
            return a.promisify(vim.ui.select)(server_names, {
                prompt = ("Please select which server you want to install for filetype %q:"):format(current_ft),
                format_item = function(server_name)
                    if registry.is_installed(server_mapping.lspconfig_to_package[server_name]) then
                        return ("%s (installed)"):format(server_name)
                    else
                        return server_name
                    end
                end,
            })
        end)
        :map(function(server_name)
            local package_name = server_mapping.lspconfig_to_package[server_name]
            return { { package = package_name, version = nil } }
        end)
        :or_else_get(function()
            notify(("No LSP servers found for filetype %q."):format(current_ft), vim.log.levels.ERROR)
            return {}
        end)
end

local parse_packages_to_install = _.cond {
    { _.compose(_.gt(0), _.length), parse_packages_from_user_args },
    { _.compose(_.equals(0), _.length), parse_packages_from_heuristics },
    { _.T, _.always {} },
}

local LspInstall = a.scope(function(servers)
    local packages_to_install = parse_packages_to_install(servers)
    require("mason.api.command").MasonInstall(_.map(function(target)
        if target.version then
            return ("%s@%s"):format(target.package, target.version)
        else
            return target.package
        end
    end, packages_to_install))
    local ui = require "mason.ui"
    ui.set_view "LSP"
end)

vim.api.nvim_create_user_command("LspInstall", function(opts)
    LspInstall(opts.fargs)
end, {
    desc = "Install one or more LSP servers.",
    nargs = "*",
    complete = "custom,v:lua.mason_lspconfig_completion.available_server_completion",
})

local function LspUninstall(servers)
    local server_mapping = require "mason-lspconfig.mappings.server"
    require("mason.api.command").MasonUninstall(_.map(function(lspconfig_name)
        return server_mapping.lspconfig_to_package[lspconfig_name] or lspconfig_name
    end, servers))
    require("mason.ui").set_view "LSP"
end

vim.api.nvim_create_user_command("LspUninstall", function(opts)
    LspUninstall(opts.fargs)
end, {
    desc = "Uninstall one or more LSP servers.",
    nargs = "+",
    complete = "custom,v:lua.mason_lspconfig_completion.installed_server_completion",
})

-- selene: allow(global_usage)
_G.mason_lspconfig_completion = {
    available_server_completion = function()
        local available_servers = require("mason-lspconfig").get_available_servers()
        local language_mapping = require "mason.mappings.language"
        local sort_deduped = _.compose(_.sort_by(_.identity), _.uniq_by(_.identity))
        local completions = sort_deduped(_.concat(_.keys(language_mapping), available_servers))
        return table.concat(completions, "\n")
    end,
    installed_server_completion = function()
        local installed_servers = require("mason-lspconfig").get_installed_servers()
        return table.concat(installed_servers, "\n")
    end,
}

return {
    LspInstall = LspInstall,
    LspUninstall = LspUninstall,
}
