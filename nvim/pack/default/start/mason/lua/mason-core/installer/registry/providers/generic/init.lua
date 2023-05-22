local Result = require "mason-core.result"
local _ = require "mason-core.functional"

local M = {}

---@param source GenericDownloadSource | GenericBuildSource
---@param purl Purl
---@param opts PackageInstallOpts
function M.parse(source, purl, opts)
    if source.download then
        source = source --[[@as GenericDownloadSource]]
        return require("mason-core.installer.registry.providers.generic.download").parse(source, purl, opts)
    elseif source.build then
        source = source --[[@as GenericBuildSource]]
        return require("mason-core.installer.registry.providers.generic.build").parse(source, purl, opts)
    else
        return Result.failure "Unknown source type."
    end
end

---@async
---@param ctx InstallContext
---@param source ParsedGenericDownloadSource | ParsedGenericBuildSource
function M.install(ctx, source)
    if source.download then
        source = source --[[@as ParsedGenericDownloadSource]]
        return require("mason-core.installer.registry.providers.generic.download").install(ctx, source)
    elseif source.build then
        source = source --[[@as ParsedGenericBuildSource]]
        return require("mason-core.installer.registry.providers.generic.build").install(ctx, source)
    else
        return Result.failure "Unknown source type."
    end
end

return M
