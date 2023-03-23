local Purl = require "mason-core.purl"
local Result = require "mason-core.result"
local golang = require "mason-core.installer.registry.providers.golang"
local installer = require "mason-core.installer"
local stub = require "luassert.stub"

---@param overrides Purl
local function purl(overrides)
    local purl = Purl.parse("pkg:golang/namespace/package@v1.5.0"):get_or_throw()
    if not overrides then
        return purl
    end
    return vim.tbl_deep_extend("force", purl, overrides)
end

describe("golang provider :: parsing", function()
    it("should parse package", function()
        assert.same(
            Result.success {
                package = "namespace/package",
                version = "v1.5.0",
                extra_packages = { "extra" },
            },
            golang.parse({ extra_packages = { "extra" } }, purl())
        )
    end)
end)

describe("golang provider :: installing", function()
    it("should install golang packages", function()
        local ctx = create_dummy_context()
        local manager = require "mason-core.installer.managers.golang"
        stub(manager, "install", mockx.returns(Result.success()))

        local result = installer.exec_in_context(ctx, function()
            return golang.install(ctx, {
                package = "namespace/package",
                version = "v1.5.0",
                extra_packages = { "extra" },
            })
        end)

        assert.is_true(result:is_success())
        assert.spy(manager.install).was_called(1)
        assert.spy(manager.install).was_called_with("namespace/package", "v1.5.0", { extra_packages = { "extra" } })
    end)

    it("should ensure valid version", function()
        local ctx = create_dummy_context {
            version = "1.10.0",
        }
        local manager = require "mason-core.installer.managers.golang"
        local providers = require "mason-core.providers"
        stub(providers.golang, "get_all_versions", mockx.returns(Result.success { "1.0.0" }))
        stub(manager, "install", mockx.returns(Result.success()))

        local result = installer.exec_in_context(ctx, function()
            return golang.install(ctx, {
                package = "package",
                version = "1.10.0",
            })
        end)

        assert.is_true(result:is_failure())
        assert.same(Result.failure [[Version "1.10.0" is not available.]], result)
        assert.spy(manager.install).was_called(0)
    end)
end)
