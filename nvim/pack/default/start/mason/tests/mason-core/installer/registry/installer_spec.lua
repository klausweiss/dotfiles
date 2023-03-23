local Result = require "mason-core.result"
local installer = require "mason-core.installer.registry"
local match = require "luassert.match"
local stub = require "luassert.stub"
local util = require "mason-core.installer.registry.util"

---@type InstallerProvider
local dummy_provider = {
    ---@param source RegistryPackageSource
    ---@param purl Purl
    ---@param opts PackageInstallOpts
    parse = function(source, purl, opts)
        return Result.try(function(try)
            if source.supported_platforms then
                try(util.ensure_valid_platform(source.supported_platforms))
            end
            return {
                package = purl.name,
                extra_info = source.extra_info,
                should_fail = source.should_fail,
            }
        end)
    end,
    install = function(ctx, source)
        if source.should_fail then
            return Result.failure "This is a failure."
        else
            return Result.success()
        end
    end,
}

describe("registry installer :: parsing", function()
    it("should parse valid package specs", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.parse({
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
                extra_info = "here",
            },
        }, {})
        local parsed = result:get_or_nil()

        assert.is_true(result:is_success())
        assert.is_true(match.is_ref(dummy_provider)(parsed.provider))
        assert.same({
            name = "package-name",
            scheme = "pkg",
            type = "dummy",
            version = "v1.2.3",
        }, parsed.purl)
        assert.same({
            package = "package-name",
            extra_info = "here",
        }, parsed.source)
    end)

    it("should reject incompatible schema versions", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.parse({
            schema = "registry+v1337",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
            },
        }, {})
        assert.same(
            Result.failure [[Current version of mason.nvim is not capable of parsing package schema version "registry+v1337".]],
            result
        )
    end)

    it("should use requested version", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.parse({
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
            },
        }, { version = "v2.0.0" })

        assert.is_true(result:is_success())
        local parsed = result:get_or_nil()

        assert.same({
            name = "package-name",
            scheme = "pkg",
            type = "dummy",
            version = "v2.0.0",
        }, parsed.purl)
    end)

    it("should handle PLATFORM_UNSUPPORTED", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.compile({
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
                supported_platforms = { "VIC64" },
            },
        }, { version = "v2.0.0" })

        assert.same(Result.failure "The current platform is unsupported.", result)
    end)

    it("should error upon parsing failures", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.compile({
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
                supported_platforms = { "VIC64" },
            },
        }, { version = "v2.0.0" })

        assert.same(Result.failure "The current platform is unsupported.", result)
    end)
end)

describe("registry installer :: compiling", function()
    it("should run compiled installer function successfully", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.compile({
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
            },
        }, {})

        assert.is_true(result:is_success())
        local installer_fn = result:get_or_nil()

        local ctx = create_dummy_context()
        local installer_result = require("mason-core.installer").exec_in_context(ctx, installer_fn)
        assert.same(Result.success(), installer_result)
    end)

    it("should raise errors upon installer failures", function()
        installer.register_provider("dummy", dummy_provider)

        local result = installer.compile({
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
                should_fail = true,
            },
        }, {})

        assert.is_true(result:is_success())
        local installer_fn = result:get_or_nil()

        local ctx = create_dummy_context()
        local err = assert.has_error(function()
            require("mason-core.installer").exec_in_context(ctx, installer_fn)
        end)
        assert.equals("This is a failure.", err)
    end)

    it("should register links", function()
        installer.register_provider("dummy", dummy_provider)
        local link = require "mason-core.installer.registry.link"
        stub(link, "bin", mockx.returns(Result.success()))
        stub(link, "share", mockx.returns(Result.success()))
        stub(link, "opt", mockx.returns(Result.success()))

        local spec = {
            schema = "registry+v1",
            source = {
                id = "pkg:dummy/package-name@v1.2.3",
            },
            bin = { ["exec"] = "exec" },
            opt = { ["opt/"] = "opt/" },
            share = { ["share/"] = "share/" },
        }

        local result = installer.compile(spec, {})

        assert.is_true(result:is_success())
        local installer_fn = result:get_or_nil()

        local ctx = create_dummy_context()
        local installer_result = require("mason-core.installer").exec_in_context(ctx, installer_fn)
        assert.is_true(installer_result:is_success())

        for _, spy in ipairs { link.bin, link.share, link.opt } do
            assert.spy(spy).was_called(1)
            assert.spy(spy).was_called_with(match.is_ref(ctx), spec, {
                scheme = "pkg",
                type = "dummy",
                name = "package-name",
                version = "v1.2.3",
            }, {
                package = "package-name",
            })
        end
    end)
end)
