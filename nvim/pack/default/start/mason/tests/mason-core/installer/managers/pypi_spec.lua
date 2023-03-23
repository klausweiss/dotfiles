local installer = require "mason-core.installer"
local path = require "mason-core.path"
local pypi = require "mason-core.installer.managers.pypi"
local stub = require "luassert.stub"

---@param ctx InstallContext
local function venv_py(ctx)
    return path.concat {
        ctx.cwd:get(),
        "venv",
        "bin",
        "python",
    }
end

describe("pypi manager", function()
    it("should init venv without upgrading pip", function()
        local ctx = create_dummy_context()
        stub(ctx, "promote_cwd")
        installer.exec_in_context(ctx, function()
            pypi.init { upgrade_pip = false }
        end)

        assert.spy(ctx.promote_cwd).was_called(1)
        assert.spy(ctx.spawn.python3).was_called(1)
        assert.spy(ctx.spawn.python3).was_called_with {
            "-m",
            "venv",
            "venv",
        }
    end)

    it("should init venv and upgrade pip", function()
        local ctx = create_dummy_context()
        stub(ctx, "promote_cwd")
        installer.exec_in_context(ctx, function()
            pypi.init { upgrade_pip = true, install_extra_args = { "--proxy", "http://localhost" } }
        end)

        assert.spy(ctx.promote_cwd).was_called(1)
        assert.spy(ctx.spawn.python3).was_called(1)
        assert.spy(ctx.spawn.python3).was_called_with {
            "-m",
            "venv",
            "venv",
        }
        assert.spy(ctx.spawn[venv_py(ctx)]).was_called(1)
        assert.spy(ctx.spawn[venv_py(ctx)]).was_called_with {
            "-m",
            "pip",
            "--disable-pip-version-check",
            "install",
            "-U",
            { "--proxy", "http://localhost" },
            { "pip" },
        }
    end)

    it("should install", function()
        local ctx = create_dummy_context()
        installer.exec_in_context(ctx, function()
            pypi.install("pypi-package", "1.0.0")
        end)

        assert.spy(ctx.spawn[venv_py(ctx)]).was_called(1)
        assert.spy(ctx.spawn[venv_py(ctx)]).was_called_with {
            "-m",
            "pip",
            "--disable-pip-version-check",
            "install",
            "-U",
            vim.NIL, -- install_extra_args
            {
                "pypi-package==1.0.0",
                vim.NIL, -- extra_packages
            },
        }
    end)

    it("should install extra specifier", function()
        local ctx = create_dummy_context()
        installer.exec_in_context(ctx, function()
            pypi.install("pypi-package", "1.0.0", {
                extra = "lsp",
            })
        end)

        assert.spy(ctx.spawn[venv_py(ctx)]).was_called(1)
        assert.spy(ctx.spawn[venv_py(ctx)]).was_called_with {
            "-m",
            "pip",
            "--disable-pip-version-check",
            "install",
            "-U",
            vim.NIL, -- install_extra_args
            {
                "pypi-package[lsp]==1.0.0",
                vim.NIL, -- extra_packages
            },
        }
    end)

    it("should install extra packages", function()
        local ctx = create_dummy_context()
        installer.exec_in_context(ctx, function()
            pypi.install("pypi-package", "1.0.0", {
                extra_packages = { "extra-package" },
                install_extra_args = { "--proxy", "http://localhost:9000" },
            })
        end)

        assert.spy(ctx.spawn[venv_py(ctx)]).was_called(1)
        assert.spy(ctx.spawn[venv_py(ctx)]).was_called_with {
            "-m",
            "pip",
            "--disable-pip-version-check",
            "install",
            "-U",
            { "--proxy", "http://localhost:9000" },
            {
                "pypi-package==1.0.0",
                { "extra-package" },
            },
        }
    end)
end)
