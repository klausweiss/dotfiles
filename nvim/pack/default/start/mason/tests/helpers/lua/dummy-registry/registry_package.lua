local Pkg = require "mason-core.package"

return Pkg.new {
    schema = "registry+v1",
    name = "registry",
    description = [[This is a dummy package.]],
    homepage = "https://example.com",
    licenses = { "MIT" },
    languages = { "DummyLang" },
    categories = { "LSP" },
    source = {
        id = "pkg:dummy/registry@1.0.0",
    },
}
