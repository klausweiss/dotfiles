local _ = require "mason-core.functional"

describe("functional: string", function()
    it("matches string patterns", function()
        assert.is_true(_.matches("foo", "foo"))
        assert.is_true(_.matches("bar", "foobarbaz"))
        assert.is_true(_.matches("ba+r", "foobaaaaaaarbaz"))

        assert.is_false(_.matches("ba+r", "foobharbaz"))
        assert.is_false(_.matches("bar", "foobaz"))
    end)

    it("returns string pattern matches", function()
        assert.same({ "foo" }, _.match("foo", "foo"))
        assert.same({ "foo", "bar", "baz" }, _.match("(foo) (bar) (baz)", "foo bar baz"))
    end)

    it("should format strings", function()
        assert.equals("Hello World!", _.format("%s", "Hello World!"))
        assert.equals("special manouvers", _.format("%s manouvers", "special"))
    end)

    it("should split strings", function()
        assert.same({ "This", "is", "a", "sentence" }, _.split("%s", "This is a sentence"))
        assert.same({ "This", "is", "a", "sentence" }, _.split("|", "This|is|a|sentence"))
    end)

    it("should gsub strings", function()
        assert.same("predator", _.gsub("^apex%s*", "", "apex predator"))
    end)

    it("should dedent strings", function()
        assert.equals(
            [[Lorem
Ipsum
    Dolor
  Sit
 Amet]],
            _.dedent [[
    Lorem
    Ipsum
        Dolor
      Sit
     Amet
]]
        )
    end)

    it("should transform casing", function()
        assert.equals("HELLO!", _.to_upper "Hello!")
        assert.equals("hello!", _.to_lower "Hello!")
    end)

    it("trim strings", function()
        assert.equals("HELLO!", _.trim "   HELLO!  ")
    end)

    it("should trim_start strings", function()
        assert.equals("HELLO!  ", _.trim_start_matches("%s", "	   HELLO!  "))
    end)

    it("should trim_end strings", function()
        assert.equals("	   HELLO!", _.trim_end_matches("%s", "	   HELLO!  "))
    end)

    it("should strip_prefix", function()
        assert.equals("withthewind", _.strip_prefix("gone", "gonewiththewind"))
        assert.equals("1.3.0", _.strip_prefix("v", "v1.3.0"))
    end)

    it("should strip_suffix", function()
        assert.equals("gone", _.strip_suffix("withtthewind", "gonewithtthewind"))
        assert.equals("name", _.strip_suffix("%.tar%.gz", "name.tar.gz"))
        assert.equals("name", _.strip_suffix(".tar.*", "name.tar.gz"))
    end)
end)
