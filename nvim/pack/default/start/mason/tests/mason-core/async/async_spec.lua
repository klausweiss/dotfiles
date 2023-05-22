local _ = require "mason-core.functional"
local a = require "mason-core.async"
local assert = require "luassert"
local control = require "mason-core.async.control"
local match = require "luassert.match"
local process = require "mason-core.process"
local spy = require "luassert.spy"

local function timestamp()
    local seconds, microseconds = vim.loop.gettimeofday()
    return (seconds * 1000) + math.floor(microseconds / 1000)
end

describe("async", function()
    it("should run in blocking mode", function()
        local start = timestamp()
        a.run_blocking(function()
            a.sleep(100)
        end)
        local stop = timestamp()
        local grace_ms = 50
        assert.is_true((stop - start) >= (100 - grace_ms))
    end)

    it("should return values in blocking mode", function()
        local function slow_maths(arg1, arg2)
            a.sleep(10)
            return arg1 + arg2 - 42
        end
        local value = a.run_blocking(slow_maths, 13, 37)
        assert.equals(8, value)
    end)

    it(
        "should pass arguments to .run",
        async_test(function()
            local callback = spy.new()
            local start = timestamp()
            a.run(a.sleep, callback, 100)
            assert.wait_for(function()
                assert.spy(callback).was_called(1)
                local stop = timestamp()
                local grace_ms = 25
                assert.is_true((stop - start) >= (100 - grace_ms))
            end, 150)
        end)
    )

    it(
        "should wrap callback-style async functions",
        async_test(function()
            local stdio = process.in_memory_sink()
            local success, exit_code = a.promisify(process.spawn)("env", {
                args = {},
                env = { "FOO=BAR", "BAR=BAZ" },
                stdio_sink = stdio.sink,
            })
            assert.is_true(success)
            assert.equals(0, exit_code)
            assert.equals("FOO=BAR\nBAR=BAZ\n", table.concat(stdio.buffers.stdout, ""))
        end)
    )

    it(
        "should reject callback-style functions",
        async_test(function()
            local err = assert.has_error(function()
                a.promisify(function(arg1, cb)
                    cb(arg1, nil)
                end, true) "påskmust"
            end)
            assert.equals(err, "påskmust")
        end)
    )

    it(
        "should return all values",
        async_test(function()
            local val1, val2, val3 = a.wait(function(resolve)
                resolve(1, 2, 3)
            end)
            assert.equals(1, val1)
            assert.equals(2, val2)
            assert.equals(3, val3)
        end)
    )

    it(
        "should cancel coroutine",
        async_test(function()
            local james_bond = spy.new()
            local poutine = a.scope(function()
                a.sleep(100)
                james_bond()
            end)()
            poutine()
            a.sleep(200)
            assert.spy(james_bond).was_not.called()
        end)
    )

    it(
        "should raise error if async function raises error",
        async_test(function()
            local err = assert.has.errors(a.promisify(function()
                error "something went wrong"
            end))
            assert.is_true(match.has_match "something went wrong$"(err))
        end)
    )

    it(
        "should raise error if async function rejects",
        async_test(function()
            local err = assert.has.errors(function()
                a.wait(function(_, reject)
                    reject "This is an error"
                end)
            end)
            assert.equals("This is an error", err)
        end)
    )

    it(
        "should pass nil arguments to promisified functions",
        async_test(function()
            local fn = spy.new(function(_, _, _, _, _, _, _, cb)
                cb()
            end)
            a.promisify(fn)(nil, 2, nil, 4, nil, nil, 7)
            assert.spy(fn).was_called_with(nil, 2, nil, 4, nil, nil, 7, match.is_function())
        end)
    )

    it("should accept yielding non-promise values to parent coroutine context", function()
        local thread = coroutine.create(function(val)
            a.run_blocking(function()
                coroutine.yield(val)
            end)
        end)
        local ok, value = coroutine.resume(thread, 1337)
        assert.is_true(ok)
        assert.equals(1337, value)
    end)

    it(
        "should run all suspending functions concurrently",
        async_test(function()
            local start = timestamp()
            local function sleep(ms, ret_val)
                return function()
                    a.sleep(ms)
                    return ret_val
                end
            end
            local one, two, three, four, five = a.wait_all {
                sleep(100, 1),
                sleep(100, "two"),
                sleep(100, "three"),
                sleep(100, 4),
                sleep(100, 5),
            }
            local grace = 50
            local delta = timestamp() - start
            assert.is_true(delta <= (100 + grace))
            assert.is_true(delta >= (100 - grace))
            assert.equals(1, one)
            assert.equals("two", two)
            assert.equals("three", three)
            assert.equals(4, four)
            assert.equals(5, five)
        end)
    )

    it(
        "should run all suspending functions concurrently",
        async_test(function()
            local start = timestamp()
            local called = spy.new()
            local function sleep(ms, ret_val)
                return function()
                    a.sleep(ms)
                    called()
                    return ret_val
                end
            end
            local first = a.wait_first {
                sleep(150, 1),
                sleep(50, "first"),
                sleep(150, "three"),
                sleep(150, 4),
                sleep(150, 5),
            }
            local grace = 20
            local delta = timestamp() - start
            assert.is_true(delta <= (50 + grace))
            assert.equals("first", first)
        end)
    )

    it("should yield back immediately when not providing any functions", function()
        assert.is_nil(a.wait_first {})
        assert.is_nil(a.wait_all {})
    end)
end)

describe("async :: Condvar", function()
    local Condvar = control.Condvar

    it("should block execution until condvar is notified", function()
        local condvar = Condvar.new()

        local function wait()
            local start = timestamp()
            condvar:wait()
            local stop = timestamp()
            return stop - start
        end

        local start = timestamp()
        local condvar_waits = a.run_blocking(function()
            vim.defer_fn(function()
                condvar:notify_all()
            end, 110)
            return _.table_pack(a.wait_all {
                wait,
                wait,
                wait,
                wait,
            })
        end)
        local stop = timestamp()

        for _, delay in ipairs(condvar_waits) do
            assert.is_True(delay >= 100)
        end
        assert.is_true((stop - start) >= 100)
    end)
end)

describe("async :: Semaphore", function()
    local Semaphore = control.Semaphore

    it("should limit the amount of permits", function()
        local sem = Semaphore.new(5)
        ---@type Permit[]
        local permits = {}

        local cancel_thread = a.run(function()
            while true do
                table.insert(permits, sem:acquire())
            end
        end)
        cancel_thread()

        assert.equals(5, #permits)
    end)

    it("should lease new permits", function()
        local sem = Semaphore.new(2)
        ---@type Permit[]
        local permits = {}

        local cancel_thread = a.run(function()
            while true do
                table.insert(permits, sem:acquire())
            end
        end)

        assert.equals(2, #permits)
        permits[1]:forget()
        permits[2]:forget()
        assert.equals(4, #permits)
        cancel_thread()
    end)
end)

describe("async :: OneShotChannel", function()
    local OneShotChannel = control.OneShotChannel

    it("should only allow sending once", function()
        local channel = OneShotChannel.new()
        assert.is_false(channel:is_closed())
        channel:send "value"
        assert.is_true(channel:is_closed())
        local err = assert.has_error(function()
            channel:send "value"
        end)
        assert.equals("Oneshot channel can only send once.", err)
    end)

    it("should wait until it can receive", function()
        local channel = OneShotChannel.new()

        local start = timestamp()
        local value = a.run_blocking(function()
            vim.defer_fn(function()
                channel:send(42)
            end, 110)
            return channel:receive()
        end)
        local stop = timestamp()

        assert.is_true((stop - start) >= 100)
        assert.equals(42, value)
    end)

    it("should receive immediately if value is already sent", function()
        local channel = OneShotChannel.new()
        channel:send(42)
        assert.equals(42, channel:receive())
    end)
end)
