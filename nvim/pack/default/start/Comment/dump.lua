-- TODO
-- [x] Handle Tabs
-- [x] Dot repeat
-- [x] Comment multiple line.
-- [x] Hook support
--      [x] pre
--      [x] post
-- [x] Custom (language) commentstring support
-- [x] Block comment basic ie. /* */ (for js)
-- [-] Block comment extended
--      [x] left-right-motions
--      [x] Partial blocks ie. gba{ gbaf
--      [ ] V-BLOCK (IDK, maybe)
--      [ ] Char motion covering mutliple lines ie. gc300w (level: HARD)
-- [-] Treesitter Integration
--      [ ] Better comment detection
--      [x] Context commentstring
-- [x] Port `commentstring` from tcomment
-- [x] Ignore line
-- [x] Disable `extra` mapping by default
-- [x] Provide more arguments to pre and post hooks
-- [x] `ignore` as a function
-- [x] Return the operator's starting and ending position in pre and post hook
-- [x] Restore cursor position in some motion operator (try `gcip`)
-- [ ] Doc comment ie. /** */ (for js)
-- [ ] Header comment

-- FIXME
-- [x] visual mode not working correctly
-- [x] space after and before of commentstring
-- [x] multiple line behavior to tcomment
--      [x] preserve indent
--      [x] determine comment status (to comment or not)
-- [x] prevent uncomment on uncommented line
-- [x] `comment` and `toggle` misbehaving when there is leading space
-- [x] messed up indentation, if the first line has greater indentation than next line (calc min indendation)
-- [x] `gcc` empty line not toggling comment
-- [x] Optimize blockwise mode (just modifiy the start and end line)
-- [x] Weird commenting when the first line is empty and the whole is indented
-- [x] no padding support in block-x

-- THINK:
-- [ ] Dot support for `[count]gcc` and `[count]gbc`
-- [ ] Parse `comments` if block comment is missing in the plugin

-- ITS_OK:
-- 1. Weird comments, if you do comments on already commented lines incl. an extra empty line
-- 2. Conflict when uncommenting interchangebly with line/block wise comment
-- 3. `ignore` doesn't work in blockwise and blockwise_x
-- 4. When a empty line is b/w two commented blocks then it should uncomment instead of commenting again in toggle.

-- DROPPED:
-- 1. Insert mode mapping (also move the cursor after commentstring)
-- 2. Use `nvim_buf_get_text` instead of `nvim_buf_get_lines`. Blocked by https://github.com/neovim/neovim/pull/15181
-- 3. Use `nvim_buf_set_text` instead of `nvim_buf_set_lines`
-- 4. Dot repeat support for visual mode mappings (Doesn't make sense)
