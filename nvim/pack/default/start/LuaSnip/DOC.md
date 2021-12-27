```
            __                       ____
           /\ \                     /\  _`\           __
           \ \ \      __  __     __ \ \,\L\_\    ___ /\_\  _____
            \ \ \  __/\ \/\ \  /'__`\\/_\__ \  /' _ `\/\ \/\ '__`\
             \ \ \L\ \ \ \_\ \/\ \L\.\_/\ \L\ \/\ \/\ \ \ \ \ \L\ \
              \ \____/\ \____/\ \__/.\_\ `\____\ \_\ \_\ \_\ \ ,__/
               \/___/  \/___/  \/__/\/_/\/_____/\/_/\/_/\/_/\ \ \/
                                                             \ \_\
                                                              \/_/
```

Luasnip is a snippet-engine written entirely in lua. It has some great
features like inserting text (`luasnip-function-node`) or nodes
(`luasnip-dynamic-node`) based on user input, parsing LSP syntax and switching
nodes (`luasnip-choice-node`).
For basic setup like mappings and installing, check the README.

All code-snippets in this help assume that

```lua
local ls = require"luasnip"
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require("luasnip.util.events")
```

# SNIPPETS

The most direct way to define snippets is `s`:
```lua
s({trig="trigger"}, {})
```

(This snippet is useless beyond being a minimal example)

`s` accepts, as the first argument, a table with the following possible
entries:

- `trig`: string, plain text by default. The only entry that must be given.
- `name`: string, can be used by eg. `nvim-compe` to identify the snippet.
- `dscr`: string, description of the snippet, \n-separated or table
          for multiple lines.
- `wordTrig`: boolean, if true, the snippet is only expanded if the word
              (`[%w_]+`) before the cursor matches the trigger entirely.
              True by default.
- `regTrig`: boolean, whether the trigger should be interpreted as a
             lua pattern. False by default.
- `docstring`: string, textual representation of the snippet, specified like
               `dscr`. Overrides docstrings loaded from json.
- `docTrig`: string, for snippets triggered using a lua pattern: define the
             trigger that is used during docstring-generation.
- `hidden`: hint for completion-engines, if set, the snippet should not show
            up when querying snippets.

`s` can also be a single string, in which case it is used instead of `trig`, all
other values being defaulted:

```lua
s("trigger", {})
```

The second argument to `s` is a table containing all nodes that belong to the
snippet. If the table only has a single node, it can be passed directly
without wrapping it in a table.

The third argument is a table with the following valid keys:
- `condition`: the condition-function. The snippet will be expanded only
               if it returns true (default is a function that just returns true)
               (the function is called before the text is modified in any way).
               Some parameters are passed to the function: The line up to the
               cursor, the matched trigger and the captures (table).
* `show_condition`: Function with signature `f(line_to_cursor) -> boolean`
               being a hint for completion-engines indicating when the snippet
               should be included in current completion candidates. Defaults to
               a function returning always true. This is different than `condition`
               because `condition` is evaluated by LuaSnip on snippet expansion
               (and thus has access to the matched trigger and captures), while
               `show_condition` is evaluated by the completion-engine when scanning
               for available snippet candidates.
- `callbacks`: Contains functions that are called upon enterin/leaving a node.
               To print text upon entering the second node of a snippet, set
               `callbacks` should be set as follows:
               `{ [2] = { [events.enter] = function() print "2!" end } }`.
               To register a callback for the snippets' events, the key `[-1]`
               may be used.
               The callbacks are passed only one argument, the node that
               triggered it.
This `opts`-table can also be passed to eg.	`snippetNode` or `indentSnippetNode`,
but only `callbacks` is used there.

Snippets contain some interesting tables, eg. `snippet.env` contains variables
used in the LSP-protocol like `TM_CURRENT_LINE` or `TM_FILENAME` or
`snippet.captures`, where capture-groups of regex-triggers are stored.
Additionally, the string that was used to trigger the snippet is stored in
`snippet.trigger`. These variables/tables are primarily useful in
dynamic/functionNodes, where the snippet can be accessed through the immediate
parent, which is passed to the function.

Snippets that should be loaded for all files must be put into the
`ls.snippets.all`-table, those only for a specific filetype `ft` belong in
`ls.snippets.ft`.

# TEXTNODE

The most simple kind of node; just text.
```lua
s("trigger", t("Wow! Text!"))
```
This snippet expands to

```
    Wow! Text!⎵
```
Where ⎵ is the cursor.
Multiline-strings can be defined by passing a table of lines rather than a
string:

```lua
s("trigger", t({"Wow! Text!", "And another line."}))
```


# INSERTNODE

These Nodes can be jumped to and from. The functionality is best demonstrated
with an example:

```lua
s("trigger", {
	t({"", "After expanding, the cursor is here ->"}), i(1),
	t({"After jumping forward once, cursor is here ->"}), i(2),
	t({"", "After jumping once more, the snippet is exited there ->"}), i(0),
})
```

The InsertNodes are jumped over in order from `1 to n`.
0-th node is special as it's always the last one.
So the order of InsertNode jump is as follows:
- After expansion, we will be at InsertNode 1.
- After jumping forward, we will be at InsertNode 2.
- After jumping forward again, we will be at InsertNode 0.

If no 0-th InsertNode is found in a snippet, one is automatically inserted
after all other nodes.

It is possible to have mixed order in jumping nodes:
```lua
s("trigger", {
	t({"After jumping forward once, cursor is here ->"}), i(2),
	t({"", "After expanding, the cursor is here ->"}), i(1),
	t({"", "After jumping once more, the snippet is exited there ->"}), i(0),
})
```
The above snippet will use the same jump flow as above which is:
- After expansion, we will be at InsertNode 1.
- After jumping forward, we will be at InsertNode 2.
- After jumping forward again, we will be at InsertNode 0.

It's possible to have easy-to-overwrite text inside an InsertNode initially:
```lua
	s("trigger", i(1, "This text is SELECTed after expanding the snippet."))
```
This initial text is defined the same way as textNodes, eg. can be multiline.

`i(0)`s can have placeholder text, but do note that when the SELECTed text is
replaced, its' replacement won't end up in the `i(0)`, but behind it (for
reasons, check out Luasnip#110).


# FUNCTIONNODE

Function Nodes insert text based on the content of other nodes using a
user-defined function:
```lua
 s("trig", {
 	i(1),
 	f(function(args, snip, user_arg_1) return args[1][1] .. user_arg_1 end,
 		{1},
 		"Will be appended to text from i(0)"),
 	i(0)
 })
```
The first parameter of `f` is the function. Its parameters are
1. A table of the text of currently contained in the argnodes.
      (eg. `{{line1}, {line1, line2}}`). The snippet-indent will be removed from
      all lines following the first.

2. The immediate parent of the `functionNode`. It is included here as it allows
      easy access to anything that could be useful in functionNodes (ie.
      `parent.snippet.env` or `parent.snippet.captures`, which contains capture
      groups of regex-triggered snippets). In most cases `parent.env` works,
      but if a `functionNode` is nested within a `snippetNode`, the immediate
      parent (a `snippetNode`) will contain neither `captures` nor `env`. Those
      are only stored in the `snippet`, which can be accessed as `parent.snippet`.

3. Any parameters passed to `f` behind the second (included to more easily
      reuse functions, ie. ternary if based on text in an insertNode).

The second parameter is a table of indices of jumpable nodes whose text is
passed to the function. The table may be empty, in this case the function is
evaluated once upon snippet-expansion. If the table only has a single node, it
can be passed directly without wrapping it in a table.

The function shall return a string, which will be inserted as-is, or a table
of strings for multiline-string, here all lines following the first will be
prefixed with the snippets' indentation.

Examples:
Use captures from the regex-trigger using a functionNode:

```lua
s({trig = "b(%d)", regTrig = true},
	f(function(args, snip) return
		"Captured Text: " .. snip.captures[1] .. "." end, {})
)
```

The table passed to functionNode:

```lua
s("trig", {
	i(1, "text_of_first"),
	i(2, {"first_line_of_second", "second_line_of_second"}),
	-- order is 2,1, not 1,2!!
	f(function(args, snip)
		--here
	end, {2,1} )})
```

At `--here`, `args` would look as follows (provided no text was changed after
expansion):
```lua
args = {
	{"first_line_of_second", "second_line_of_second"},
	{"text_of_first"}
}
```

If the function only performs simple operations on text, consider using
the `lambda` from [`luasnip.extras`](#extras)

# CHOICENODE

ChoiceNodes allow choosing between multiple nodes.

```lua
 s("trig", c(1, {
 	t("Ugh boring, a text node"),
 	i(nil, "At least I can edit something now..."),
 	f(function(args) return "Still only counts as text!!" end, {})
 }))
```

`c()` expects as its first arg, as with any jumpable node, its position in the
jumplist, and as its second a table with nodes, the choices. This table can
either contain a single node or a table of nodes. In the latter case the table
will be converted into a `snippetNode`.
The third parameter is a table of options with the following keys:
- `restore_cursor`: `false` by default. If it is set and the node that was
	being edited also appears in the switched-to choice (can be the case if a
	`restoreNode` is present in both choice) the cursor is restored relative to
	that node.
	The default is `false` as enabling might lead to worse performance. It's
	possible to override the default by wrapping the `choiceNode`-constructor
	in another function that sets `opts.restore_cursor` to `true` and then using
	that to construct `choiceNode`s:
	```lua
	local function restore_cursor_choice(pos, choices, opts)
		if opts then
			opts.restore_cursor = true
		else
			opts = {restore_cursor = true}
		end
		return c(pos, choices, opts)
	end
	```

Jumpable nodes that normally expect an index as their first parameter don't
need one inside a choiceNode; their index is the same as the choiceNodes'.



# SNIPPETNODE

SnippetNodes directly insert their contents into the surrounding snippet.
This is useful for choiceNodes, which only accept one child, or dynamicNodes,
where nodes are created at runtime and inserted as a snippetNode.

Syntax is similar to snippets, however, where snippets require a table
specifying when to expand, snippetNodes, similar to insertNodes, expect a
number, as they too are jumpable:
```lua
 s("trig", sn(1, {
 	t("basically just text "),
 	i(1, "And an insertNode.")
 }))
```

Note that snippetNodes don't expect an `i(0)`.



# INDENTSNIPPETNODE

By default, all nodes are indented at least as deep as the trigger. With these
nodes it's possible to override that behaviour:

```lua
s("isn", {
	isn(1, {
		t({"This is indented as deep as the trigger",
		"and this is at the beginning of the next line"})
	}, "")
})
```

(Note the empty string passed to isn).

Indent is only applied after linebreaks, so it's not possible to remove indent
on the line where the snippet was triggered using `ISN` (That is possible via
regex-triggers where the entire line before the trigger is matched).

Another nice usecase for `ISN` is inserting text, eg. `//` or some other comment-
string before the nodes of the snippet:

```lua
s("isn2", {
	isn(1, t({"//This is", "A multiline", "comment"}), "$PARENT_INDENT//")
})
```

Here the `//` before `This is` is important, once again, because indent is only
applied after linebreaks.
To enable such usage, `$PARENT_INDENT` in the indentstring is replaced by the
parents' indent (duh).



# DYNAMICNODE

Very similar to functionNode: returns a snippetNode instead of just text,
which makes them very powerful.

Parameters:
1. position (just like all jumpable nodes)
2. function: Similar to functionNodes' function, first and second parameters
   are the `table of text` from nodes the dynamicNode depends on (also without
   snippet-indent) and the `snippet` (immediate parent of the `dynamicNode`).
   The third, unlike functionNode, is a user-defined table, `old_state`. This
   table can contain anything, its main usage is to preserve information from
   the previously generated snippetNode: If the dynamicNode depends on another
   node it may be reconstructed, which means all user input to the dynamicNode
   is lost. Using `old_state`, the user may store eg. insertNodes and then get
   their text via `node:get_text()` or `node.old_text` upon reconstruction to
   initialize the new nodes with (only an example, this specific usecase is
   covered much better by `restoreNode`).
   The `old_state` table must be stored inside the `snippetNode` returned by
   the function.
   All parameters following the third are user defined.
3. Indices of nodes the dynamicNode depends on: if any of these trigger an
   update, the dynamicNodes function will be executed and the result inserted at
   the nodes place. Can be a single index or a table of indices.
4. The fourth and following parameters are user defined, anything passed
   here will also be passed to the function (arg 2) following its third
   parameter (easy to reuse similar functions with small changes).

```lua
local function lines(args, snip, old_state, initial_text)
	local nodes = {}
	if not old_state then old_state = {} end

	-- count is nil for invalid input.
	local count = tonumber(args[1][1])
	-- Make sure there's a number in args[1].
	if count then
		for j=1, count do
			local iNode
			if old_state and old_state[j] then
				-- old_text is used internally to determine whether
				-- dependents should be updated. It is updated whenever the
				-- node is left, but remains valid when the node is no
				-- longer 'rendered', whereas node:get_text() grabs the text
				-- directly from the node.
				iNode = i(j, old_state[j].old_text)
			else
			  iNode = i(j, initial_text)
			end
			nodes[2*j-1] = iNode

			-- linebreak
			nodes[2*j] = t({"",""})
			-- Store insertNode in old_state, potentially overwriting older
			-- nodes.
			old_state[j] = iNode
		end
	else
		nodes[1] = t("Enter a number!")
	end

	local snip = sn(nil, nodes)
	snip.old_state = old_state
	return snip
end

...

s("trig", {
	i(1, "1"),
	-- pos, function, argnodes, user_arg1
	d(2, lines, {1}, "Sample Text")
})
```
This snippet would start out as "1\nSample Text" and, upon changing the 1 to
eg. 3, it would change to "3\nSample Text\nSample Text\nSample Text". Text
that was inserted into any of the dynamicNodes insertNodes is kept when
changing to a bigger number.

# RESTORENODE

This node can store and restore a snippetNode that was modified (changed
choices, inserted text) by the user. It's usage is best demonstrated by an
example:

```lua
s("paren_change", {
	c(1, {
		sn(nil, { t("("), r(1, "user_text"), t(")") }),
		sn(nil, { t("["), r(1, "user_text"), t("]") }),
		sn(nil, { t("{"), r(1, "user_text"), t("}") }),
	}),
}, {
	stored = {
		user_text = i(1, "default_text")
	}
})
```

Here the text entered into `user_text` is preserved upon changing choice.

The constructor for the restoreNode, `r`, takes (at most) three parameters:
- `pos`, when to jump to this node.
- `key`, the key that identifies which `restoreNode`s should share their
  content.
- `nodes`, the contents of the `restoreNode`. Can either be a single node or
  a table of nodes (both of which will be wrapped inside a `snippetNode`,
  except if the single node already is a `snippetNode`).
  The content of a given key may be defined multiple times, but if the
  contents differ, it's undefined which will actually be used.
  If a keys content is defined in a `dynamicNode`, it will not be used for
  `restoreNodes` outside that `dynamicNode`. A way around this limitation is
  defining the content in the `restoreNode` outside the `dynamicNode`.

The content for a key may also be defined in the `opts`-parameter of the
snippet-constructor, as seen in the example above. The `stored`-table accepts
the same values as the `nodes`-parameter passed to `r`.
If no content is defined for a key, it defaults to the empty `insertNode`.

An important-to-know limitation of `restoreNode` is that, for a given key, only
one may be visible at a time. See
[this issue](https://github.com/L3MON4D3/LuaSnip/issues/234) for details.

The `restoreNode` is also useful for storing user-input across updates of a
`dynamicNode`. Consider this:

```lua
local function simple_restore(args, _)
	return sn(nil, {i(1, args[1]), i(2, "user_text")})
end

s("rest", {
	i(1, "preset"), t{"",""},
	d(2, simple_restore, 1)
}),
```

Every time the `i(1)` in the outer snippet is changed, the text inside the
`dynamicNode` is reset to `"user_text"`. This can be prevented by using a
`restoreNode`:

```lua
local function simple_restore(args, _)
	return sn(nil, {i(1, args[1]), r(2, "dyn", i(nil, "user_text"))})
end

s("rest", {
	i(1, "preset"), t{"",""},
	d(2, simple_restore, 1)
}),
```
Now the entered text is stored.

`RestoreNode`s indent is not influenced by `indentSnippetNodes` right now. If
that really bothers you feel free to open an issue.

# EXTRAS

The module `"luasnip.extras"` contains nodes that ease writing snippets (This
is only a short outline, their usage is shown more expansively in
`Examples/snippets.lua`):

- `lambda`: A shortcut for `functionNode`s that only do very basic string-
manipulation. For example, to replace all occurences of "a" in the nth insert
with "e", one could use `lambda(lambda._1:gsub("a", "e"), n)` (signature is
similar to that of `functionNode`).
If a node has multiple lines, they will be concatenated using "\n".

- `match`: Can insert text based on whether a given insert matches a pattern or
a function or lambda returns true.
The complete signature for the node is `match(index, match, then, else)`, where
`index` _has_ to be a single number, `match` can be a string (interpreted as lua
pattern), function, or lambda (useful if string-manipulations have to be
performed before the string is matched).
`then` and `else` can also be strings (inserted literally), functions (run with
the same arguments as a `functionNode`-function), or lambda. The default for
`else` is the empty string.
To return "A" if insert n matches "ABC", `match(n, "^ABC$", "A", "")` can be
used. `match(n, lambda._1:match(lambda._1:reverse()), "PALINDROME")` inserts
"PALINDROME" if the nth insert is a palindrome.

- `rep`: repeats the node with the passed index. `rep(1)` to repeat the content
of the first insert.

- `partial`: directly inserts the output of a function. Useful for eg.
`partial(os.date, "%Y")` (arguments passed after the function are passed to it).

- `nonempty`: inserts text if the insert at the given index doesn't contain any
text. `nonempty(n, "empty!", "not empty!")` inserts "empty!" if insert n is
empty, "not empty!" it it isn't.

- `dynamic_lambda`: Operates almost exactly like `lambda`, only that it can be
jumped to and it's contents therfore be easily overridden.
`dynamic_lambda(2, lambda._1..lambda._1, 1)` will first contain the content of
insert 1 appended to itself, but the second jump will lead to it, making it
easy to override the generated text.
The text will only be changed when a argnode updates it.


# LSP-SNIPPETS

Luasnip is capable of parsing lsp-style snippets using
`ls.parser.parse_snippet(context, snippet_string)`:
```lua
ls.parser.parse_snippet({trig = "lsp"}, "$1 is ${2|hard,easy,challenging|}")
```

Nested placeholders(`"${1:this is ${2:nested}}"`) will be turned into
choiceNode's with:
	- the given snippet(`"this is ${1:nested}"`) and
	- an empty insertNode



# VARIABLES

All `TM_something`-variables are supported with two additions:
`SELECT_RAW` and `SELECT_DEDENT`. These were introduced because
`TM_SELECTED_TEXT` is designed to be compatible with vscodes' behavior, which
can be counterintuitive when the snippet can be expanded at places other than
the point where selection started (or when doing transformations on selected text).

All variables can be used outside of lsp-parsed snippets as their values are
stored in a snippets' `snip.env`-table:
```lua
s("selected_text", {
	-- the surrounding snippet is passed in args after all argnodes (none,
	-- in this case).
	f(function(args, snip) return snip.env.SELECT_RAW end, {})
})
```

To use any `*SELECT*` variable, the `store_selection_keys` must be set via
`require("luasnip").config.setup({store_selection_keys="<Tab>"})`. In this case,
hitting `<Tab>` while in Visualmode will populate the `*SELECT*`-vars for the next
snippet and then clear them.



# VSCODE SNIPPETS LOADER

As luasnip is capable of loading the same format of plugins as vscode, it also
includes an easy way for loading those automatically. You just have to call:
```lua
 	require("luasnip.loaders.from_vscode").load(opts) -- opts can be ommited
```

Where `opts` is a table containing the keys:
	-  `paths`: List of paths to load. Can be a table or a single,
		comma-separated string. If not set, `runtimepath` is used. The paths
		may begin with `~/` or `./` to indicate that the path is relative to
		your home or to the folder where your `$MYVIMRC` resides (useful to
		add your snippets). The directories passed this way must be structured
		like [`friendly-snippets`](https://github.com/rafamadriz/friendly-snippets)
		eg. include a `package.json`.
	-  `exclude`: List of languages to exclude, by default is empty.
	-  `include`: List of languages to include, by default is not set.

The last two are useful mainly to avoid loading snippets from 3erd parties you
don't wanna include.

Keep in mind that it will extend your `snippets` table, so do it after setting
your snippets or you will have to extend the table as well.

Another way of using the loader is making it lazily

```lua
 	require("luasnip.loaders.from_vscode").lazy_load(opts) -- opts can be ommited
```

In this case `opts` only accepts paths (`runtimepath` if any). That will load
the general snippets (the ones of filetype 'all') and those of the filetype
of the buffers, you open every time you open a new one (but it won't reload them).

Apart from what is stipulated by the start each snippet in the json file can
contain a "luasnip" field which is a table for extra parameters for the snippet,
till now the only valid one is autotrigger.

After snippets were lazy-loaded, the `User LuasnipSnippetsAdded`-event will be
triggered.

Note load vscode-style packages using `require("luasnip.loaders.from_vscode").load()`, if 
you've configured luasnip to detect the filetype based on the cursor position. Else the
snippets won't be available to the `from_cursor_pos` function.

# SNIPMATE SNIPPETS LOADER

As the snipmate snippet format is fundamentally the same as vscode, it can also
be loaded.

```lua
 	require("luasnip.loaders.from_snipmate").load(opts) -- opts can be ommited
```

See `from_vscode` for an explanation of opts.
If `opts.paths` is ommited, snippets are loaded from any directory named
`snippets` located in the `runtimepath`.

Luasnip is compatible with
[honza/vim-snippets](https://github.com/honza/vim-snippets).
Please use it as a reference for your directory structure.

When using `honza/vim-snippets`, the file with the global snippets is
`_.snippets`, So we need to tell luasnip that `_` also contains global snippets:
```lua
ls.filetype_extend("all", { "_" })
```

Something similar may have to be done for other snippet-repos as well.


Lazy loading is also available with the snipmate-loader.

```lua
 	require("luasnip.loaders.from_snipmate").lazy_load(opts) -- opts can be ommited
```


Here is a summary of the differences from the original snipmate format.

- Only `./{ft}.snippets` and `./{ft}/*.snippets` will be loaded.
- The file name or folder name will be used as file type.
- You can use the comment and extends syntax.
- `${VISUAL}` will be replaced by `$TM_SELECTED_TEXT` to make the snippets
compatible with luasnip
- We do not implement eval using \` (backtick). This may be implemented in the future.


# EXT\_OPTS

`ext_opts` are probably best explained with a short example:
```lua
local types = require("luasnip.util.types")

vim.api.nvim_command("hi LuasnipChoiceNodePassive cterm=italic")
ls.config.setup({
	ext_opts = {
		[types.insertNode] = {
			passive = {
				hl_group = "GruvboxRed"
			}
		},
		[types.choiceNode] = {
			active = {
				virt_text = {{"choiceNode", "GruvboxOrange"}}
			}
		},
		[types.textNode] = {
			snippet_passive = {
				hl_group = "GruvboxGreen"
			}
		},
	},
	ext_base_prio = 200,
	ext_prio_increase = 3,
})
```

This highlights `insertNodes` red (both when active and passive) and adds
virtualText and italics to `choiceNode` while it is active (both only if the
snippet is active). `textNodes` are highlighted green, even if the snippet is
no longer active. (unspecified values in `passive` are populated with values
from `snippet_passive`, those in `active` with those from the new `passive`).

The `active`/ `passive`-tables are passed to `nvim_buf_set_extmark` as `opts`
which means only entries valid there can be used here. `priority`, while still
affecting the priority of highlighting, is interpreted as a relative value here,
not absolute (`0 <= priority < ext_prio_increase`).
The absolute range of priorities can still be somewhat controlled using
`ext_base_prio` and `ext_prio_increase` (all highlights start out with
`ext_base_prio`+their own priority, for highlights belonging to a nested
snippet(Node), `ext_base_prio` is increased by `ext_prio_increase`)).

As a shortcut for setting `hl_group`, the highlight-groups
`Luasnip*Node{Active,Passive,SnippetPassive}` may be defined (to be actually
used by LuaSnip, `ls.config.setup` has to be called after defining). They are
overridden by the values defined in `ext_opts` directly, but otherwise behave
the same (active is extended by passive).



# DOCSTRING

Snippet-docstrings can be queried using `snippet:get_docstring()`. The function
evaluates the snippet as if it was expanded regularly, which can be problematic
if eg. a dynamicNode in the snippet relies on inputs other than
the argument-nodes.
`snip.env` and `snip.captures` are populated with the names of the queried
variable and the index of the capture respectively
(`snip.env.TM_SELECTED_TEXT` -> `'$TM_SELECTED_TEXT'`, `snip.captures[1]` ->
 `'$CAPTURES1'`). Although this leads to more expressive docstrings, it can
 cause errors in functions that eg. rely on a capture being a number:

```lua
s({trig = "(%d)", regTrig = true}, {
	f(function(args, snip)
		return string.rep("repeatme ", tonumber(snip.captures[1]))
	end, {})
}),
```

This snippet works fine because	`snippet.captures[1]` is always a number.
During docstring-generation, however, `snippet.captures[1]` is `'$CAPTURES1'`,
which will cause an error in the functionNode.
Issues with `snippet.captures` can be prevented by specifying `docTrig` during
snippet-definition:

```lua
s({trig = "(%d)", regTrig = true, docTrig = "3"}, {
	f(function(args, snip)
		return string.rep("repeatme ", tonumber(snip.captures[1]))
	end, {})
}),
```

`snippet.captures` and `snippet.trigger` will be populated as if actually
triggered with `3`.

Other issues will have to be handled manually by checking the contents of eg.
`snip.env` or predefining the docstring for the snippet:

```lua
s({trig = "(%d)", regTrig = true, docstring = "repeatmerepeatmerepeatme"}, {
	f(function(args, snip)
		return string.rep("repeatme ", tonumber(snip.captures[1]))
	end, {})
}),
```



# DOCSTRING-CACHE

Although generation of docstrings is pretty fast, it's preferable to not
redo it as long as the snippets haven't changed. Using
`ls.store_snippet_docstrings(ls.snippets)` and its counterpart
`ls.load_snippet_docstrings(ls.snippets)`, they may be serialized from or
deserialized into the snippets.
Both functions accept a table structured like `ls.snippets`, ie.
`{ft1={snippets}, ft2={snippets}}`.
`load` should be called before any of the `loader`-functions as snippets loaded
from vscode-style packages already have their `docstring` set (`docstrings`
wouldn't be overwritten, but there'd be unnecessary calls).

The cache is located at `stdpath("cache")/luasnip/docstrings.json` (probably
`~/.cache/nvim/luasnip/docstrings.json`).

# EVENTS

Upon leaving/entering nodes or changing a choice an event is triggered:
`User Luasnip<Node>{Enter,Leave}`, where `<Node>` is the name of a node in
PascalCase, eg. `InsertNode` or `DynamicNode` or `Snippet`.
The event triggered when changing the choice in a `choiceNode` is
`User LuasnipChangeChoice`.

A pretty useless, beyond serving as an example here, application of these would
be printing eg. the nodes' text after entering:

```vim
au User LuasnipInsertNodeEnter
	\lua print(require("luasnip").session.event_node:get_text()[1])
```

# CLEANUP
The function ls.cleanup()  triggers the `LuasnipCleanup` user-event, that you can listen to do some kind
of cleaning in your own snippets, by default it will  empty the snippets table and the caches of
the lazy_load.

# API-REFERENCE

`require("luasnip")`:
- `jumpable(direction)`: returns true if the current node has a
  next(`direction` = 1) or previous(`direction` = -1), eg. whether it's
  possible to jump forward or backward to another node.

- `jump(direction)`: returns true if the jump was successful.

- `expandable()`: true if a snippet can be expanded at the current cursor position.

- `expand()`: expands the snippet at(before) the cursor.

- `expand_or_jumpable()`: returns `expandable() or jumpable(1)` (exists only
  because commonly, one key is used to both jump forward and expand).

- `expand_or_locally_jumpable()`: same as `expand_or_jumpable()` except jumpable
  is ignored if the cursor is not inside the current snippet.

- `expand_or_jump()`: returns true if jump/expand was succesful.

- `expand_auto()`: expands the autosnippets before the cursor (not necessary
  to call manually, will be called via autocmd if `enable_autosnippet` is set
  in the config).

- `snip_expand(snip, opts)`: expand `snip` at the current cursor position.
  `opts` may contain the following keys:
    - `clear_region`: A region of text to clear after expanding (but before
      jumping into) snip. It has to be at this point (and therefore passed to
      this function) as clearing before expansion will populate `TM_CURRENT_LINE`
      and `TM_CURRENT_WORD` with wrong values (they would miss the snippet trigger)
      and clearing after expansion may move the text currently under the cursor
      and have it end up not at the `i(1)`, but a `#trigger` chars to it's right.
      The actual values used for clearing are `from` and `to`, both (0,0)-indexed
      byte-positions.
      If the variables don't have to be populated with the correct values, it's
      safe to remove the text manually.
    - `expand_params`: table, for overriding the `trigger` used in the snippet
      and setting the `captures` (useful for pattern-triggered nodes where the
      trigger has to be changed from the pattern to the actual text triggering the
      node).
      Pass as `trigger` and `captures`.
    - `pos`: position (`{line, col}`), (0,0)-indexed (in bytes, as returned by
      `nvim_win_get_cursor()`), where the snippet should be expanded. The
      snippet will be put between `(line,col-1)` and `(line,col)`. The snippet
      will be expanded at the current cursor if pos is nil.
  `opts` and either of its parameter may be nil.

- `get_active_snip()`: returns the currently active snippet (not node!).

- `choice_active()`: true if inside a choiceNode.

- `change_choice(direction)`: changes the choice in the innermost currently
  active choiceNode forward (`direction` = 1) or backward (`direction` = -1).

- `unlink_current()`: removes the current snippet from the jumplist (useful
  if luasnip fails to automatically detect eg. deletion of a snippet) and
  sets the current node behind the snippet, or, if not possible, before it.

- `lsp_expand(snip_string, opts)`: expand the lsp-syntax-snippet defined via
  `snip_string` at the cursor.
  `opts` can have the same options as `opts` in `snip_expand`.

- `active_update_dependents()`: update all function/dynamicNodes that have
  the current node as their argument (will only actually update them if
  the text in any of the argnodes changed).

- `available()`: return a table of all snippets defined for the current
  filetypes(s) (`{ft1={snip1, snip2}, ft2={snip3, snip4}}`).

- `exit_out_of_region(node)`: checks whether the cursor is still within the
  range of the snippet `node` belongs to. If yes, no change occurs, if not,
  the snippet is left and following snippets are checked (the next active
  node will be the 0-node of the snippet before the one the cursor is
  inside. If the cursor isn't inside any snippet, the active node will be
  the last node in the jumplist). If a jump causes an error (happens mostly
  because a snippet was deleted), the snippet is removed from the jumplist.

- `store_snippet_docstrings(snippet_table)`: Stores the docstrings of all
  snippets in `snippet_table` to a file
  (`stdpath("cache")`/luasnip/docstrings.json).
  Calling `store_snippet_docstrings(snippet_table)` after adding/modifying
  snippets and `load_snippet_docstrings(snippet_table)` on startup
  after all snippets have been added to `snippet_table` is a way to avoide
  regenerating the (unchanged) docstrings on each startup.
  (Depending on when the docstrings are required and how luasnip is loaded,
  it may be more sensible to let them load lazily, eg. just before they are
  required).
  `snippet_table` should be laid out just like `luasnip.snippets` (it will
  also most likely always _be_ `luasnip.snippets`).

- `load_snippet_docstrings(snippet_table)`: Load docstrings for all snippets
  in `snippet_table` from `stdpath("cache")`/luasnip/docstrings.json.
  The docstrings are stored and restored via trigger, meaning if two
  snippets for one filetype have the same(very unlikely to happen in actual
  usage), bugs could occur.
  `snippet_table` should be laid out like described in
  `store_snippet_docstrings`.

- `unlink_current_if_deleted()`: Checks if the current snippet was deleted,
  if so, it is removed from the jumplist. This is not 100% reliable as
  luasnip only sees the extmarks and their begin/end may not be on the same
  position, even if all the text between them was deleted.

- `filetype_extend(filetype, extend_filetypes)`: Tells luasnip that for a
  buffer with `ft=filetype`, snippets from `extend_filetypes` should be
  searched as well. `extend_filetypes` is a lua-array (`{ft1, ft2, ft3}`).
  `luasnip.filetype_extend("lua", {"c", "cpp"})` would search and expand
  c-and cpp-snippets for lua-files.

- `filetype_set(filetype, replace_filetypes):` Similar to `filetype_extend`,
  but where it appended filetypes, this replaces them:
  `filetype_set("lua", {"c"})` causes only c-snippets to be expanded in
  lua-files, lua-snippets aren't even searched.

- `cleanup()`: clears all snippets. Not useful for regular usage, only when
  authoring and testing snippets.

- `refresh_notify(ft)`: Triggers an autocmd that other plugins can hook into to
  perform various cleanup for the refreshed filetype.
  Useful for signaling that new snippets were added for the filetype `ft`.

Not covered in this section are the various node-constructors exposed by
the module, their usage is shown either previously in this file or in
`Examples/snippets.lua` (in the repo).
