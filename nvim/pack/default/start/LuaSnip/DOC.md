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
- `callbacks`: Contains functions that are called upon enterin/leaving a node.
               To print text upon entering the second node of a snippet, set
               `callbacks` should be set as follows:
               `{ [2] = { [events.enter] = function() print "2!" end } }`.
               To register a callback for the snippets' events, the key `[-1]`
               may be used.
               The callbacks are passed only one argument, the node that
               triggered it.
This `opts`-table can also be passed to eg.	`snippetNode` or `indentSnippetNode`,
`condition` doesn't have any effect there, but `callbacks` are used.

Snippets contain some interesting tables, eg. `snippet.env` contains variables
used in the LSP-protocol like `TM_CURRENT_LINE` or `TM_FILENAME` or
`snippet.captures`, where capture-groups of regex-triggers are stored. These
tables are primarily useful in dynamic/functionNodes, where the snippet is
passed to the generating function.

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
	1.: A table of the text of currently contained in the argnodes.
	(eg. `{{line1}, {line1, line2}}`). The snippet-indent will be removed from
	all lines following the first.

	2.: The surrounding snippet. It is included here as it allows access to
	anything that could be useful in functionNodes (ie. `snippet.env` or
	`snippet.captures`, which contains capture groups of regex-triggered
	snippets).

	3.: Any parameters passed to `f` behind the second (included to more easily
	reuse functions, ie. ternary if based on text in an insertNode).

The second parameter is a table of indizes of jumpable nodes whose text is
passed to the function. The table may be empty, in this case the function is
evaluated once upon snippet-expansion. If the table only has a single node, it
can be passed directly without wrapping it in a table.

The function shall return a string, which will be inserted as-is, or a table
of strings for multiline snippets, here all lines following the first will be
prepended with the snippets' indentation.

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
	f(function(args, snip) --here end, {2,1} )})
```

At `--here`, `args` would look as follows (provided no text was changed after
expansion):
```lua
args = {
	{"first_line_of_second", "second_line_of_second"},
	{"text_of_first"}
}
```

# CHOICENODE

ChoiceNodes allow choosing between multiple nodes.

```lua
 s("trig", c(1, {
 	t("Ugh boring, a text node"),
 	i(nil, "At least I can edit something now..."),
 	f(function(args) return "Still only counts as text!!" end, {})
 }))
```

`c()` expects as it first arg, as with any jumpable node, its position in the
jumplist, and as its second a table with nodes, the choices.

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
2. function: Similar to functionNodes' function, first parameter is the
   `table of text` from nodes the dynamicNode depends on(also without
   snippet-indent), the second, unlike functionNode, is a user-defined table,
   `old_state`. This table can contain anything, its main usage is to preserve
   information from the previously generated snippetNode:
   If the dynamicNode depends on another node it may be reconstructed,
   which means all user input to the dynamicNode is lost. Using
   `old_state`, the user may pass eg. insertNodes and then get their text via
   `node:get_text()` or `node.old_text` upon reconstruction to initialize the
   new nodes with.
   The `old_state` table must be stored inside the snippetNode returned by
   the function.
   All parameters following the second are user defined.
3. Nodes the dynamicNode depends on: if any of these trigger an update,
   the dynamicNodes function will be executed and the result inserted at
   the nodes place. Can be a single node or a table of nodes.
4. The fourth and following parameters are user defined, anything passed
   here will also be passed to the function (arg 2) following its second
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
 	require("luasnip/loaders/from_vscode").load(opts) -- opts can be ommited
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
contain a "luasnip" field wich is a table for extra parameters for the snippet,
till now the only valid one is autotrigger.

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
