local cmd = vim.cmd

-- stdlib
local function table_merge(t1, t2)
  for k, v in pairs(t2) do
    if type(v) == "table" then
      if type(t1[k] or false) == "table" then
        table_merge(t1[k] or {}, t2[k] or {})
      else
        t1[k] = v
      end
    else
      t1[k] = v
    end
  end
  return t1
end

local is_gnvim = (vim.g.gnvim == 1)

-- theme, powerline, font
vim.g.gruvbox_material_sign_column_background = 'clear' -- transparent signcolumn
vim.g.gruvbox_material_enable_bold = 1
cmd 'autocmd vimenter * ++nested colorscheme gruvbox-material'
vim.g.airline_powerline_fonts = 1
cmd 'set guifont=JetBrains\\ Mono'
cmd 'set termguicolors'
-- line numbers
cmd 'set number'
-- smartcase
cmd 'set ignorecase smartcase'
-- mouse
cmd 'set mouse=a'
-- allow hiding unsaved buffers
cmd 'set hidden'

-- lualine
require('lualine').setup {
  options = { theme = 'gruvbox' },
  extensions = { "nvim-tree" },
}

-- telescope
require('telescope').setup {
  pickers = {
    lsp_code_actions = {
      theme = "cursor",
    }
  },
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {
      }
    }
  }
}

require("telescope").load_extension("ui-select")

-- leader key
vim.g.mapleader = ' '

-- snippets
local luasnip = require('luasnip')
require("luasnip.loaders.from_vscode").lazy_load({
  paths = {
    "./pack/default/start/awesome-flutter-snippets/",
    "./pack/default/start/friendly-snippets",
  }
})

-- completion
vim.o.completeopt = "menuone,noselect"

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local cmp = require 'cmp'
cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      luasnip.lsp_expand(args.body) -- For `luasnip` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      -- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
    end,
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  mapping = {
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Set `select` to `false` to only confirm explicitly selected items.
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    -- { name = 'vsnip' }, -- For vsnip users.
    { name = 'luasnip' }, -- For luasnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' }, -- For snippy users.
  }, {
    { name = 'buffer' },
  }),
  experimental = {
    native_menu = is_gnvim, -- only use native_menu in gnvim
  },
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- cmp + autoopairs
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))

-- Setup lspconfig.
local cmp_lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()

cmd 'highlight link CompeDocumentation NormalFloat'

-- autopairs
require('nvim-autopairs').setup {}

-- commenting
require('Comment').setup()

-- hop
require 'hop'.setup()

-- undotree
vim.g.undotree_SetFocusWhenToggle = 1

-- nvim tree
-- automatically close nvim when nvim-tree is the last open window
cmd "autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif"
require 'nvim-tree'.setup {
  update_focused_file = {
    enable      = true,
    -- update the root directory of the tree to the one of the folder containing the file if the file is not under the current root directory
    -- only relevant when `update_focused_file.enable` is true
    update_cwd  = false,
    -- list of buffer names / filetypes that will not update the cwd if the file isn't found under the current root directory
    -- only relevant when `update_focused_file.update_cwd` is true and `update_focused_file.enable` is true
    ignore_list = {}
  },
  actions = {
    open_file = {
      quit_on_open = false,
      window_picker = {
        exclude = {
          filetype = { 'Outline' },
          buftype = { 'terminal' }
        }
      }
    }
  },
  renderer = {
    icons = {
      show = {
        file = true,
        folder = true,
        folder_arrow = true,
      },
    },
    highlight_git = true,
  },
  update_cwd = true,
}

-- session
local auto_session_config = {
  log_level = 'info',
  auto_session_enable_last_session = false,
  auto_session_root_dir = vim.fn.stdpath('data') .. "/sessions/",
  auto_session_enabled = true,
  auto_save_enabled = true,
  auto_restore_enabled = true,
  auto_session_suppress_dirs = {
    '~',
  },
}

require('auto-session').setup(auto_session_config)

-- neogit
local neogit = require('neogit')
neogit.setup {}

-- lazygit
-- vim.api.nvim_set_var('lazygit_floating_window_use_plenary', '0')

-- gitsigns
require('gitsigns').setup {
  keymaps = {
    noremap = true,
    buffer = true,

    ['n ]c'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'" },
    ['n [c'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'" },

    ['n <leader>gs'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['v <leader>gs'] = '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <leader>gu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <leader>gr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['v <leader>gr'] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <leader>gR'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <leader>gp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <leader>gb'] = '<cmd>lua require"gitsigns".blame_line(true)<CR>',

    -- Text objects
    ['o ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
  }
}

-- trouble
require("trouble").setup {}

-- symbols outline
require("symbols-outline").setup {}

-- tree-sitter
cmd "set nofoldenable" -- don't close all folds upon opening a file
cmd "set foldlevel=99" -- don't close all folds upon opening a file
cmd "set foldmethod=expr"
cmd "set foldexpr=nvim_treesitter#foldexpr()"
require 'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "lua", "haskell" },
  -- Automatically install missing parsers when entering buffer
  auto_install = true,

  highlight = {
    enable = true,
    -- list of language that will be disabled
    disable = {},

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<leader>ee",
      node_incremental = "<leader>ee",
      scope_incremental = "<leader>es",
      node_decremental = "<leader>ew",
    },
  },
}

-- lsp
local lspconfig = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap = true, silent = true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'gn', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>Telescope lsp_references<CR>', opts)
  buf_set_keymap('n', 'gtd', '<cmd>Telescope lsp_type_definitions<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>lwa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>la', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('v', '<leader>la', "<cmd>'<,'>lua vim.lsp.buf.code_action()<CR>", opts)
  buf_set_keymap('n', '<leader>le', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '<leader>lq', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<leader>lf', '<cmd>lua vim.lsp.buf.format{ async = true }<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('', '<F2>', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('', '<S-F2>', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('', '<F3>', '<cmd>SymbolsOutlineOpen<CR>', opts)
  buf_set_keymap('', '<S-F3>', '<cmd>SymbolsOutlineClose<CR>', opts)

  -- lsp signature
  local cfg = {
    bind = true, -- This is mandatory, otherwise border config won't get registered.
    -- If you want to hook lspsaga or other signature handler, pls set to false
    doc_lines = 2, -- will show two lines of comment/doc(if there are more than two lines in doc, will be truncated);
    -- set to 0 if you DO NOT want any API comments be shown
    -- This setting only take effect in insert mode, it does not affect signature help in normal
    -- mode, 10 by default

    floating_window = true, -- show hint in a floating window, set to false for virtual text only mode
    fix_pos = false, -- set to true, the floating window will not auto-close until finish all parameters
    hint_enable = false, -- virtual hint enable
    hint_prefix = "🐼 ", -- Panda for parameter
    hint_scheme = "String",
    use_lspsaga = false, -- set to true if you want to use lspsaga popup
    hi_parameter = "Search", -- how your parameter will be highlight
    max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down
    -- to view the hiding contents
    max_width = 120, -- max_width of signature floating_window, line will be wrapped if exceed max_width
    handler_opts = {
      border = "single" -- double, single, shadow, none
    },
    extra_trigger_chars = {} -- Array of extra characters that will trigger signature completion, e.g., {"(", ","}
  }
  require "lsp_signature".on_attach(cfg)
end


local default_lsp_options = {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = {
    textDocument = {
      completion = {
        completionItem = {
          snippetSupport = true,
        },
      },
    },
  },
}

local function setup_lsp(server, options_arg)
  local options = table_merge(options_arg or {}, default_lsp_options)
  options.capabilities = table_merge(options.capabilities, cmp_lsp_capabilities)
  lspconfig[server].setup(options)
end

require("mason").setup()
require("mason-lspconfig").setup()

require("mason-lspconfig").setup_handlers {
  -- The first entry (without a key) will be the default handler
  -- and will be called for each installed server that doesn't have
  -- a dedicated handler.
  function(server_name) -- default handler (optional)
    setup_lsp(server_name, {})
  end,
  -- Next, you can provide a dedicated handler for specific servers.
  -- For example, a handler override for the `rust_analyzer`:

  ["purescriptls"] = function()
    setup_lsp('purescriptls', {
      settings = {
        purescript = {
          formatter = "tidy",
        }
      },
    })
  end,

}


-- haskell
setup_lsp('hls', {
  settings = {
    haskell = {
      formattingProvider = 'fourmolu',
    },
  },
})

-- flutter
require("flutter-tools").setup {
  lsp = default_lsp_options,
}

-- lsp event handlers
local telescope_builtin = require 'telescope.builtin'
vim.lsp.handlers['textDocument/references'] = telescope_builtin.lsp_references
vim.lsp.handlers['textDocument/definition'] = telescope_builtin.lsp_definitions
vim.lsp.handlers['textDocument/implementation'] = telescope_builtin.lsp_implementations
vim.lsp.handlers['textDocument/documentSymbol'] = telescope_builtin.lsp_document_symbols
vim.lsp.handlers['workspace/symbol'] = telescope_builtin.lsp_workspace_symbols

-- keymap
local remap = { noremap = false, silent = true }
local noremap = { noremap = true, silent = true }
local function ikeycmd(k, cmd)
  vim.api.nvim_set_keymap('i', k, '<cmd>' .. cmd .. '<CR>', noremap)
end

local function nkeycmd(k, cmd)
  vim.api.nvim_set_keymap('n', k, '<cmd>' .. cmd .. '<CR>', noremap)
end

local function keycmd(k, cmd)
  ikeycmd(k, cmd)
  vim.api.nvim_set_keymap('', k, '<cmd>' .. cmd .. '<CR>', noremap)
end

local function leader_shortcut(k, cmd)
  vim.api.nvim_set_keymap('', '<leader>' .. k, '<cmd>' .. cmd .. '<CR>', noremap)
end

local function mk_prefix(p)
  return function(k, ...) return leader_shortcut(p .. k, ...) end
end

local command_key = mk_prefix('c')
local file_key = mk_prefix('f')
local git_key = mk_prefix('g')
-- local lsp_key = mk_prefix('l')
local project_key = mk_prefix('p')
local tab_key = mk_prefix('b')
local window_key = mk_prefix('w')

keycmd('<F1>', 'NvimTreeOpen')
keycmd('<S-F1>', 'NvimTreeClose')
keycmd('<F4>', 'Trouble')
keycmd('<S-F4>', 'TroubleClose')
nkeycmd('U', 'UndotreeToggle')

command_key('a', 'Telescope commands')

local telescope_find_file = 'Telescope find_files disable_devicons=false'
local window_select_top = 'wincmd k'
local window_select_right = 'wincmd l'
local window_select_down = 'wincmd j'
local window_select_left = 'wincmd h'
file_key('o', telescope_find_file)
file_key('h', ' split <bar> ' .. window_select_down .. ' <bar> ' .. telescope_find_file)
file_key('v', 'vsplit <bar> ' .. window_select_right .. ' <bar> ' .. telescope_find_file)
file_key('s', 'write')

-- see gitsigns for more shortcuts
git_key('g', 'Neogit')
git_key('l', 'LazyGit')

project_key('s', 'Telescope session-lens search_session')
project_key('w', 'SaveSession')
project_key('d', 'DeleteSession')

if is_gnvim then
  tab_key('c', 'tabnew')
  tab_key('d', 'tabclose')
  tab_key('n', 'tabnext')
  tab_key('p', 'tabprevious')
else
  tab_key('d', 'BufferClose')
  tab_key('n', 'BufferNext')
  tab_key('p', 'BufferPrevious')
end

window_key('o', 'only')
window_key('v', 'vsplit')
window_key('V', 'vsplit <bar> ' .. window_select_right)
window_key('h', 'split')
window_key('H', 'split <bar> ' .. window_select_down)
window_key('d', 'hide')
window_key('i', window_select_top)
window_key('j', window_select_left)
window_key('k', window_select_down)
window_key('l', window_select_right)
window_key('p', window_select_top)
window_key('t', window_select_left)
window_key('n', window_select_down)
window_key('s', window_select_right)

leader_shortcut('h', 'HopChar1')
leader_shortcut('sh', 'nohlsearch')
