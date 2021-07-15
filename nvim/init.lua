local cmd = vim.cmd

-- stdlib
function table_merge(t1, t2)
    for k,v in pairs(t2) do
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
vim.api.nvim_set_var('gruvbox_material_sign_column_background', 'clear') -- transparent signcolumn
cmd 'autocmd vimenter * ++nested colorscheme gruvbox-material'
vim.api.nvim_set_var('airline_powerline_fonts', 1)
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
require('lualine').setup{
  options = {theme = 'gruvbox'}
}

-- leader key
vim.g.mapleader = ' '

-- completion
vim.o.completeopt = "menuone,noselect"
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  };

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = false;
    ultisnips = false;
    luasnip = true;
  };
}
cmd 'highlight link CompeDocumentation NormalFloat'
cmd [[
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
]]

-- hop
require'hop'.setup()

-- undotree
vim.g.undotree_SetFocusWhenToggle = 1

-- nvim tree
-- vim.g.nvim_tree_update_cwd = 1 -- TODO: upstream issue: https://github.com/kyazdani42/nvim-tree.lua/issues/441
vim.g.nvim_tree_quit_on_open = 1
vim.g.nvim_tree_follow = 1
vim.g.nvim_tree_show_icons = {
  git= 1,
  files= 1,
  folders= 1,
  folder_arrows= 1
}

-- session
local opts = {
  log_level = 'info',
  auto_session_enable_last_session = false,
  auto_session_root_dir = vim.fn.stdpath('data').."/sessions/",
  auto_session_enabled = true,
  auto_save_enabled = nil,
  auto_restore_enabled = nil,
  auto_session_suppress_dirs = {
    '~',
  },
}

require('auto-session').setup(opts)

-- neogit
local neogit = require('neogit')
neogit.setup {}

-- gitsigns
require('gitsigns').setup{
  keymaps = {
    noremap = true,
    buffer = true,

    ['n ]c'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"},

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

-- neuron
require'neuron'.setup{
  neuron_dir = '~/doc/neuron',
  leader = ' z',
}

-- snippets
local luasnip = require('luasnip')
local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

_G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-n>"
    elseif luasnip and luasnip.expand_or_jumpable() then
        return t "<Plug>luasnip-expand-or-jump"
    elseif check_back_space() then
        return t "<Tab>"
    else
        return vim.fn['compe#complete']()
    end
end
_G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-p>"
    elseif luasnip and luasnip.jumpable(-1) then
        return t "<Plug>luasnip-jump-prev"
    else
        return t "<S-Tab>"
    end
end

-- Map tab to the above tab complete functions
vim.api.nvim_set_keymap('i', '<Tab>', 'v:lua.tab_complete()', { expr = true })
vim.api.nvim_set_keymap('s', '<Tab>', 'v:lua.tab_complete()', { expr = true })
vim.api.nvim_set_keymap('i', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })
vim.api.nvim_set_keymap('s', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })

-- lsp
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'gn', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', 'gtd', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>lwa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>la', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<leader>le', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '<leader>lq', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<leader>lf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('', '<F2>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('', '<S-F2>', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('', '<F3>', '<cmd>SymbolsOutline<CR>', opts)

  -- lsp signature
  cfg = {
    bind = true, -- This is mandatory, otherwise border config won't get registered.
                 -- If you want to hook lspsaga or other signature handler, pls set to false
    doc_lines = 2, -- will show two lines of comment/doc(if there are more than two lines in doc, will be truncated);
                   -- set to 0 if you DO NOT want any API comments be shown
                   -- This setting only take effect in insert mode, it does not affect signature help in normal
                   -- mode, 10 by default

    floating_window = true, -- show hint in a floating window, set to false for virtual text only mode
    fix_pos = false,  -- set to true, the floating window will not auto-close until finish all parameters
    hint_enable = false, -- virtual hint enable
    hint_prefix = "🐼 ",  -- Panda for parameter
    hint_scheme = "String",
    use_lspsaga = false,  -- set to true if you want to use lspsaga popup
    hi_parameter = "Search", -- how your parameter will be highlight
    max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down
                     -- to view the hiding contents
    max_width = 120, -- max_width of signature floating_window, line will be wrapped if exceed max_width
    handler_opts = {
      border = "single"   -- double, single, shadow, none
    },
    extra_trigger_chars = {} -- Array of extra characters that will trigger signature completion, e.g., {"(", ","}
  }
  require "lsp_signature".on_attach(cfg)
end

function setup_lsp(server, options)
	local options = table_merge(options, {
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
	})
	require'lspconfig'[server].setup(options)
end

-- lsp event handlers
local telescope_builtin = require'telescope.builtin'
vim.lsp.handlers['textDocument/codeAction'] = telescope_builtin.lsp_code_actions
vim.lsp.handlers['textDocument/references'] = telescope_builtin.lsp_references
vim.lsp.handlers['textDocument/definition'] = telescope_builtin.lsp_definitions
vim.lsp.handlers['textDocument/implementation'] = telescope_builtin.lsp_implementations
vim.lsp.handlers['textDocument/documentSymbol'] = telescope_builtin.lsp_document_symbols
vim.lsp.handlers['workspace/symbol'] = telescope_builtin.lsp_workspace_symbols

-- c++
setup_lsp('clangd', {})

-- elm
setup_lsp('elmls', {})

-- haskell
setup_lsp('hls', {})


-- keymap
local remap = {noremap = false, silent = true}
local noremap = {noremap = true, silent = true}
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
  return function (k, ...) return leader_shortcut(p .. k, ...) end
end
local command_key = mk_prefix('c')
local file_key =  mk_prefix('f')
local git_key =  mk_prefix('g')
local lsp_key =  mk_prefix('l')
local project_key =  mk_prefix('p')
local tab_key =  mk_prefix('b')
local window_key =  mk_prefix('w')

keycmd('<F1>', 'NvimTreeToggle')
nkeycmd('U', 'UndotreeToggle')

command_key('a', 'Telescope commands')

file_key('f', 'Telescope file_browser disable_devicons=false hidden=true')
file_key('o', 'Telescope find_files disable_devicons=false')

-- see gitsigns for more shortcuts
git_key('g', 'Neogit')

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

window_key('v', 'vsplit')
window_key('h', 'split')
window_key('d', 'hide')
window_key('i', 'wincmd k')
window_key('j', 'wincmd h')
window_key('k', 'wincmd j')
window_key('l', 'wincmd l')
window_key('p', 'wincmd k')
window_key('t', 'wincmd h')
window_key('n', 'wincmd j')
window_key('s', 'wincmd l')

leader_shortcut('h', 'HopChar1')
