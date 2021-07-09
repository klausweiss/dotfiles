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
    vsnip = true;
    ultisnips = true;
    luasnip = true;
  };
}

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

-- neuron
require'neuron'.setup{
  neuron_dir = '~/doc/neuron',
}

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
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

end

function setup_lsp(server, options)
	local options = table_merge(options, {
		on_attach = on_attach,
		flags = {
			debounce_text_changes = 150,
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

-- elm
setup_lsp('elmls', {})

-- haskell
setup_lsp('hls', {})


-- keymap
local remap = {noremap = false, silent = true}
local noremap = {noremap = true, silent = true}
local function keycmd(k, cmd)
  vim.api.nvim_set_keymap('i', k, '<cmd>' .. cmd .. '<CR>', noremap)
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
local tab_key =  mk_prefix('t')
local window_key =  mk_prefix('w')

keycmd('<F1>', 'NvimTreeToggle')

command_key('a', 'Telescope commands')

file_key('f', 'Telescope file_browser disable_devicons=false hidden=true')
file_key('o', 'Telescope find_files disable_devicons=false')

git_key('s', 'Neogit')

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

leader_shortcut('h', 'HopChar1')
