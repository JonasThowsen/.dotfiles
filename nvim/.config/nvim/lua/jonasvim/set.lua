vim.g.tmux_navigator_no_mappings = 1

vim.opt.guicursor = ''

vim.opt.autoread = true

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

vim.opt.smartindent = true

-- set this because i want quickfix substitute to be case insensitive
-- also works better when i send things from telescope to quickfix
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- vim.opt.wrap = false

vim.opt.conceallevel = 1
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv('HOME') .. '/.vim/undodir'
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = 'yes'
vim.opt.isfname:append('@-@')

vim.opt.updatetime = 10

-- vim.opt.colorcolumn = '80'

-- Minimal statusline - just filename
vim.opt.statusline = ' %t %m%='

-- File-specific indentation settings
vim.api.nvim_create_autocmd('FileType', {
  -- 4 spaces
  pattern = { 'python', 'rust', 'java', 'c', 'cpp', 'cs' },
  callback = function()
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.shiftwidth = 4
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  -- 2 spaces
  pattern = {
    'javascript',
    'typescript',
    'javascriptreact',
    'typescriptreact',
    'json',
    'yaml',
    'html',
    'css',
    'scss',
    'ruby',
    'lua',
  },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
  end,
})

vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Highlight for system clipboard yank
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking to system clipboard',
  group = vim.api.nvim_create_augroup('highlight-system-yank', { clear = true }),
  callback = function(event)
    -- Check if the target register is the system clipboard ('+' register)
    if event.regname == '+' then
      vim.highlight.on_yank()
    end
  end,
})
