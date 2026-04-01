-- Leader
vim.g.mapleader = " "

-- Set
vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true

-- set this because i want quickfix substitute to be case insensitive
-- also works better when i send things from fff to quickfix
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.undodir = os.getenv('HOME') .. '/.vim/undodir'
vim.opt.undofile = true

vim.opt.scrolloff = 8

-- Keymaps
vim.keymap.set('x', '<leader>p', [["_dP]])

vim.keymap.set({ 'n', 'v' }, '<leader>y', [["+y]])
vim.keymap.set('n', '<leader>Y', [["+Y]])

vim.keymap.set('v', '<leader>v', '"+p', { desc = 'Paste from system clipboard' })
vim.keymap.set('n', '<leader>v', '"+p', { desc = 'Paste from system clipboard' })

vim.keymap.set({ 'n', 'v' }, '<leader>d', '"_d')

vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv")

vim.keymap.set('n', 'J', 'mzJ`z')

-- Neovim builtin terminal keymaps
vim.keymap.set('n', '<leader>T', [[<CMD>below new<CR><CMD>terminal<CR>]], { desc = 'Open terminal in bottom split' })
vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], { desc = 'Back to normal mode in terminal mode' })

-- Keymaps for resizing av splits
vim.keymap.set('n', '<C-Left>', '<C-w><', { desc = 'Increase split size to the left' })
vim.keymap.set('n', '<C-Down>', '<C-w>+', { desc = 'Increase split size down' })
vim.keymap.set('n', '<C-Up>', '<C-w>-', { desc = 'Increase split size up' })
vim.keymap.set('n', '<C-Right>', '<C-w>>', { desc = 'Increase split size to the right' })

-- LSP greier
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { desc = 'LSP go to definition' })

vim.keymap.set("n", "K", function()
  vim.lsp.buf.hover {
    border = "rounded",
  }
end, { desc = "Hover documentation" })

-- Manual keybind to activate LSP completion
vim.keymap.set('i', '<c-space>', function()
  vim.lsp.completion.get()
end)
