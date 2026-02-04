vim.g.mapleader = ' '

-- Neovim builtin terminal keymaps
vim.keymap.set('n', '<leader>t', [[<CMD>below new<CR><CMD>terminal<CR>]], { desc = 'Open terminal in bottom split' })
vim.keymap.set('n', '<leader>T', [[<CMD>terminal<CR>]], { desc = 'Open terminal to take up full pane' })
vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], { desc = 'Back to normal mode in terminal mode' })

-- Keymaps for resizing av splits
vim.keymap.set('n', '<C-Left>', '<C-w><', { desc = 'Increase split size to the left' })
vim.keymap.set('n', '<C-Down>', '<C-w>+', { desc = 'Increase split size down' })
vim.keymap.set('n', '<C-Up>', '<C-w>-', { desc = 'Increase split size up' })
vim.keymap.set('n', '<C-Right>', '<C-w>>', { desc = 'Increase split size to the right' })

-- Keymap for å åpne oil
vim.keymap.set('n', '-', '<CMD>Oil<CR>', { desc = 'Open parent directory' })

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

-- ThePrimeagen sine keymaps
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv")

vim.keymap.set('n', 'J', 'mzJ`z')
-- vim.keymap.set('n', '<C-d>', '<C-d>zz')
-- vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')

-- greatest remap ever
vim.keymap.set('x', '<leader>p', [["_dP]])

-- next greatest remap ever : asbjornHaland
vim.keymap.set({ 'n', 'v' }, '<leader>y', [["+y]])
vim.keymap.set('n', '<leader>Y', [["+Y]])

vim.keymap.set('v', '<leader>v', '"+p', { desc = 'Paste from system clipboard' })
vim.keymap.set('n', '<leader>v', '"+p', { desc = 'Paste from system clipboard' })

vim.keymap.set({ 'n', 'v' }, '<leader>d', '"_d')

-- disable the Q key
vim.keymap.set('n', 'Q', '<nop>')

vim.keymap.set('n', '<leader>r', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
vim.keymap.set('v', '<leader>r', [[y:%s/\V<C-r>"/<C-r>"/gI<Left><Left><Left>]])

vim.keymap.set('n', '<leader>mr', '<cmd>CellularAutomaton make_it_rain<CR>')
