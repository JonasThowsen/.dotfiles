vim.pack.add({ 'https://github.com/stevearc/oil.nvim' })

require('oil').setup({
  use_default_keymaps = true,
  view_options = {
    show_hidden = false,
  },
  keymaps = {
    ['H'] = { 'actions.toggle_hidden', mode = 'n' },
  },
})

vim.keymap.set('n', '-', '<CMD>Oil<CR>', { desc = 'Open parent directory' })
