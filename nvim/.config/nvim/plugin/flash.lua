vim.pack.add({'https://github.com/folke/flash.nvim'})

vim.keymap.set(
  {'n', 'x', 'o'},
  '<leader>a',
  function() require('flash').jump() end,
  { desc = 'Flash' }
)

