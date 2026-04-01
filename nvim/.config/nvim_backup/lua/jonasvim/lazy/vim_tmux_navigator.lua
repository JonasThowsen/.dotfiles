return { -- Tmux Navigation
  'christoomey/vim-tmux-navigator',
  lazy = false,
  config = function()
    vim.keymap.set(
      'n',
      '<A-h>',
      '<cmd>TmuxNavigateLeft<CR>',
      { silent = true, desc = 'Navigate window left' }
    )
    vim.keymap.set(
      'n',
      '<A-j>',
      '<cmd>TmuxNavigateDown<CR>',
      { silent = true, desc = 'Navigate window down' }
    )
    vim.keymap.set(
      'n',
      '<A-k>',
      '<cmd>TmuxNavigateUp<CR>',
      { silent = true, desc = 'Navigate window up' }
    )
    vim.keymap.set(
      'n',
      '<A-l>',
      '<cmd>TmuxNavigateRight<CR>',
      { silent = true, desc = 'Navigate window right' }
    )
  end,
}
