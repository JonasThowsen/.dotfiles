return {
  'stevearc/oil.nvim',
  dependencies = {
    { 'nvim-tree/nvim-web-devicons' },
  },
  lazy = false,
  opts = {
    default_file_explorer = true,
    use_default_keymaps = true,
    columns = { 'icon' },
    -- Hide hidden files by default
    view_options = {
      show_hidden = false,
    },
    keymaps = {
      -- Remove the default 'H' binding first
      ['H'] = false,
      -- Add the new Shift+h binding
      ['<S-h>'] = function()
        require('oil').toggle_hidden()
      end,
    },
  },
  keys = {
  }
}
