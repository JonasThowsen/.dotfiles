return {
  'nvim-tree/nvim-web-devicons',
  enabled = vim.g.have_nerd_font,
  config = function()
    local web_devicons = require('nvim-web-devicons')
    web_devicons.setup({
      override = {
        toml = {
          icon = '󰰤',
          color = '#FFFFFF',
          name = 'Toml',
        },
      },
    })
  end,
}
