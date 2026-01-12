return {
  dir = '/home/jonas/coding/fff.nvim',
  name = 'fff.nvim',
  build = "nix run .#release",
  opts = {
    debug = {
      enabled = false,
      show_scores = false,
    },
    layout = {
      height = 0.9,
      width = 0.9,
    }
  },
  lazy = false,
  keys = {
    {
      "<leader>ff",
      function() require('fff').find_files() end,
      desc = 'FFFind files',
    },
  }
}
