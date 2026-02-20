return {
  'dmtrKovalenko/fff.nvim',
  build = function()
    require("fff.download").download_or_build_binary()
  end,
  opts = {
    prompt = 'λ ',
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
      "<leader>f",
      function() require('fff').find_files() end,
      desc = 'FFFind files',
    },
    {
      "<leader>s",
      function()
        require('fff').live_grep({
          grep = {
            modes = { 'fuzzy', 'plain', 'regex' }
          }
        })
      end,
      desc = 'Live fffuzy grep',
    }
  }
}
