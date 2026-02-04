return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local fzf = require("fzf-lua")
    local config = fzf.config

    fzf.setup({
      'fzf-native',
      winopts = {
        fullscreen = true,
        treesitter = {
          enabled    = true,
          fzf_colors = { ["hl"] = "-1:reverse", ["hl+"] = "-1:reverse" }
        },
      },
      keymap = {
        builtin = {
          ["<C-d>"] = "preview-page-down",
          ["<C-u>"] = "preview-page-up",
        },
      },
    })

    -- Quickfix
    config.defaults.keymap.fzf["ctrl-q"] = "select-all+accept"

    -- vim.keymap.set("n", "<leader>ff", fzf.files, { desc = "Find files" })
    vim.keymap.set("n", "<leader>s", fzf.live_grep, { desc = "Live grep" })
  end,
}
