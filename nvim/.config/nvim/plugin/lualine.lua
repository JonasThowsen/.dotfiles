vim.pack.add({
  { src = "https://github.com/nvim-lualine/lualine.nvim", name = "lualine.nvim" },
})

require("lualine").setup({
  options = {
    theme = "auto",
    globalstatus = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff" },
    lualine_c = {
      { "filename", path = 1 },
      {
        "diagnostics",
        symbols = { error = " ", warn = " ", info = " ", hint = " " },
      },
    },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
})
