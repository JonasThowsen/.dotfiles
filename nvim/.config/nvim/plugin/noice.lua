vim.pack.add({
  { src = "https://github.com/folke/noice.nvim", name = "noice.nvim" },
  { src = "https://github.com/MunifTanjim/nui.nvim", name = "nui.nvim" },
})

require("noice").setup({
  lsp = {
    override = {
      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
      ["vim.lsp.util.stylize_markdown"] = true,
    },
  },
  routes = {
    {
      filter = {
        event = "msg_show",
        any = {
          { find = "%d+L, %d+B" },
          { find = "; after #%d+" },
          { find = "; before #%d+" },
        },
      },
      view = "mini",
    },
  },
  presets = {
    bottom_search = true,
    -- Noice's default popup is centered; this preset moves it to the top.
    command_palette = false,
    long_message_to_split = true,
    lsp_doc_border = true,
  },
})

vim.keymap.set("c", "<S-Enter>", function()
  require("noice").redirect(vim.fn.getcmdline())
end, { desc = "Redirect Command Output" })
vim.keymap.set("n", "<leader>snl", function() require("noice").cmd("last") end, { desc = "Noice Last Message" })
vim.keymap.set("n", "<leader>snh", function() require("noice").cmd("history") end, { desc = "Noice History" })
vim.keymap.set("n", "<leader>sna", function() require("noice").cmd("all") end, { desc = "Noice All Messages" })
vim.keymap.set("n", "<leader>snd", function() require("noice").cmd("dismiss") end, { desc = "Dismiss Messages" })
