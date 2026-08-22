vim.pack.add({
  { src = "https://github.com/folke/snacks.nvim", name = "snacks.nvim" },
})

require("snacks").setup({
  notifier = { enabled = true },
})

vim.keymap.set("n", "<leader>n", function()
  require("snacks").notifier.show_history()
end, { desc = "Notification History" })
vim.keymap.set("n", "<leader>un", function()
  require("snacks").notifier.hide()
end, { desc = "Dismiss Notifications" })
