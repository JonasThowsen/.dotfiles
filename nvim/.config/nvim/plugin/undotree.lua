vim.pack.add({
  "https://github.com/mbbill/undotree",
})

vim.keymap.set("n", "<leader>u", function()
  vim.cmd.packadd("undotree")
  vim.cmd.UndotreeToggle()
end, {
  desc = "Toggle undo tree",
})
