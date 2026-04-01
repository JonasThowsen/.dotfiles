local M = {}

function M.lightmode()
  vim.o.background = "light"
  vim.cmd.colorscheme("modus_operandi")
end

function M.darkmode()
  vim.o.background = "dark"
  vim.cmd.colorscheme("modus_vivendi")
end

vim.api.nvim_create_user_command("LightMode", function()
  M.lightmode()
end, {})

vim.api.nvim_create_user_command("DarkMode", function()
  M.darkmode()
end, {})

return M
