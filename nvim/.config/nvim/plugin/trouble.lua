vim.pack.add({
  "https://github.com/folke/trouble.nvim",
})

local trouble_loaded = false
local function ensure_trouble()
  if trouble_loaded then
    return
  end
  trouble_loaded = true

  vim.cmd.packadd("trouble.nvim")
  require("trouble").setup({})
end

vim.keymap.set("n", "<leader>xx", function()
  ensure_trouble()
  vim.cmd("Trouble diagnostics toggle")
end, {
  desc = "Diagnostics (Trouble)",
})
