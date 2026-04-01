vim.pack.add({
  "https://github.com/folke/which-key.nvim",
})

local wk_loaded = false
local function ensure_which_key()
  if wk_loaded then
    return
  end
  wk_loaded = true

  vim.cmd.packadd("which-key.nvim")
  require("which-key").setup({
    delay = 700,
  })
end

vim.keymap.set("n", "<leader>?", function()
  ensure_which_key()
  require("which-key").show({ global = false })
end, {
  desc = "Buffer Local Keymaps (which-key)",
})
