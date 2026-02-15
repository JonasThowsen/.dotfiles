return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require("harpoon")
    harpoon:setup()

    local function add_file()
      local list = harpoon:list()
      local before = list:length()
      list:add()

      if list:length() > before then
        vim.notify("Added file to Harpoon", vim.log.levels.INFO)
      else
        local current = vim.api.nvim_buf_get_name(0)
        if current == "" then
          vim.notify("Save the buffer before adding it to Harpoon", vim.log.levels.WARN)
        else
          vim.notify("File is already in Harpoon", vim.log.levels.INFO)
        end
      end
    end

    vim.keymap.set("n", "<C-t>", add_file, { desc = "Harpoon add file" })
    vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

    vim.keymap.set("n", "<C-h>", function() harpoon:list():select(1) end)
    vim.keymap.set("n", "<C-j>", function() harpoon:list():select(2) end)
    vim.keymap.set("n", "<C-k>", function() harpoon:list():select(3) end)
    vim.keymap.set("n", "<C-l>", function() harpoon:list():select(4) end)

    vim.keymap.set("n", "<C-p>", function() harpoon:list():prev() end)
    vim.keymap.set("n", "<C-n>", function() harpoon:list():next() end)
  end,
}
