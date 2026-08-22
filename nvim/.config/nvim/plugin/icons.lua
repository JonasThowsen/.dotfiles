vim.pack.add({
  { src = "https://github.com/nvim-mini/mini.icons", name = "mini.icons" },
})

require("mini.icons").setup()

-- Let plugins that use nvim-web-devicons use MiniIcons instead, without
-- adding a second icon provider.
package.preload["nvim-web-devicons"] = function()
  require("mini.icons").mock_nvim_web_devicons()
  return package.loaded["nvim-web-devicons"]
end
