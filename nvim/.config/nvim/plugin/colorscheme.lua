-- Use Omarchy's generated palette when it is available.  This stays portable:
-- on the development VPS (where Omarchy has no state directory), it falls back
-- to Catppuccin.
vim.opt.termguicolors = true

local state_home = vim.env.XDG_STATE_HOME or vim.fn.expand("~/.local/state")
local palette_file = state_home .. "/omarchy/current/theme/colors.toml"

local function omarchy_palette()
  if vim.fn.filereadable(palette_file) == 0 then
    return nil
  end

  local palette = {}
  for _, line in ipairs(vim.fn.readfile(palette_file)) do
    local key, value = line:match('^%s*([%w_]+)%s*=%s*"([^"]+)"%s*$')
    if key then
      palette[key] = value
    end
  end
  return next(palette) and palette or nil
end

local palette = omarchy_palette()
if palette then
  vim.pack.add({
    {
      src = "https://github.com/bjarneo/aether.nvim",
      name = "aether",
      version = "v3",
    },
  })

  -- Aether's bundled watcher expects LazyVim and follows Omarchy's
  -- `neovim.lua` spec. This configuration uses native vim.pack, so it must
  -- not try to load theme-specific LazyVim plugins on a switch.
  require("aether.hotreload").setup = function() end

  local function apply_omarchy_palette()
    local current_palette = omarchy_palette()
    if not current_palette then
      return
    end

    -- Aether is also Omarchy's generated fallback theme. Map Omarchy's
    -- `magenta` names to Aether's equivalent `purple` names.
    local colors = {}
    for _, name in ipairs({
      "background", "dark_background", "darker_background", "lighter_background",
      "foreground", "dark_foreground", "light_foreground", "bright_foreground",
      "muted", "red", "yellow", "orange", "green", "cyan", "blue", "brown",
      "bright_red", "bright_yellow", "bright_green", "bright_cyan", "bright_blue",
      "accent", "selection",
    }) do
      colors[name] = current_palette[name]
    end
    colors.bg = current_palette.background
    colors.dark_bg = current_palette.dark_background
    colors.darker_bg = current_palette.darker_background
    colors.lighter_bg = current_palette.lighter_background
    colors.fg = current_palette.foreground
    colors.dark_fg = current_palette.dark_foreground
    colors.light_fg = current_palette.light_foreground
    colors.bright_fg = current_palette.bright_foreground
    colors.purple = current_palette.magenta
    colors.bright_purple = current_palette.bright_magenta
    colors.cursor = current_palette.bright_foreground
    colors.selection_foreground = current_palette.selection_foreground or current_palette.foreground
    colors.selection_background = current_palette.selection_background or current_palette.selection

    vim.o.background = current_palette.mode == "light" and "light" or "dark"
    require("aether").setup({ colors = colors })
    vim.cmd.colorscheme("aether")
  end

  apply_omarchy_palette()
  vim.api.nvim_create_user_command("OmarchyThemeReload", apply_omarchy_palette, {
    desc = "Reload the active Omarchy palette",
  })

  -- Omarchy atomically replaces `current/theme` on a theme switch. Watch its
  -- parent directory and debounce the burst of filesystem events, then reload
  -- the palette directly rather than interpreting Omarchy's LazyVim spec.
  local uv = vim.uv or vim.loop
  local watch_dir = state_home .. "/omarchy/current"
  local previous_watch = _G.omarchy_palette_watch
  if previous_watch then
    if previous_watch.timer and not previous_watch.timer:is_closing() then
      previous_watch.timer:stop()
      previous_watch.timer:close()
    end
    if previous_watch.handle and not previous_watch.handle:is_closing() then
      previous_watch.handle:stop()
      previous_watch.handle:close()
    end
  end

  if uv and uv.new_fs_event and vim.fn.isdirectory(watch_dir) == 1 then
    local watch = { handle = uv.new_fs_event(), timer = nil }
    _G.omarchy_palette_watch = watch
    watch.handle:start(watch_dir, {}, vim.schedule_wrap(function()
      if watch.timer and not watch.timer:is_closing() then
        watch.timer:stop()
        watch.timer:close()
      end
      watch.timer = vim.defer_fn(function()
        watch.timer = nil
        apply_omarchy_palette()
      end, 250)
    end))
  end
  return
end

vim.pack.add({ { src = "https://github.com/catppuccin/nvim", name = "catppuccin" } })
vim.cmd.colorscheme("catppuccin")
