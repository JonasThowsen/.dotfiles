-- Keep only your personal keybinding overrides here. Add new bindings or
-- unbind defaults before replacing them.

-- See current bindings and descriptions:
--   omarchy menu keybindings --print

-- To disable every Omarchy default binding, set this in
-- ~/.config/hypr/hyprland.lua before require("default.hypr.omarchy"), then add
-- only the bindings you want below:
--   omarchy_default_bindings = false

-- To disable all preinstalled app/webapp bindings, set:
--   omarchy_preinstalled_bindings = false

-- Add a new binding.
-- o.bind("SUPER + SHIFT + R", "SSH", "alacritty -e ssh your-server")

-- Change an existing binding by unbinding it first, then binding the key again.
-- This example changes SUPER+SPACE from the launcher to the Omarchy root menu.
-- hl.unbind("SUPER + SPACE")
-- o.bind("SUPER + SPACE", "Omarchy menu", "omarchy-menu toggle root")
-- Keep the browser shortcut you previously tried to remove disabled.
hl.unbind("SUPER + SHIFT + RETURN")

-- Disable Omarchy's preinstalled web-app shortcuts only.
hl.unbind("SUPER + SHIFT + A")       -- ChatGPT
hl.unbind("SUPER + SHIFT + ALT + A") -- Grok
hl.unbind("SUPER + SHIFT + C")       -- Calendar
hl.unbind("SUPER + SHIFT + E")       -- Email
hl.unbind("SUPER + SHIFT + ALT + E") -- New email
hl.unbind("SUPER + SHIFT + Y")       -- YouTube
hl.unbind("SUPER + SHIFT + ALT + G") -- WhatsApp
hl.unbind("SUPER + SHIFT + CTRL + G") -- Google Messages
hl.unbind("SUPER + SHIFT + P")       -- Google Photos
hl.unbind("SUPER + SHIFT + S")       -- Google Maps
hl.unbind("SUPER + SHIFT + X")       -- X
hl.unbind("SUPER + SHIFT + ALT + X") -- X Post

-- Niri-style Vim navigation: focus with Super+H/J/K/L.
-- J, K, and L replace Toggle window split, Keybindings, and Toggle workspace layout.
hl.unbind("SUPER + J")
hl.unbind("SUPER + K")
hl.unbind("SUPER + L")
o.bind("SUPER + H", "Focus left window", hl.dsp.focus({ direction = "l" }))
o.bind("SUPER + J", "Focus below window", hl.dsp.focus({ direction = "d" }))
o.bind("SUPER + K", "Focus above window", hl.dsp.focus({ direction = "u" }))
o.bind("SUPER + L", "Focus right window", hl.dsp.focus({ direction = "r" }))
o.bind("CTRL + ALT + L", "Toggle workspace layout", "omarchy-hyprland-workspace-layout-toggle")

-- Use M (maximize) for Full width instead of the same-side Super+Alt+F.
hl.unbind("SUPER + ALT + F")
o.bind("CTRL + ALT + M", "Full width", hl.dsp.window.fullscreen({ mode = "maximized" }))

-- Move tiled windows with Super+Shift+H/J/K/L, matching your preferred Niri keys.
o.bind("SUPER + SHIFT + H", "Swap window left", hl.dsp.window.swap({ direction = "l" }))
o.bind("SUPER + SHIFT + J", "Swap window down", hl.dsp.window.swap({ direction = "d" }))
o.bind("SUPER + SHIFT + K", "Swap window up", hl.dsp.window.swap({ direction = "u" }))
o.bind("SUPER + SHIFT + L", "Swap window right", hl.dsp.window.swap({ direction = "r" }))

-- Match Niri's screenshot shortcuts.
-- Super+Ctrl+P replaces Omarchy's default OCR capture binding.
hl.unbind("SUPER + CTRL + P")
o.bind("SUPER + CTRL + P", "Region screenshot", "omarchy-capture-screenshot region")
o.bind("SUPER + SHIFT + P", "Fullscreen screenshot", "omarchy-capture-screenshot fullscreen")
o.bind("SUPER + ALT + P", "Window screenshot", "omarchy-capture-screenshot windows")

-- Match Niri's close-window shortcut (replaces Omarchy's Super+W).
hl.unbind("SUPER + W")
o.bind("SUPER + Q", "Close window", hl.dsp.window.close())

-- Match Niri's hotkey-overlay position for Omarchy's keybinding menu.
o.bind("ALT + K", "Keybindings", "omarchy-menu-keybindings")

-- Disable a default binding without replacing it.
-- hl.unbind("SUPER + SHIFT + B")

-- Logitech MX Keys examples:
-- o.bind("SUPER + SHIFT + S", nil, "omarchy-capture-screenshot")
-- o.bind("SUPER + H", nil, "voxtype record toggle")
-- o.bind("SUPER + PERIOD", nil, "omarchy-shell shell toggle omarchy.emojis")
