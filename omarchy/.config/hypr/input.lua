-- Keep only your personal input overrides here. Uncommented settings below
-- replace Omarchy's defaults.

-- Keyboard layout and options.
-- See https://wiki.hypr.land/Configuring/Basics/Variables/#input
hl.config({
input = {
  -- Norwegian layout. Do not bind Alt keys to layout switching: Right Alt
  -- (AltGr) is needed to enter Norwegian third-level symbols such as @.
  kb_layout = "no",
  kb_options = "caps:escape",

  -- Standard Norwegian keyboard layout.
  kb_variant = "",

  -- Change speed of keyboard repeat.
  repeat_rate = 70,
  repeat_delay = 250,

  -- Start with numlock on by default.
  numlock_by_default = true,

  -- Increase sensitivity for mouse/trackpad (default: 0).
  sensitivity = 0.35,

  -- Turn off mouse acceleration (default: adaptive).
  accel_profile = "flat",

  touchpad = {
    -- Use natural (inverse) scrolling.
    natural_scroll = true,

    -- Use two-finger clicks for right-click instead of lower-right corner.
    clickfinger_behavior = true,

    -- Control the speed of your scrolling.
    scroll_factor = 0.4,

    -- Enable the touchpad while typing.
    disable_while_typing = false,

    -- Left-click-and-drag with three fingers.
    drag_3fg = 1,
  },
},
  })

-- Keep the pointer visible while using Hyprland keyboard navigation.
-- Ghostty still hides it while you type via mouse-hide-while-typing.
hl.config({ cursor = { hide_on_key_press = false } })

-- App-specific touchpad scroll speeds.
-- o.window("(Alacritty|kitty|foot)", { scroll_touchpad = 1.5 })
-- o.window("com.mitchellh.ghostty", { scroll_touchpad = 0.2 })

-- Enable touchpad gestures for changing workspaces.
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Gestures/
-- hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

-- Enable touchpad gestures for moving focus (helpful on scrolling layout).
-- hl.gesture({ fingers = 3, direction = "left", action = function() hl.dispatch(hl.dsp.focus({ direction = "l" })) end })
-- hl.gesture({ fingers = 3, direction = "right", action = function() hl.dispatch(hl.dsp.focus({ direction = "r" })) end })
