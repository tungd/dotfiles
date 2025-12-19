local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
config.font = wezterm.font('Iosevka Term SS07', {weight="Medium"})
config.font_size = 16

-- For example, changing the color scheme:
config.color_scheme = 'VibrantInk'

config.window_background_opacity = 1.0

config.audible_bell = 'Disabled'

-- Function to send macOS notification
local function send_notification(title, body)
  os.execute(string.format('osascript -e \'display notification "%s" with title "%s"\'', body, title))
end

-- Handle command completion notifications
wezterm.on('user-var-changed', function(window, pane, name, value)
  if name == 'WEZTERM_COMMAND_STATUS' then
    -- Only send notification if WezTerm window is not focused
    if not window:is_focused() then
      local command = value:match('^finished:(.+)')
      if command then
        os.execute(string.format('osascript -e \'display notification "Command completed: %s" with title "WezTerm"\'', command:gsub("'", "\\'")))
      end
    end
  end
end)

-- and finally, return the configuration to wezterm
return config
